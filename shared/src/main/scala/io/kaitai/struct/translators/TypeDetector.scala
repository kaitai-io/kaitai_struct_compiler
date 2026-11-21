package io.kaitai.struct.translators

import io.kaitai.struct.Utils
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.{Ast, ConstEvaluator}
import io.kaitai.struct.format.Identifier
import io.kaitai.struct.precompile.{MethodNotFoundError, TypeMismatchError, TypeUndecidedError}

/**
  * Basic class the implements type inferring functionality for Ast.expr
  * expressions. The main method is [[detectType]] that allows to derive
  * type of any given expression.
  * @param provider TypeProvider that will answer queries on user types
  */
class TypeDetector(provider: TypeProvider) {
  import TypeDetector._

  /**
    * Detects type of a given expression. If it returns a SwitchType, it
    * effectively flattens it to a resulting combined type.
    * @param v expression
    * @return data type
    */
  def detectType(v: Ast.expr): DataType = {
    detectTypeRaw(v).asCombined
  }

  /**
    * Detects type of a given expression, raw, without any switch type
    * flattening.
    * @param v expression
    * @return data type
    */
  def detectTypeRaw(v: Ast.expr): DataType = {
    v match {
      case Ast.expr.IntNum(x) =>
        if (x >= 0 && x <= 127) {
          // [0..127] => signed 1-byte integer
          Int1Type(true)
        } else if (x >= 128 && x <= 255) {
          // [128..255] => unsigned 1-byte integer
          Int1Type(false)
        } else if (x >= Int.MinValue && x <= Int.MaxValue) {
          IntMultiType(true, Width4, None)
        } else if (x >= 0 && x <= Utils.MAX_UINT32) {
          IntMultiType(false, Width4, None)
        } else if (x >= Long.MinValue && x <= Long.MaxValue) {
          IntMultiType(true, Width8, None)
        } else if (x >= 0 && x <= Utils.MAX_UINT64) {
          IntMultiType(false, Width8, None)
        } else {
          throw new TypeMismatchError(s"integer literal $x is out of the 64-bit integer range")
        }
      case Ast.expr.UnaryOp(Ast.unaryop.Minus, Ast.expr.IntNum(x)) =>
        // Negative integer literals are parsed as the unary minus applied to a
        // non-negative literal, but they must be typed based on their actual
        // value (otherwise `-2147483648` would be rejected as `-` applied to
        // `u4`, which is not allowed)
        detectTypeRaw(Ast.expr.IntNum(-x))
      case Ast.expr.FloatNum(_) => CalcFloatType
      case Ast.expr.Str(_) => CalcStrType
      case Ast.expr.InterpolatedStr(_) => CalcStrType
      case Ast.expr.Bool(_) => CalcBooleanType
      case Ast.expr.EnumByLabel(enumType, _, inType) =>
        val t = EnumType(enumType.name, inType.names.toList, CalcIntType)
        t.enumSpec = Some(provider.resolveEnum(inType, enumType.name))
        t
      case Ast.expr.EnumById(enumType, _, inType) =>
        val t = EnumType(enumType.name, List(), CalcIntType)
        t.enumSpec = Some(provider.resolveEnum(inType, enumType.name))
        t
      case Ast.expr.Name(name: Ast.identifier) => provider.determineType(name.name).asNonOwning()
      case Ast.expr.InternalName(id) => provider.determineType(id)
      case Ast.expr.UnaryOp(op: Ast.unaryop, v: Ast.expr) =>
        val t = detectType(v)
        (t, op) match {
          case (it: IntType, Ast.unaryop.Minus) =>
            if (it == CalcIntType) {
              CalcIntType
            } else if (it.bitWidth < 32 || (it.bitWidth == 32 && it.signed)) {
              IntMultiType(true, Width4, None)
            } else if (it.bitWidth == 64 && it.signed) {
              IntMultiType(true, Width8, None)
            } else {
              val hint = if (it.bitWidth < 64) " (if you want the result as `s8`, use `-val.as<s8>`)" else ""
              throw new TypeMismatchError(s"unable to apply unary operator $op to $t$hint")
            }
          case (it: IntType, Ast.unaryop.Invert) =>
            if (it == CalcIntType) CalcIntType else promoteIntType(it)
          case (_: FloatType, Ast.unaryop.Minus) => t
          case (_: BooleanType, Ast.unaryop.Not) => t
          case _ => throw new TypeMismatchError(s"unable to apply unary operator $op to $t")
        }
      case Ast.expr.Compare(left: Ast.expr, op: Ast.cmpop, right: Ast.expr) =>
        val ltype = detectType(left)
        val rtype = detectType(right)
        assertCompareTypes(ltype, rtype, op)
        CalcBooleanType
      case Ast.expr.BinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) =>
        (detectType(left), detectType(right), op) match {
          case (CalcIntType, _: IntType, Ast.operator.LShift | Ast.operator.RShift) =>
            // `CalcIntType` has no specific bit width, so the result type stays
            // `CalcIntType`, and the shift distance can't be checked
            CalcIntType
          case (lt: IntType, _: IntType, Ast.operator.LShift | Ast.operator.RShift) =>
            shiftIntResultType(lt, right, op)
          case (CalcIntType, rt: IntType, _) =>
            binaryCalcIntResultType(rt)
          case (lt: IntType, CalcIntType, _) =>
            binaryCalcIntResultType(lt)
          case (lt: IntType, rt: IntType, _) =>
            binaryIntResultType(left, lt, right, rt, op)
          case (_: NumericType, _: NumericType, _) => CalcFloatType
          case (_: StrType, _: StrType, Ast.operator.Add) => CalcStrType
          case (ltype, rtype, _) =>
            throw new TypeMismatchError(s"can't apply operator $op to $ltype and $rtype")
        }
      case Ast.expr.BoolOp(op: Ast.boolop, values: Seq[Ast.expr]) =>
        values.foreach(v => {
          val t = detectType(v)
          t match {
            case _: BooleanType => // we're fine
            case _ =>
              throw new TypeMismatchError(s"unable to use $t argument in $op boolean expression")
          }
        })
        CalcBooleanType
      case Ast.expr.IfExp(condition: Ast.expr, ifTrue: Ast.expr, ifFalse: Ast.expr) =>
        detectType(condition) match {
          case _: BooleanType =>
            (detectType(ifTrue), detectType(ifFalse)) match {
              case (tt: IntType, ft: IntType) =>
                ternaryIntResultType(ifTrue, tt, ifFalse, ft)
              case (tt, ft) =>
                combineTypesAndFail(tt, ft)
            }
          case other => throw new TypeMismatchError(s"unable to switch over $other")
        }
      case Ast.expr.Subscript(container: Ast.expr, idx: Ast.expr) =>
        detectType(container) match {
          case ArrayTypeInStream(elType: DataType) =>
            detectType(idx) match {
              case _: IntType => elType.asNonOwning(
                elType match {
                  case ct: ComplexDataType => ct.isOwning
                  case _ => false
                }
              )
              case idxType => throw new TypeMismatchError(s"unable to index an array using $idxType")
            }
          case CalcArrayType(elType: DataType, _) =>
            detectType(idx) match {
              case _: IntType => elType.asNonOwning(
                elType match {
                  case ct: ComplexDataType => ct.isOwning
                  case _ => false
                }
              )
              case idxType => throw new TypeMismatchError(s"unable to index an array using $idxType")
            }
          case _: BytesType => Int1Type(false)
          case cntType => throw new TypeMismatchError(s"unable to apply operation [] to $cntType")
        }
      case Ast.expr.Attribute(value: Ast.expr, attr: Ast.identifier) =>
        detectAttributeType(value, attr)
      case call: Ast.expr.Call =>
        detectCallType(call).asNonOwning() // we have no methods that can return owning types yet, but it's probably safer to treat them as user type attributes
      case Ast.expr.List(values: Seq[Ast.expr]) =>
        detectArrayType(values) match {
          case Int1Type(_) => CalcBytesType
          case t => ArrayTypeInStream(t)
        }
      case Ast.expr.CastToType(_, typeName) =>
        detectCastType(typeName)
      case Ast.expr.ByteSizeOfType(_) | Ast.expr.BitSizeOfType(_) =>
        CalcIntType
    }
  }

  /**
    * Detects resulting data type of a given attribute expression.
    *
    * @note Must be kept in sync with [[CommonMethods.translateAttribute]]
    * @param value value part of attribute expression
    * @param attr attribute identifier part of attribute expression
    * @return data type
    */
  def detectAttributeType(value: Ast.expr, attr: Ast.identifier): DataType = {
    val valType = detectType(value)

    // Special case: will be compiled as compile-time determined constant
    if (attr.name == Identifier.SIZEOF)
      return CalcIntType

    valType match {
      case KaitaiStructType | CalcKaitaiStructType(_) =>
        attr.name match {
          case Identifier.PARENT => CalcKaitaiStructType()
          case Identifier.IO => KaitaiStreamType
          case _ => throw new MethodNotFoundError(attr.name, valType)
        }
      case t: UserType =>
        t.classSpec match {
          case Some(tt) => provider.determineType(tt, attr.name).asNonOwning()
          case None => throw new TypeUndecidedError(s"expression '$value' has undecided type '${t.name}' (while asking for attribute '${attr.name}')")
        }
      case _: BytesType =>
        attr.name match {
          case "length" | "size" => CalcIntType
          case "first" | "last" | "min" | "max" => Int1Type(false)
          case _ => throw new MethodNotFoundError(attr.name, valType)
        }
      case _: StrType =>
        attr.name match {
          case "length" => CalcIntType
          case "reverse" => CalcStrType
          case "to_i" => CalcIntType
          case _ => throw new MethodNotFoundError(attr.name, valType)
        }
      case _: IntType =>
        attr.name match {
          case "to_s" => CalcStrType
          case _ => throw new MethodNotFoundError(attr.name, valType)
        }
      case _: FloatType =>
        attr.name match {
          case "to_i" => CalcIntType
          case _ => throw new MethodNotFoundError(attr.name, valType)
        }
      case ArrayTypeInStream(_) | CalcArrayType(_, _) =>
        val inType = valType match {
          case ArrayTypeInStream(inType) => inType.asNonOwning(
            inType match {
              case ct: ComplexDataType => ct.isOwning
              case _ => false
            }
          )
          case CalcArrayType(inType, _) => inType.asNonOwning(
            inType match {
              case ct: ComplexDataType => ct.isOwning
              case _ => false
            }
          )
          case _ => throw new TypeMismatchError(s"Unexpected type for arrays ${valType}.");
        }

        attr.name match {
          case "first" | "last" | "min" | "max" => inType
          case "size" => CalcIntType
          case _ => throw new MethodNotFoundError(attr.name, valType)
        }
      case KaitaiStreamType | OwnedKaitaiStreamType =>
        attr.name match {
          case "size" => CalcIntType
          case "pos" => CalcIntType
          case "eof" => CalcBooleanType
          case _ => throw new MethodNotFoundError(attr.name, valType)
        }
      case et: EnumType =>
        attr.name match {
          case "to_i" => et.enumSpec.get.intType
          case _ => throw new MethodNotFoundError(attr.name, valType)
        }
      case _: BooleanType =>
        attr.name match {
          case "to_i" => CalcIntType
          case _ => throw new MethodNotFoundError(attr.name, valType)
        }
      case _ =>
        throw new MethodNotFoundError(attr.name, valType)
    }
  }

  /**
    * Detects resulting data type of a given function call expression. Typical function
    * call expression in KSY is `foo.bar(arg1, arg2)`, which is represented in AST as
    * `Call(Attribute(foo, bar), Seq(arg1, arg2))`.
    * @note Must be kept in sync with [[CommonMethods.translateCall]]
    * @param call function call expression
    * @return data type
    */
  def detectCallType(call: Ast.expr.Call): DataType = {
    call.func match {
      case Ast.expr.Attribute(obj: Ast.expr, methodName: Ast.identifier) =>
        val objType = detectType(obj)
        // TODO: check number and type of arguments in `call.args`
        (objType, methodName.name) match {
          case (_: StrType, "substring") => CalcStrType
          case (_: StrType, "to_i") => CalcIntType
          case (_: StrType, "to_b") => CalcBytesType
          case (_: BytesType, "to_s") => CalcStrType
          case (_: BytesType, "index_of") => CalcIntType
          case _ =>
            throw new MethodNotFoundError(methodName.name, objType)
        }
    }
  }

  /**
    * Checks that elements in the array literal are all the of same type and
    * returns that type. Throws exception if multiple mismatching types are
    * encountered.
    *
    * @param values
    * @return
    */
  def detectArrayType(values: Seq[Ast.expr]): DataType = {
    var t1o: Option[DataType] = None

    values.foreach { v =>
      val t2 = detectType(v)
      t1o = t1o match {
        case None => Some(t2)
        case Some(t1) => Some(combineTypesAndFail(t1, t2))
      }
    }

    t1o match {
      case None => throw new RuntimeException("empty array literals are not allowed - can't detect array type")
      case Some(t) => t
    }
  }

  /**
    * Detects cast type determined by a typeId definition.
    * @param typeName typeId definition to use
    * @return data type
    */
  def detectCastType(typeName: Ast.typeId): DataType = {
    val singleType = if ((!typeName.absolute) && typeName.names.size == 1) {
      // May be it's a reserved pure data type name?
      DataType.pureFromString(Some(typeName.names(0))) match {
        case _: UserType =>
          // No, it's a user type, let's try to resolve it through provider
          provider.resolveType(typeName)
        case primitiveType =>
          // Yes, it is!
          primitiveType
      }
    } else {
      // It's a complex type name, it can be only resolved through provider
      provider.resolveType(typeName)
    }

    // Wrap it in array type, if needed
    if (typeName.isArray) {
      ArrayTypeInStream(singleType)
    } else {
      singleType
    }
  }
}

object TypeDetector {
  /**
    * Promotes an integer type to one of `s4`, `u4`, `s8` and `u8`. Types
    * narrower than 32 bits (both signed and unsigned) are promoted to `s4`.
    * @param t integer type (other than [[CalcIntType]])
    * @return promoted type
    */
  def promoteIntType(t: IntType): IntMultiType = t match {
    case _ if t.bitWidth < 32 => IntMultiType(true, Width4, None)
    case _ if t.bitWidth == 32 => IntMultiType(t.signed, Width4, None)
    case _ => IntMultiType(t.signed, Width8, None)
  }

  /**
    * Returns the value of an integer literal (possibly negative).
    * @param v expression
    * @return value of the literal, or None if `v` isn't an integer literal
    */
  def intLiteralValue(v: Ast.expr): Option[BigInt] = v match {
    case Ast.expr.IntNum(x) => Some(x)
    case Ast.expr.UnaryOp(Ast.unaryop.Minus, Ast.expr.IntNum(x)) => Some(-x)
    case _ => None
  }

  /**
    * Derives the resulting type of an arithmetic (`+`, `-`, `*`, `/`, `%`) or
    * bitwise (`&`, `|`, `^`) binary operator applied to integer operands.
    *
    * If all possible values of one operand fit into the promoted type of the
    * other operand, that type is the result type. For example,
    * `s8_val + u4_val` is `s8`, `u8_val + 1` is `u8`, and `u1_val + s1_val`
    * is `s4` (`u1` doesn't fit into `s1`, but it fits into its promoted type
    * `s4`). If neither operand fits into the promoted type of the other one
    * (e.g. in `s4_val + u4_val`), the operator can't be applied.
    * @param left left-hand operand
    * @param ltype type of the left-hand operand (other than [[CalcIntType]])
    * @param right right-hand operand
    * @param rtype type of the right-hand operand (other than [[CalcIntType]])
    * @param op binary operator
    * @return resulting type
    */
  def binaryIntResultType(left: Ast.expr, ltype: IntType, right: Ast.expr, rtype: IntType, op: Ast.operator): IntMultiType =
    promotedIntResultType(left, ltype, right, rtype).getOrElse(
      throw new TypeMismatchError(
        s"unable to apply binary operator $op to ${ltype.toPureTypeString} and ${rtype.toPureTypeString} " +
        "(signed integers can't be mixed with unsigned integers of the same or greater width, use a type cast `.as<>` on one of the operands)"
      )
    )

  /**
    * Returns the promoted type of one operand if all possible values of the
    * other operand fit into it (see [[binaryIntResultType]]).
    * @param left left-hand operand
    * @param ltype type of the left-hand operand (other than [[CalcIntType]])
    * @param right right-hand operand
    * @param rtype type of the right-hand operand (other than [[CalcIntType]])
    * @return resulting type, or None if neither operand fits into the
    *         promoted type of the other one
    */
  def promotedIntResultType(left: Ast.expr, ltype: IntType, right: Ast.expr, rtype: IntType): Option[IntMultiType] = {
    val lt = promoteIntType(ltype)
    val rt = promoteIntType(rtype)
    if (intOperandFitsInto(left, ltype, rt)) {
      Some(rt)
    } else if (intOperandFitsInto(right, rtype, lt)) {
      Some(lt)
    } else {
      None
    }
  }

  /**
    * Derives the resulting type of the ternary operator (`?:`) with integer
    * branches. The result type is the same as for arithmetic binary
    * operations (see [[binaryIntResultType]]), except that if both branches
    * are at most 16 bits wide and have the same signedness, the wider of the
    * two types is the result type (e.g. `c ? u1_val : u2_val` is `u2`).
    * Here, bit-sized integers up to `b8` count as `u1`, and `b9`..`b16` as
    * `u2` (e.g. `c ? b3_val : b5_val` is `u1`). An integer literal branch
    * takes the type of the other branch if its value fits into it (e.g.
    * `c ? u1_val : 5` is `u1`). A [[CalcIntType]] branch is handled as in
    * [[binaryCalcIntResultType]].
    * @param ifTrue branch for a true condition
    * @param ttype type of `ifTrue`
    * @param ifFalse branch for a false condition
    * @param ftype type of `ifFalse`
    * @return resulting type
    */
  def ternaryIntResultType(ifTrue: Ast.expr, ttype: IntType, ifFalse: Ast.expr, ftype: IntType): IntType = {
    (ttype, ftype) match {
      case (CalcIntType, _) => binaryCalcIntResultType(ftype)
      case (_, CalcIntType) => binaryCalcIntResultType(ttype)
      case _ =>
        val tt = ternaryBranchIntType(ttype)
        val ft = ternaryBranchIntType(ftype)
        val trueLiteralFits = intLiteralValue(ifTrue).exists(x => x >= ft.min && x <= ft.max)
        val falseLiteralFits = intLiteralValue(ifFalse).exists(x => x >= tt.min && x <= tt.max)
        if (tt.bitWidth <= 16 && ft.bitWidth <= 16 && tt.signed == ft.signed) {
          if (tt.bitWidth >= ft.bitWidth) tt else ft
        } else if (trueLiteralFits && falseLiteralFits) {
          // Both branches are literals that fit into the type of the other one
          // (e.g. `-1` is `s4` and fits into `s1`, and `5` is `s1` and fits into
          // `s4`). The narrower type is the result type, so that the result type
          // doesn't depend on the branch order.
          if (tt.bitWidth <= ft.bitWidth) tt else ft
        } else if (trueLiteralFits) {
          ft
        } else if (falseLiteralFits) {
          tt
        } else {
          // the original types are passed here, because all values of
          // `b17`..`b31` fit into `u4`, but not all values of their normalized
          // type `s4` do
          promotedIntResultType(ifTrue, ttype, ifFalse, ftype).getOrElse(
            throw new TypeMismatchError(
              s"unable to combine ${ttype.toPureTypeString} and ${ftype.toPureTypeString} in the ternary operator " +
              "(signed integers can't be mixed with unsigned integers of the same or greater width, use a type cast `.as<>` on one of the branches)"
            )
          )
        }
    }
  }

  /**
    * Normalizes the type of an integer branch of the ternary operator:
    * types at most 16 bits wide become `s1`, `s2`, `u1` or `u2` (e.g. `b3`
    * becomes `u1`). Wider types are promoted like operands of binary
    * operators.
    * @param t integer type (other than [[CalcIntType]])
    * @return normalized type
    */
  def ternaryBranchIntType(t: IntType): IntType = t match {
    case _ if t.bitWidth <= 8 => Int1Type(t.signed)
    case _ if t.bitWidth <= 16 => IntMultiType(t.signed, Width2, None)
    case _ => promoteIntType(t)
  }

  /**
    * Derives the resulting type of an arithmetic or bitwise binary operator
    * with a [[CalcIntType]] operand. Built-in integer properties and variables
    * such as `_io.size` or `_index` have this type, because they don't have a
    * specific integer type yet. If the promoted type of the other operand is
    * 64-bit, the result has that type, e.g. `u8_val + _io.pos` is `u8`.
    * Otherwise, the result is [[CalcIntType]].
    * @param other type of the other operand
    * @return resulting type
    */
  def binaryCalcIntResultType(other: IntType): IntType =
    // `CalcIntType` is checked first, because its `bitWidth` isn't defined
    if (other != CalcIntType && other.bitWidth > 32) promoteIntType(other) else CalcIntType

  /**
    * Checks if all possible values of an integer operand fit into a given
    * type. For an integer literal, that's just its value. For a type narrower
    * than 32 bits, it's the range of that type, e.g. `u2` fits into `s4`. For
    * wider types, it's the range of the promoted type, which makes
    * `b33`..`b64` behave like `u8`.
    * @param v operand
    * @param t type of the operand (other than [[CalcIntType]])
    * @param dst type that the values should fit into
    * @return true if all possible values of the operand fit into `dst`
    */
  def intOperandFitsInto(v: Ast.expr, t: IntType, dst: IntType): Boolean = {
    intLiteralValue(v) match {
      case Some(x) =>
        x >= dst.min && x <= dst.max
      case None =>
        val rangeType = if (t.bitWidth < 32) t else promoteIntType(t)
        rangeType.subsetOf(dst)
    }
  }

  /**
    * Derives the resulting type of a bitwise shift operator (`<<`, `>>`)
    * applied to integer operands. It depends only on the left-hand operand.
    * If the shift distance is a constant, it must be non-negative and less
    * than the bit width of the resulting type.
    * @param ltype type of the left-hand operand (other than [[CalcIntType]])
    * @param right right-hand operand (shift distance)
    * @param op shift operator
    * @return resulting type
    */
  def shiftIntResultType(ltype: IntType, right: Ast.expr, op: Ast.operator): IntMultiType = {
    val t = promoteIntType(ltype)
    ConstEvaluator.evaluateIntConst(right) match {
      case Some(dist) if dist < 0 || dist >= t.bitWidth =>
        val opStr = if (op == Ast.operator.LShift) "<<" else ">>"
        // the hint only helps if the distance is valid for a 64-bit type
        val hint = if (dist >= t.bitWidth && dist < 64) {
          val castType = if (ltype.signed) "s8" else "u8"
          s" (to get a 64-bit result, cast the left-hand operand first, e.g. `.as<$castType>`)"
        } else {
          ""
        }
        throw new TypeMismatchError(
          s"shift distance $dist is out of range for `$opStr` on ${ltype.toPureTypeString}: " +
          s"the result type is ${t.toPureTypeString}, so the distance must be in 0..${t.bitWidth - 1}$hint"
        )
      case _ => t
    }
  }

  /**
    * Checks is values of two types can be compared with a given comparison operator.
    * Throws exception in case of incompatibility.
    * @param ltype first type
    * @param rtype second type
    * @param op comparison operator
    */
  def assertCompareTypes(ltype: DataType, rtype: DataType, op: Ast.cmpop): Unit = {
    (ltype, rtype) match {
      case (_: StrType, _: StrType) => // ok
      case (_: NumericType, _: NumericType) => // ok
      case (_: BooleanType, _: BooleanType) => // ok
      case (_: BytesType, _: BytesType) => // ok
      case (et1: EnumType, et2: EnumType) =>
        val et1Spec = et1.enumSpec.get
        val et2Spec = et2.enumSpec.get
        if (et1Spec != et2Spec) {
          throw new TypeMismatchError(s"can't compare different enums '${et1Spec.nameAsStr}' and '${et2Spec.nameAsStr}'")
        }
        op match {
          case Ast.cmpop.Eq | Ast.cmpop.NotEq => // ok
          case _ =>
            throw new TypeMismatchError(s"can't use comparison operator $op on enums")
        }
      case _ =>
        throw new TypeMismatchError(s"can't compare $ltype and $rtype")
    }
  }

  /**
    * Checks if the values of two types can be combined (i.e. there exists a single type that can
    * be used to hold values of both values - usually it means that they're either equal or one
    * is a subset of another).
    *
    * @param t1 first type
    * @param t2 second type
    * @return type that can accommodate values of both source types without any data loss
    */
  def combineTypes(t1: DataType, t2: DataType): DataType = {
    if (t1 == t2) {
      // Obviously, if types are equal, they'll fit into one another
      t1
    } else {
      (t1, t2) match {
        // for 1-byte integers, "unsigned" wins (it is always wider)
        case (Int1Type(false), Int1Type(true)) => Int1Type(false)
        case (Int1Type(true), Int1Type(false)) => Int1Type(false)
        case (Int1Type(_), _: IntMultiType) => t2
        case (_: IntMultiType, Int1Type(_)) => t1
        case (i1: IntMultiType, i2: IntMultiType) =>
          if (i1.endian == i2.endian && i1.signed == i2.signed) {
            val width = if (i1.width.width > i2.width.width) {
              i1.width
            } else {
              i2.width
            }
            IntMultiType(i1.signed, width, i1.endian)
          } else {
            CalcIntType
          }
        case (_: IntType, _: IntType) => CalcIntType
        case (_: NumericType, _: NumericType) => CalcFloatType
        case (_: BytesType, _: BytesType) => CalcBytesType
        case (_: BooleanType, _: BooleanType) => CalcBooleanType
        case (_: StrType, _: StrType) => CalcStrType
        case (t1: UserType, t2: UserType) =>
          // Two user types can differ in reserved size and/or processing, but that doesn't matter in case of
          // type combining - we treat them the same as long as they result in same class spec or have same
          // opaque name
          (t1.classSpec, t2.classSpec) match {
            case (None, None) =>
              // opaque classes
              if (t1.name == t2.name) {
                t1
              } else {
                if (t1.isOwning || t2.isOwning) {
                  KaitaiStructType
                } else {
                  CalcKaitaiStructType()
                }
              }
            case (Some(cs1), Some(cs2)) =>
              if (cs1 == cs2) {
                t1
              } else {
                if (t1.isOwning || t2.isOwning) {
                  KaitaiStructType
                } else {
                  CalcKaitaiStructType()
                }
              }
            case (_, _) =>
              if (t1.isOwning || t2.isOwning) {
                KaitaiStructType
              } else {
                CalcKaitaiStructType()
              }
          }
        case (t1: StructType, t2: StructType) =>
          if (t1.isOwning || t2.isOwning) {
            KaitaiStructType
          } else {
            CalcKaitaiStructType()
          }
        case (t1: EnumType, t2: EnumType) =>
          if (t1.enumSpec.get == t2.enumSpec.get) {
            val t = EnumType(t1.name, t1.owner, CalcIntType)
            t.enumSpec = t1.enumSpec
            t
          } else {
            AnyType
          }
        case (a1: ArrayType, a2: ArrayType) => CalcArrayType(combineTypesAndFail(a1.elType, a2.elType))
        case _ => AnyType
      }
    }
  }

  /**
    * Helper method to combine arbitrary number of types at once. Uses combineTypes mechanics internally.
    * @param types types to combine
    * @return type that can accommodate values of all source types without any data loss
    */
  def combineTypes(types: Iterable[DataType]): DataType = types.reduceLeft(combineTypes)

  /**
    * Tries to combine types using combineType. Throws exception when no sane combining type can be found
    * (i.e. only last-resort AnyType results).
    * @param t1 first type
    * @param t2 second type
    * @return type that can accommodate values of both source types without any data loss
    */
  def combineTypesAndFail(t1: DataType, t2: DataType): DataType = {
    if (t1 == AnyType || t2 == AnyType) {
      // combining existing AnyTypes is not a crime :)
      AnyType
    } else {
      combineTypes(t1, t2) match {
        case AnyType =>
          throw new TypeMismatchError(s"can't combine output types: $t1 vs $t2")
        case ct => ct
      }
    }
  }

  /**
    * Returns true if one can assign value of type `src` into a variable / parameter of type `dst`.
    * @param src data type of source value to be assigned
    * @param dst destination data type to be assigned into
    * @return true if assign if possible
    */
  def canAssign(src: DataType, dst: DataType): Boolean = {
    if (src == dst) {
      // Obviously, if types are equal, they'll fit into one another
      true
    } else {
      (src, dst) match {
        case (_, AnyType) => true
        case (_: IntType, _: IntType) => true
        case (_: FloatType, _: FloatType) => true
        case (_: BytesType, _: BytesType) => true
        case (_: BooleanType, _: BooleanType) => true
        case (_: StrType, _: StrType) => true
        case (_: UserType, KaitaiStructType) => true
        case (_: UserType, _: CalcKaitaiStructType) => true
        case (KaitaiStructType, _: CalcKaitaiStructType) => true
        case (t1: UserType, t2: UserType) =>
          (t1.classSpec, t2.classSpec) match {
            case (None, None) =>
              // opaque classes are assignable if their names match
              t1.name == t2.name
            case (Some(cs1), Some(cs2)) =>
              // normal user types are assignable if their class specs match
              cs1 == cs2
            case (_, _) =>
              false
          }
        case (t1: EnumType, t2: EnumType) =>
          // enums are assignable if their enumSpecs match
          t1.enumSpec.get == t2.enumSpec.get
        case (a1: ArrayType, a2: ArrayType) => canAssign(a1.elType, a2.elType)
        case (_, _) => false
      }
    }
  }
}
