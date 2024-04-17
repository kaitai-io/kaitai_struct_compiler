package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
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
    detectTypeRaw(v) match {
      case st: SwitchType => st.combinedType
      case other => other
    }
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
        if (x < 0 || x > 255) {
          CalcIntType
        } else if (x <= 127) {
          // [0..127] => signed 1-byte integer
          Int1Type(true)
        } else {
          // [128..255] => unsigned 1-byte integer
          Int1Type(false)
        }
      case Ast.expr.FloatNum(_) => CalcFloatType
      case Ast.expr.Str(_) => CalcStrType
      case Ast.expr.InterpolatedStr(_) => CalcStrType
      case Ast.expr.Bool(_) => CalcBooleanType
      case Ast.expr.EnumByLabel(enumType, _, inType) =>
        val t = EnumType(inType.names.toList :+ enumType.name, CalcIntType)
        t.enumSpec = Some(provider.resolveEnum(inType, enumType.name))
        t
      case Ast.expr.EnumById(enumType, _, inType) =>
        val t = EnumType(List(enumType.name), CalcIntType)
        t.enumSpec = Some(provider.resolveEnum(inType, enumType.name))
        t
      case Ast.expr.Name(name: Ast.identifier) => provider.determineType(name.name).asNonOwning()
      case Ast.expr.InternalName(id) => provider.determineType(id)
      case Ast.expr.UnaryOp(op: Ast.unaryop, v: Ast.expr) =>
        val t = detectType(v)
        (t, op) match {
          case (IntMultiType(_, w, _), Ast.unaryop.Minus | Ast.unaryop.Invert) if w.width > 4 => t
          case (_: IntType, Ast.unaryop.Minus | Ast.unaryop.Invert) => CalcIntType
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
          case (_: IntType, _: IntType, _) => CalcIntType
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
            val trueType = detectType(ifTrue)
            val falseType = detectType(ifFalse)
            combineTypesAndFail(trueType, falseType)
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
          case "to_i" => CalcIntType
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
          case (_: BytesType, "to_s") => CalcStrType
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
            val t = EnumType(t1.name, CalcIntType)
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
