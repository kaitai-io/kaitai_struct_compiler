package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.precompile.{TypeMismatchError, TypeUndecidedError}

/**
  * Basic class the implements type inferring functionality for Ast.expr
  * expressions. The main method is [[detectType]] that allows to derive
  * type of any given expression.
  * @param provider TypeProvider that will answer queries on user types
  */
class TypeDetector(provider: TypeProvider) {
  import TypeDetector._

  def detectType(v: Ast.expr): DataType = {
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
      case Ast.expr.Bool(_) => CalcBooleanType
      case Ast.expr.EnumByLabel(enumType, _) =>
        val t = EnumType(List(enumType.name), CalcIntType)
        t.enumSpec = Some(provider.resolveEnum(enumType.name))
        t
      case Ast.expr.EnumById(enumType, _) =>
        val t = EnumType(List(enumType.name), CalcIntType)
        t.enumSpec = Some(provider.resolveEnum(enumType.name))
        t
      case Ast.expr.Name(name: Ast.identifier) => provider.determineType(name.name)
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
      case Ast.expr.IfExp(condition: expr, ifTrue: expr, ifFalse: expr) =>
        detectType(condition) match {
          case _: BooleanType =>
            val trueType = detectType(ifTrue)
            val falseType = detectType(ifFalse)
            combineTypesAndFail(trueType, falseType)
          case other => throw new TypeMismatchError(s"unable to switch over $other")
        }
      case Ast.expr.Subscript(container: Ast.expr, idx: Ast.expr) =>
        detectType(container) match {
          case ArrayType(elType: DataType) =>
            detectType(idx) match {
              case _: IntType => elType
              case idxType => throw new TypeMismatchError(s"unable to index an array using $idxType")
            }
          case cntType => throw new TypeMismatchError(s"unable to apply operation [] to $cntType")
        }
      case Ast.expr.Attribute(value: Ast.expr, attr: Ast.identifier) =>
        val valType = detectType(value)
        valType match {
          case KaitaiStructType =>
            throw new TypeMismatchError(s"called attribute '${attr.name}' on generic struct expression '$value'")
          case t: UserType =>
            t.classSpec match {
              case Some(tt) => provider.determineType(tt, attr.name)
              case None => throw new TypeUndecidedError(s"expression '$value' has undecided type '${t.name}' (while asking for attribute '${attr.name}')")
            }
          case _: StrType =>
            attr.name match {
              case "length" => CalcIntType
              case "reverse" => CalcStrType
              case "to_i" => CalcIntType
              case _ => throw new TypeMismatchError(s"called invalid attribute '${attr.name}' on expression of type $valType")
            }
          case _: IntType =>
            attr.name match {
              case "to_s" => CalcStrType
              case _ => throw new TypeMismatchError(s"called invalid attribute '${attr.name}' on expression of type $valType")
            }
          case ArrayType(inType) =>
            attr.name match {
              case "first" | "last" => inType
              case "size" => CalcIntType
              case _ => throw new TypeMismatchError(s"called invalid attribute '${attr.name}' on expression of type $valType")
            }
          case KaitaiStreamType =>
            attr.name match {
              case "size" => CalcIntType
              case "pos" => CalcIntType
              case "eof" => CalcBooleanType
              case _ => throw new TypeMismatchError(s"called invalid attribute '${attr.name}' on expression of type $valType")
            }
          case et: EnumType =>
            attr.name match {
              case "to_i" => CalcIntType
              case _ => throw new TypeMismatchError(s"called invalid attribute '${attr.name}' on expression of type $valType")
            }
          case _: BooleanType =>
            attr.name match {
              case "to_i" => CalcIntType
              case _ => throw new TypeMismatchError(s"called invalid attribute '${attr.name}' on expression of type $valType")
            }
          case _ =>
            throw new TypeMismatchError(s"don't know how to call anything on $valType")
        }
      case Ast.expr.Call(func: Ast.expr, args: Seq[Ast.expr]) =>
        func match {
          case Ast.expr.Attribute(obj: Ast.expr, methodName: Ast.identifier) =>
            val objType = detectType(obj)
            (objType, methodName.name) match {
              case (_: StrType, "substring") => CalcStrType
              case (_: StrType, "to_i") => CalcIntType
              case _ => throw new RuntimeException(s"don't know how to call method '$methodName' of object type '$objType'")
            }
        }
      case Ast.expr.List(values: Seq[Ast.expr]) =>
        detectArrayType(values) match {
          case Int1Type(_) => CalcBytesType
          case t => ArrayType(t)
        }
      case Ast.expr.CastToType(value, typeName) =>
        provider.resolveType(typeName.name)
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
  def detectArrayType(values: Seq[expr]): DataType = {
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
      case (EnumType(name1, _), EnumType(name2, _)) =>
        if (name1 != name2) {
          throw new TypeMismatchError(s"can't compare different enums '$name1' and '$name2'")
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
                KaitaiStructType
              }
            case (Some(cs1), Some(cs2)) =>
              if (cs1 == cs2) {
                t1
              } else {
                KaitaiStructType
              }
            case (_, _) =>
              KaitaiStructType
          }
        case (_: UserType, KaitaiStructType) => KaitaiStructType
        case (KaitaiStructType, _: UserType) => KaitaiStructType
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
    combineTypes(t1, t2) match {
      case AnyType =>
        throw new TypeMismatchError(s"can't combine output types: $t1 vs $t2")
      case ct => ct
    }
  }
}
