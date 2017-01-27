package io.kaitai.struct.translators

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.{cmpop, expr}
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format.{ClassSpec, EnumSpec}

trait TypeProvider {
  def nowClass: ClassSpec
  def determineType(attrName: String): BaseType
  def determineType(inClass: ClassSpec, attrName: String): BaseType
  def resolveEnum(enumName: String): EnumSpec
}

class TypeMismatchError(msg: String) extends RuntimeException(msg)
class TypeUndecidedError(msg: String) extends RuntimeException(msg)

abstract class BaseTranslator(val provider: TypeProvider) {
  import BaseTranslator._

  def translate(v: Ast.expr): String = {
    v match {
      case Ast.expr.IntNum(n) =>
        doIntLiteral(n)
      case Ast.expr.FloatNum(n) =>
        doFloatLiteral(n)
      case Ast.expr.Str(s) =>
        doStringLiteral(s)
      case Ast.expr.Bool(n) =>
        doBoolLiteral(n)
      case Ast.expr.EnumById(enumType, id) =>
        val enumSpec = provider.resolveEnum(enumType.name)
        doEnumById(enumSpec.name, translate(id))
      case Ast.expr.EnumByLabel(enumType, label) =>
        val enumSpec = provider.resolveEnum(enumType.name)
        doEnumByLabel(enumSpec.name, label.name)
      case Ast.expr.Name(name: Ast.identifier) =>
        doLocalName(name.name)
      case Ast.expr.UnaryOp(op: Ast.unaryop, v: Ast.expr) =>
        s"${unaryOp(op)}${translate(v)}"
      case Ast.expr.Compare(left: Ast.expr, op: Ast.cmpop, right: Ast.expr) =>
        (detectType(left), detectType(right)) match {
          case (_: NumericType, _: NumericType) =>
            doNumericCompareOp(left, op, right)
          case (_: StrType, _: StrType) =>
            doStrCompareOp(left, op, right)
          case (EnumType(ltype, _), EnumType(rtype, _)) =>
            if (ltype != rtype) {
              throw new TypeMismatchError(s"can't compare enums type ${ltype} and ${rtype}")
            } else {
              doEnumCompareOp(left, op, right)
            }
          case (ltype, rtype) =>
            throw new RuntimeException(s"can't compare ${ltype} and ${rtype}")
        }
      case Ast.expr.BinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) =>
        (detectType(left), detectType(right), op) match {
          case (_: NumericType, _: NumericType, _) =>
            numericBinOp(left, op, right)
          case (_: StrType, _: StrType, Ast.operator.Add) =>
            strConcat(left, right)
          case (ltype, rtype, _) =>
            throw new RuntimeException(s"can't do ${ltype} ${op} ${rtype}")
        }
      case Ast.expr.BoolOp(op: Ast.boolop, values: Seq[Ast.expr]) =>
        doBooleanOp(op, values)
      case Ast.expr.IfExp(condition: expr, ifTrue: expr, ifFalse: expr) =>
        doIfExp(condition, ifTrue, ifFalse)
      case Ast.expr.Subscript(container: Ast.expr, idx: Ast.expr) =>
        detectType(idx) match {
          case _: IntType =>
            doSubscript(container, idx)
          case idxType =>
            throw new RuntimeException(s"can't use $idx as array index (need int, got $idxType)")
        }
      case Ast.expr.Attribute(value: Ast.expr, attr: Ast.identifier) =>
        val valType = detectType(value)
        valType match {
          case _: UserType =>
            userTypeField(value, attr.name)
          case _: StrType =>
            attr.name match {
              case "length" => strLength(value)
              case "to_i" => strToInt(value, Ast.expr.IntNum(10))
            }
          case _: IntType =>
            attr.name match {
              case "to_s" => intToStr(value, Ast.expr.IntNum(10))
            }
          case ArrayType(inType) =>
            attr.name match {
              case "first" => arrayFirst(value)
              case "last" => arrayLast(value)
            }
          case KaitaiStreamType =>
            attr.name match {
              case "size" => kaitaiStreamSize(value)
              case "eof" => kaitaiStreamEof(value)
              case "pos" => kaitaiStreamPos(value)
            }
        }
      case Ast.expr.Call(func: Ast.expr, args: Seq[Ast.expr]) =>
        func match {
          case Ast.expr.Attribute(obj: Ast.expr, methodName: Ast.identifier) =>
            val objType = detectType(obj)
            (objType, methodName.name) match {
              // TODO: check argument quantity
              case (_: StrType, "substring") => strSubstring(obj, args(0), args(1))
              case (_: StrType, "to_i") => strToInt(obj, args(0))
              case _ => throw new RuntimeException(s"don't know how to call method '$methodName' of object type '$objType'")
            }
        }
      case Ast.expr.List(values: Seq[Ast.expr]) =>
        val t = detectArrayType(values)
        t match {
          case Int1Type(_) =>
            val literalBytes: Seq[Byte] = values.map {
              case Ast.expr.IntNum(x) =>
                if (x < 0 || x > 0xff) {
                  throw new TypeMismatchError(s"got a weird byte value in byte array: $x")
                } else {
                  x.toByte
                }
              case n =>
                throw new RuntimeException(s"got $n in byte array, unable to put it literally")
            }
            doByteArrayLiteral(literalBytes)
          case _ =>
            doArrayLiteral(t, values)
        }
    }
  }

  def numericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) = {
    s"(${translate(left)} ${binOp(op)} ${translate(right)})"
  }

  def binOp(op: Ast.operator): String = {
    op match {
      case Ast.operator.Add => "+"
      case Ast.operator.Sub => "-"
      case Ast.operator.Mult => "*"
      case Ast.operator.Div => "/"
      case Ast.operator.Mod => "%"
      case Ast.operator.BitAnd => "&"
      case Ast.operator.BitOr => "|"
      case Ast.operator.BitXor => "^"
      case Ast.operator.LShift => "<<"
      case Ast.operator.RShift => ">>"
    }
  }

  def doNumericCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr) = {
    s"${translate(left)} ${cmpOp(op)} ${translate(right)}"
  }

  def doStrCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr) = {
    s"${translate(left)} ${cmpOp(op)} ${translate(right)}"
  }

  def doEnumCompareOp(left: expr, op: cmpop, right: expr): String = {
    s"${translate(left)} ${cmpOp(op)} ${translate(right)}"
  }

  def cmpOp(op: Ast.cmpop): String = {
    op match {
      case Ast.cmpop.Lt => "<"
      case Ast.cmpop.LtE => "<="
      case Ast.cmpop.Gt => ">"
      case Ast.cmpop.GtE => ">="
      case Ast.cmpop.Eq => "=="
      case Ast.cmpop.NotEq => "!="
    }
  }

  def doBooleanOp(op: Ast.boolop, values: Seq[Ast.expr]): String = {
    val opStr = s"${booleanOp(op)}"
    val dividerStr = s") ${opStr} ("
    val valuesStr = values.map(translate).mkString("(", dividerStr, ")")

    // Improve compatibility for statements like: ( ... && ... || ... ) ? ... : ...
    s" (${valuesStr}) "
  }

  def booleanOp(op: Ast.boolop) = op match {
    case Ast.boolop.Or => "||"
    case Ast.boolop.And => "&&"
  }

  def unaryOp(op: Ast.unaryop) = op match {
    case Ast.unaryop.Invert => "~"
    case Ast.unaryop.Minus => "-"
    case Ast.unaryop.Not => "!"
  }

  def doSubscript(container: expr, idx: expr): String
  def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String

  // Literals
  def doIntLiteral(n: BigInt): String = n.toString
  def doFloatLiteral(n: Any): String = n.toString
  def doStringLiteral(s: String): String = "\"" + s + "\""
  def doBoolLiteral(n: Boolean): String = n.toString
  def doArrayLiteral(t: BaseType, value: Seq[expr]): String = "[" + value.map((v) => translate(v)).mkString(", ") + "]"
  def doByteArrayLiteral(arr: Seq[Byte]): String = "[" + arr.map(_ & 0xff).mkString(", ") + "]"

  def doLocalName(s: String): String = doName(s)
  def doName(s: String): String
  def userTypeField(value: expr, attrName: String): String =
    s"${translate(value)}.${doName(attrName)}"
  def doEnumByLabel(enumTypeAbs: List[String], label: String): String
  def doEnumById(enumTypeAbs: List[String], id: String): String

  // Predefined methods of various types
  def strConcat(left: Ast.expr, right: Ast.expr): String = s"${translate(left)} + ${translate(right)}"
  def strToInt(s: Ast.expr, base: Ast.expr): String
  def intToStr(i: Ast.expr, base: Ast.expr): String
  def strLength(s: Ast.expr): String
  def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): String

  def arrayFirst(a: Ast.expr): String
  def arrayLast(a: Ast.expr): String

  def kaitaiStreamSize(value: Ast.expr): String = userTypeField(value, "size")
  def kaitaiStreamEof(value: Ast.expr): String = userTypeField(value, "is_eof")
  def kaitaiStreamPos(value: Ast.expr): String = userTypeField(value, "pos")

  def detectType(v: Ast.expr): BaseType = {
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
      case Ast.expr.Bool(_) => BooleanType
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
          case (BooleanType, Ast.unaryop.Not) => t
          case _ => throw new TypeMismatchError(s"unable to apply unary operator $op to $t")
        }
      case Ast.expr.Compare(left: Ast.expr, op: Ast.cmpop, right: Ast.expr) =>
        val ltype = detectType(left)
        val rtype = detectType(right)
        (ltype, rtype) match {
          case (_: StrType, _: StrType) => // ok
          case (_: NumericType, _: NumericType) => // ok
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
        BooleanType
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
          if (t != BooleanType) {
            throw new TypeMismatchError(s"unable to use $t argument in $op boolean expression")
          }
        })
        BooleanType
      case Ast.expr.IfExp(condition: expr, ifTrue: expr, ifFalse: expr) =>
        detectType(condition) match {
          case BooleanType =>
            val trueType = detectType(ifTrue)
            val falseType = detectType(ifFalse)
            combineTypesAndFail(trueType, falseType)
          case other => throw new TypeMismatchError(s"unable to switch over $other")
        }
      case Ast.expr.Subscript(container: Ast.expr, idx: Ast.expr) =>
        detectType(container) match {
          case ArrayType(elType: BaseType) =>
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
              case _ => throw new TypeMismatchError(s"called invalid attribute '${attr.name}' on expression of type $valType")
            }
          case KaitaiStreamType =>
            attr.name match {
              case "size" => CalcIntType
              case "pos" => CalcIntType
              case "eof" => BooleanType
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
  def detectArrayType(values: Seq[expr]): BaseType = {
    var t1o: Option[BaseType] = None

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

object BaseTranslator {
  /**
    * Checks if the values of two types can be combined (i.e. there exists a single type that can
    * be used to hold values of both values - usually it means that they're either equal or one
    * is a subset of another).
    *
    * @param t1 first type
    * @param t2 second type
    * @return type that can accommodate values of both source types without any data loss
    */
  def combineTypes(t1: BaseType, t2: BaseType): BaseType = {
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
  def combineTypes(types: Iterable[BaseType]): BaseType = types.reduceLeft(combineTypes)

  /**
    * Tries to combine types using combineType. Throws exception when no sane combining type can be found
    * (i.e. only last-resort AnyType results).
    * @param t1 first type
    * @param t2 second type
    * @return type that can accommodate values of both source types without any data loss
    */
  def combineTypesAndFail(t1: BaseType, t2: BaseType): BaseType = {
    combineTypes(t1, t2) match {
      case AnyType =>
        throw new TypeMismatchError(s"can't combine output types: $t1 vs $t2")
      case ct => ct
    }
  }
}
