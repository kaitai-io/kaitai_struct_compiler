package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.Identifier
import io.kaitai.struct.precompile.TypeMismatchError

abstract trait CommonMethods[T] extends TypeDetector {
  /**
    * Translates a certain attribute call (as in `foo.bar`) into a rendition
    * of expression in certain target language.
    * @note Must be kept in sync with [[TypeDetector.detectAttributeType]]
    * @param call attribute call expression to translate
    * @return result of translation as [[T]]
    */
  def translateAttribute(call: Ast.expr.Attribute): T = {
    val attr = call.attr
    val value = call.value
    val valType = detectType(value)

    // Special case: will be compiled as compile-time determined constant
    if (attr.name == Identifier.SIZEOF)
      return byteSizeOfValue(value.toString, valType)

    valType match {
      case KaitaiStructType | CalcKaitaiStructType =>
        attr.name match {
          case Identifier.PARENT => kaitaiStructField(value, attr.name)
        }
      case ut: UserType =>
        userTypeField(ut, value, attr.name)
      case _: BytesType =>
        attr.name match {
          case "first" => bytesFirst(value)
          case "last" => bytesLast(value)
          case "length" | "size" => bytesLength(value)
          case "min" => bytesMin(value)
          case "max" => bytesMax(value)
        }
      case _: StrType =>
        attr.name match {
          case "length" => strLength(value)
          case "reverse" => strReverse(value)
          case "to_i" => strToInt(value, Ast.expr.IntNum(10))
        }
      case _: IntType =>
        attr.name match {
          case "to_s" => intToStr(value, Ast.expr.IntNum(10))
        }
      case _: FloatType =>
        attr.name match {
          case "to_i" => floatToInt(value)
        }
      case _: ArrayType =>
        attr.name match {
          case "first" => arrayFirst(value)
          case "last" => arrayLast(value)
          case "size" => arraySize(value)
          case "min" => arrayMin(value)
          case "max" => arrayMax(value)
        }
      case KaitaiStreamType | OwnedKaitaiStreamType =>
        attr.name match {
          case "size" => kaitaiStreamSize(value)
          case "eof" => kaitaiStreamEof(value)
          case "pos" => kaitaiStreamPos(value)
        }
      case et: EnumType =>
        attr.name match {
          case "to_i" => enumToInt(value, et)
          case _ => throw new TypeMismatchError(s"called invalid attribute '${attr.name}' on expression of type $valType")
        }
      case _: BooleanType =>
        attr.name match {
          case "to_i" => boolToInt(value)
          case _ => throw new TypeMismatchError(s"called invalid attribute '${attr.name}' on expression of type $valType")
        }
    }
  }

  /**
    * Translates a certain function call (as in `foo.bar(arg1, arg2)`) into a
    * rendition of expression in certain target language.
    * @note Must be kept in sync with [[TypeDetector.detectCallType]]
    * @param call function call expression to translate
    * @return result of translation as [[T]]
    */
  def translateCall(call: Ast.expr.Call): T = {
    val func = call.func
    val args = call.args

    func match {
      case Ast.expr.Attribute(obj: Ast.expr, methodName: Ast.identifier) =>
        val objType = detectType(obj)
        (objType, methodName.name) match {
          // TODO: check argument quantity
          case (_: StrType, "substring") => strSubstring(obj, args(0), args(1))
          case (_: StrType, "to_i") => strToInt(obj, args(0))
          case (_: BytesType, "to_s") => bytesToStr(obj, args(0))
          case _ => throw new TypeMismatchError(s"don't know how to call method '$methodName' of object type '$objType'")
        }
    }
  }

  def userTypeField(ut: UserType, value: Ast.expr, name: String): T
  def kaitaiStructField(value: Ast.expr, name: String): T

  def bytesSubscript(container: Ast.expr, idx: Ast.expr): T
  def bytesFirst(b: Ast.expr): T
  def bytesLast(b: Ast.expr): T
  def bytesLength(b: Ast.expr): T
  def bytesMin(b: Ast.expr): T
  def bytesMax(b: Ast.expr): T

  def strLength(s: Ast.expr): T
  def strReverse(s: Ast.expr): T
  def strToInt(s: Ast.expr, base: Ast.expr): T
  def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): T

  def bytesToStr(value: Ast.expr, expr: Ast.expr): T

  def intToStr(value: Ast.expr, num: Ast.expr): T

  def floatToInt(value: Ast.expr): T

  def kaitaiStreamSize(value: Ast.expr): T
  def kaitaiStreamEof(value: Ast.expr): T
  def kaitaiStreamPos(value: Ast.expr): T

  def arraySubscript(container: Ast.expr, idx: Ast.expr): T
  def arrayFirst(a: Ast.expr): T
  def arrayLast(a: Ast.expr): T
  def arraySize(a: Ast.expr): T
  def arrayMin(a: Ast.expr): T
  def arrayMax(a: Ast.expr): T

  def enumToInt(value: Ast.expr, et: EnumType): T

  def boolToInt(value: Ast.expr): T

  def byteSizeOfValue(attrName: String, valType: DataType): T
}
