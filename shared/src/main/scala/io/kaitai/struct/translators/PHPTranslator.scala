package io.kaitai.struct.translators

import io.kaitai.struct.Utils
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType.IntType
import io.kaitai.struct.languages.PHPCompiler

class PHPTranslator(provider: TypeProvider, lang: PHPCompiler) extends BaseTranslator(provider) {
  override def doByteArrayLiteral(arr: Seq[Byte]): String =
    "\"" + Utils.hexEscapeByteArray(arr) + "\""

  override def numericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Div) =>
        s"intval(${translate(left)} / ${translate(right)})"
      case (_: IntType, _: IntType, Ast.operator.Mod) =>
        s"${PHPCompiler.kstreamName}::mod(${translate(left)}, ${translate(right)})"
      case _ =>
        super.numericBinOp(left, op, right)
    }
  }

  override def userTypeField(value: expr, attrName: String): String =
    s"${translate(value)}->${doName(attrName)}"

  override def doLocalName(s: String) = {
    s match {
      case "_" => "$_"
      case _ => s"$$this->${doName(s)}"
    }
  }

  override def doName(s: String) = s"${Utils.lowerCamelCase(s)}()"

  override def doEnumByLabel(enumTypeAbs: List[String], label: String): String = {
    val enumClass = lang.types2classAbs(enumTypeAbs)
    s"$enumClass::${label.toUpperCase}"
  }
  override def doEnumById(enumTypeAbs: List[String], id: String) =
    // Just an integer, without any casts / resolutions - one would have to look up constants manually
    id

  override def doSubscript(container: expr, idx: expr): String =
    s"${translate(container)}[${translate(idx)}]"
  override def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String =
    s"(${translate(condition)} ? ${translate(ifTrue)} : ${translate(ifFalse)})"

  // Predefined methods of various types
  override def strConcat(left: Ast.expr, right: Ast.expr): String =
    s"${translate(left)} . ${translate(right)}"

  override def strToInt(s: expr, base: expr): String =
    s"intval(${translate(s)}, ${translate(base)})"

  override def intToStr(i: expr, base: expr): String = {
    val baseStr = translate(base)
    baseStr match {
      case "10" =>
        s"strval(${translate(i)})"
      case _ =>
        s"base_convert(strval(${translate(i)}), 10, $baseStr)"
    }
  }

  override def strLength(s: expr): String =
    s"strlen(${translate(s)})"

  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"${translate(s)}.substring(${translate(from)}, ${translate(to)})"

  override def arrayFirst(a: expr): String =
    s"${translate(a)}[0]"
  override def arrayLast(a: expr): String = {
    val v = translate(a)
    s"$v[$v.length - 1]"
  }
}
