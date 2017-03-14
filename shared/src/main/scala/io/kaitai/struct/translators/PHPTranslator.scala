package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format.Identifier
import io.kaitai.struct.languages.PHPCompiler
import io.kaitai.struct.{RuntimeConfig, Utils}

class PHPTranslator(provider: TypeProvider, config: RuntimeConfig) extends BaseTranslator(provider) {
  override def doByteArrayLiteral(arr: Seq[Byte]): String =
    "\"" + Utils.hexEscapeByteArray(arr) + "\""

  // http://php.net/manual/en/language.types.string.php#language.types.string.syntax.double
  override val asciiCharQuoteMap: Map[Char, String] = Map(
    '\t' -> "\\t",
    '\n' -> "\\n",
    '\r' -> "\\r",
    '"' -> "\\\"",
    '\\' -> "\\\\",

    // allowed and required to not trigger variable interpolation
    '$' -> "\\$",

    '\f' -> "\\f",
    '\13' -> "\\v",
    '\33' -> "\\e"
  )

  override def strLiteralUnicode(code: Char): String =
    "\\u{%x}".format(code.toInt)

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
      case Identifier.ITERATOR => "$_"
      case Identifier.ITERATOR2 => "$_buf"
      case _ => s"$$this->${doName(s)}"
    }
  }

  override def doName(s: String) = s"${Utils.lowerCamelCase(s)}()"

  override def doEnumByLabel(enumTypeAbs: List[String], label: String): String = {
    val enumClass = types2classAbs(enumTypeAbs)
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

  override def enumToInt(v: expr, et: EnumType): String =
    translate(v)

  override def boolToInt(v: expr): String =
    s"intval(${translate(v)})"

  override def intToStr(i: expr, base: expr): String = {
    val baseStr = translate(base)
    baseStr match {
      case "10" =>
        s"strval(${translate(i)})"
      case _ =>
        s"base_convert(strval(${translate(i)}), 10, $baseStr)"
    }
  }
  override def bytesToStr(bytesExpr: String, encoding: Ast.expr): String =
    s"${PHPCompiler.kstreamName}::bytesToStr($bytesExpr, ${translate(encoding)})"
  override def strLength(s: expr): String =
    s"strlen(${translate(s)})"
  override def strReverse(s: expr): String =
    s"strrev(${translate(s)})"
  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"${translate(s)}.substring(${translate(from)}, ${translate(to)})"

  override def arrayFirst(a: expr): String =
    s"${translate(a)}[0]"
  override def arrayLast(a: expr): String = {
    val v = translate(a)
    s"$v[$v.length - 1]"
  }
  override def arraySize(a: expr): String =
    s"count(${translate(a)})"

  val namespaceRef = if (config.phpNamespace.isEmpty) {
    ""
  } else {
    "\\" + config.phpNamespace
  }

  def types2classAbs(names: List[String]) =
    names match {
      case List("kaitai_struct") => PHPCompiler.kstructName
      case _ =>
        namespaceRef + "\\" + PHPCompiler.types2classRel(names)
    }
}
