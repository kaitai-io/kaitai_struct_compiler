package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType.EnumType
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format.Identifier
import io.kaitai.struct.languages.RubyCompiler

class RubyTranslator(provider: TypeProvider) extends BaseTranslator(provider) {
  override def doByteArrayLiteral(arr: Seq[Byte]): String =
    s"${super.doByteArrayLiteral(arr)}.pack('C*')"

  // https://github.com/ruby/ruby/blob/trunk/doc/syntax/literals.rdoc#strings
  // https://github.com/ruby/ruby/blob/trunk/string.c - see "rb_str_inspect"
  override val asciiCharQuoteMap: Map[Char, String] = Map(
    '\t' -> "\\t",
    '\n' -> "\\n",
    '\r' -> "\\r",
    '"' -> "\\\"",
    '\\' -> "\\\\",

    '#' -> "\\#",
    '\7' -> "\\a",
    '\f' -> "\\f",
    '\13' -> "\\v",
    '\33' -> "\\e",
    '\b' -> "\\b"
  )

  override def doName(s: String) = {
    s match {
      case Identifier.INDEX => "i" // FIXME: probably would clash with attribute named "i"
      case _ => s
    }
  }

  override def doEnumByLabel(enumTypeAbs: List[String], label: String): String =
    s":${enumTypeAbs.last}_$label"
  override def doEnumById(enumType: List[String], id: String): String =
    s"${RubyCompiler.kstreamName}::resolve_enum(${enumType.last.toUpperCase}, $id)"

  override def doSubscript(container: Ast.expr, idx: Ast.expr): String =
    s"${translate(container)}[${translate(idx)}]"
  override def doIfExp(condition: Ast.expr, ifTrue: Ast.expr, ifFalse: Ast.expr): String =
    s"(${translate(condition)} ? ${translate(ifTrue)} : ${translate(ifFalse)})"

  // Predefined methods of various types
  override def strToInt(s: Ast.expr, base: Ast.expr): String = {
    val baseStr = translate(base)
    translate(s) + ".to_i" + (baseStr match {
      case "10" => ""
      case _ => s"($baseStr)"
    })
  }
  override def enumToInt(v: Ast.expr, et: EnumType): String =
    s"${RubyCompiler.inverseEnumName(et.name.last.toUpperCase)}[${translate(v)}]"
  override def floatToInt(v: Ast.expr): String =
    s"(${translate(v)}).to_i"
  override def intToStr(i: Ast.expr, base: Ast.expr): String =
    translate(i) + s".to_s(${translate(base)})"
  override def bytesToStr(bytesExpr: String, encoding: Ast.expr): String =
    s"($bytesExpr).force_encoding(${translate(encoding)})"
  override def strLength(s: Ast.expr): String =
    s"${translate(s)}.size"
  override def strReverse(s: Ast.expr): String =
    s"${translate(s)}.reverse"
  override def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): String =
    s"${translate(s)}[${translate(from)}, (${translate(to)} - 1)]"

  override def arrayFirst(a: Ast.expr): String =
    s"${translate(a)}.first"
  override def arrayLast(a: Ast.expr): String =
    s"${translate(a)}.last"
  override def arraySize(a: Ast.expr): String =
    s"${translate(a)}.length"
  override def arrayMin(a: expr): String =
    s"${translate(a)}.min"
  override def arrayMax(a: expr): String =
    s"${translate(a)}.max"

  override def kaitaiStreamEof(value: Ast.expr): String =
    s"${translate(value)}.eof?"
}
