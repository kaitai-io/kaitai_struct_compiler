package io.kaitai.struct.translators

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.languages.RubyCompiler

class RubyTranslator(provider: TypeProvider) extends BaseTranslator(provider) {
  override def doByteArrayLiteral(arr: Seq[Byte]): String =
    s"${super.doByteArrayLiteral(arr)}.pack('C*')"

  override def doName(s: String) = s

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
  override def intToStr(i: Ast.expr, base: Ast.expr): String = {
    translate(i) + s".to_s(${translate(base)})"
  }
  override def strLength(s: Ast.expr): String =
    s"${translate(s)}.size"
  override def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): String =
    s"${translate(s)}[${translate(from)}, (${translate(to)} - 1)]"

  override def arrayFirst(a: Ast.expr): String =
    s"${translate(a)}.first"
  override def arrayLast(a: Ast.expr): String =
    s"${translate(a)}.last"
  override def arraySize(a: Ast.expr): String =
    s"${translate(a)}.length"

  override def kaitaiStreamEof(value: Ast.expr): String =
    s"${translate(value)}.eof?"
}
