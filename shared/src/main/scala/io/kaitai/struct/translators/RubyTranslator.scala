package io.kaitai.struct.translators

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.languages.RubyCompiler
import io.kaitai.struct.translators.components.CTernaryOperator

class RubyTranslator(provider: TypeProvider) extends BaseTranslator(provider) with CTernaryOperator {
  override def doByteArrayLiteral(arr: Seq[Byte]): (String, Int) = {
    val (bal, prio) = super.doByteArrayLiteral(arr)
    (s"$bal.pack('C*')", prio)
  }

  override def doName(s: String) = s

  override def doEnumByLabel(enumTypeAbs: List[String], label: String): (String, Int) =
    (s":${enumTypeAbs.last}_$label", 0)
  override def doEnumById(enumType: List[String], id: String): (String, Int) =
    (s"${RubyCompiler.kstreamName}::resolve_enum(${enumType.last.toUpperCase}, $id)", 0)

  override def doSubscript(container: Ast.expr, idx: Ast.expr): String =
    s"${translate(container)}[${translate(idx)}]"

  // Predefined methods of various types
  override def strToInt(s: Ast.expr, base: Ast.expr): String = {
    val baseStr = translate(base, 0)
    translate(s) + ".to_i" + (baseStr match {
      case "10" => ""
      case _ => s"($baseStr)"
    })
  }
  override def strLength(s: Ast.expr): String =
    s"${translate(s)}.size"
  override def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): String =
    s"${translate(s)}[${translate(from)}, (${translate(to)} - 1)]"

  override def arrayFirst(a: Ast.expr): String =
    s"${translate(a)}.first"
  override def arrayLast(a: Ast.expr): String =
    s"${translate(a)}.last"

  override def kaitaiStreamEof(value: Ast.expr): String =
    s"${translate(value)}.eof?"
}
