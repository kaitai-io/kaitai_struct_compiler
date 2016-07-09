package io.kaitai.struct.translators

import io.kaitai.struct.exprlang.Ast.expr

class RubyTranslator(provider: TypeProvider) extends BaseTranslator(provider) {
  override def doName(s: String) = s

  override def doEnumByLabel(enumType: String, label: String): String =
    s":${enumType}_${label}"

  override def doSubscript(container: expr, idx: expr): String =
    s"${translate(container)}[${translate(idx)}]"
  override def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String =
    s"${translate(condition)} ? ${translate(ifTrue)} : ${translate(ifFalse)}"

  // Predefined methods of various types
  override def strToInt(s: expr, base: expr): String =
    s"${translate(s)}.to_i(${translate(base)})"
  override def strLength(s: expr): String =
    s"${translate(s)}.size"
  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"${translate(s)}[${translate(from)}, (${translate(to)} - 1)]"

  override def arrayFirst(a: expr): String =
    s"${translate(a)}.first"
  override def arrayLast(a: expr): String =
    s"${translate(a)}.last"
}
