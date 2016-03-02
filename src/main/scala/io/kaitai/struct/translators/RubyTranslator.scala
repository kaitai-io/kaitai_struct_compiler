package io.kaitai.struct.translators

import io.kaitai.struct.exprlang.Ast.expr

class RubyTranslator(provider: TypeProvider) extends BaseTranslator(provider) {
  override def doName(s: String) = s"$s"

  // Predefined methods of various types
  override def strToInt(s: expr, base: expr): String =
    s"${translate(s)}.to_i(${translate(base)}"
  override def strLength(s: expr): String =
    s"${translate(s)}.size"
  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"${translate(s)}[${translate(from)}, (${translate(to)} - 1)]"
}
