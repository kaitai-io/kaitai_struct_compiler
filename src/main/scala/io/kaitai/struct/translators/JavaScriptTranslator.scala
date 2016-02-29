package io.kaitai.struct.translators

import io.kaitai.struct.exprlang.Ast.expr

object JavaScriptTranslator extends BaseTranslator {
  override def doName(s: String) = s"this.$s"

  // Predefined methods of various types
  override def strToInt(s: expr, base: expr): String =
    s"Number.parseInt(${translate(s)}, ${translate(base)})"

  override def strLength(s: expr): String =
    s"${translate(s)}.length"

  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"${translate(s)}.substring(${translate(from)}, ${translate(to)})"
}
