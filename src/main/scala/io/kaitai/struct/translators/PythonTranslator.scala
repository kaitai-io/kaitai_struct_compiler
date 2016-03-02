package io.kaitai.struct.translators

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr

class PythonTranslator(provider: TypeProvider) extends BaseTranslator(provider) {
  override def doStringLiteral(s: String): String = "u\"" + s + "\""
  override def doName(s: String) = s"self.$s"

  override def booleanOp(op: Ast.boolop) = op match {
    case Ast.boolop.Or => "or"
    case Ast.boolop.Or => "and"
  }

  override def doSubscript(container: expr, idx: expr): String =
    s"${translate(container)}[${translate(idx)}]"
  override def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String =
    s"${translate(ifTrue)} if ${translate(condition)} else ${translate(ifFalse)}"

  // Predefined methods of various types
  override def strToInt(s: Ast.expr, base: Ast.expr): String =
    ???
  override def strLength(value: Ast.expr): String =
    s"len(${translate(value)})"
  override def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): String =
    s"${translate(s)}[${translate(from)}:${translate(to)}]"
}
