package io.kaitai.struct.translators

import io.kaitai.struct.Utils
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast._

class CSharpTranslator(provider: TypeProvider) extends BaseTranslator(provider) {
  override def doName(s: String) =
    s match {
      case "_root" => s
      case "_parent" => s
      case "_io" => s
      case _ => s"${Utils.lowerCamelCase(s)}"
    }

  override def doEnumByLabel(enumType: String, label: String): String =
    s"${Utils.upperCamelCase(enumType)}.${Utils.upperCamelCase(label)}"

  override def doStrCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr) = {
    if (op == Ast.cmpop.Eq) {
      s"${translate(left)} == ${translate(right)}"
    } else if (op == Ast.cmpop.NotEq) {
      s"${translate(left)} != ${translate(right)}"
    } else {
      s"(${translate(left)}.CompareTo(${translate(right)}) ${cmpOp(op)} 0)"
    }
  }

  override def doSubscript(container: expr, idx: expr): String =
    s"${translate(container)}[${translate(idx)}]"
  override def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String =
    s"${translate(condition)} ? ${translate(ifTrue)} : ${translate(ifFalse)}"

  // Predefined methods of various types
  override def strToInt(s: expr, base: expr): String =
  s"long.Parse(${translate(s)})"
  override def strLength(s: expr): String =
    s"${translate(s)}.Length"
  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"${translate(s)}.Substring(${translate(from)}, ${translate(to)} - ${translate(from)})"

  override def arrayFirst(a: expr): String =
    s"${translate(a)}[0]"
  override def arrayLast(a: expr): String = {
    val v = translate(a)
    s"$v[$v.Length - 1]"
  }
}
