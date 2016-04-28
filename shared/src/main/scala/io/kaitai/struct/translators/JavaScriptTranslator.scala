package io.kaitai.struct.translators

import io.kaitai.struct.Utils
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr

class JavaScriptTranslator(provider: TypeProvider) extends BaseTranslator(provider) {
  override def intBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) = {
    op match {
      case Ast.operator.Div =>
        s"Math.floor(${translate(left)} / ${translate(right)})"
      case _ =>
        super.intBinOp(left, op, right)
    }
  }

  override def doLocalName(s: String) = s"this.${doName(s)}"
  override def doName(s: String) = {
    s match {
      case "_root" | "_parent" | "_io" => s
      case _ => Utils.lowerCamelCase(s)
    }
  }

  override def doEnumByLabel(enumType: String, label: String): String =
    s"this._root.constructor.${Utils.upperCamelCase(enumType)}.${label.toUpperCase}"

  override def doSubscript(container: expr, idx: expr): String =
    s"${translate(container)}[${translate(idx)}]"
  override def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String =
    s"${translate(condition)} ? ${translate(ifTrue)} : ${translate(ifFalse)}"

  // Predefined methods of various types
  override def strToInt(s: expr, base: expr): String =
    s"Number.parseInt(${translate(s)}, ${translate(base)})"

  override def strLength(s: expr): String =
    s"${translate(s)}.length"

  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"${translate(s)}.substring(${translate(from)}, ${translate(to)})"

  override def arrayFirst(a: expr): String =
    s"${translate(a)}[0]"
  override def arrayLast(a: expr): String = {
    val v = translate(a)
    s"$v.get($v.length - 1)"
  }
}
