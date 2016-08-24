package io.kaitai.struct.translators

import io.kaitai.struct.Utils
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType.{BaseType, Int1Type}

class PythonTranslator(provider: TypeProvider) extends BaseTranslator(provider) {
  override def doStringLiteral(s: String): String = "u\"" + s + "\""
  override def doBoolLiteral(n: Boolean): String = if (n) "True" else "False"

  override def doArrayLiteral(t: BaseType, value: Seq[expr]): String = {
    t match {
      case Int1Type(_) =>
        val arrStr = super.doArrayLiteral(t, value)
        s"str(bytearray($arrStr))"
      case _ =>
        super.doArrayLiteral(t, value)
    }
  }

  override def doLocalName(s: String) = {
    s match {
      case "_" => s
      case _ => s"self.${doName(s)}"
    }
  }
  override def doName(s: String) = s

  override def doEnumByLabel(enumType: String, label: String): String =
    s"self._root.${Utils.upperCamelCase(enumType)}.${label}"

  override def booleanOp(op: Ast.boolop) = op match {
    case Ast.boolop.Or => "or"
    case Ast.boolop.And => "and"
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

  override def arrayFirst(a: expr): String =
    s"${translate(a)}[0]"
  override def arrayLast(a: expr): String =
    s"${translate(a)}[-1]"

  override def kaitaiStreamSize(value: Ast.expr): String =
    s"${translate(value)}.size()"
}
