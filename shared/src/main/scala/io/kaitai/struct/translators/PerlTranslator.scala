package io.kaitai.struct.translators

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType.BaseType

class PerlTranslator(provider: TypeProvider) extends BaseTranslator(provider) {
  override def doBoolLiteral(n: Boolean): String = if (n) "1" else "0"

  override def doArrayLiteral(t: BaseType, value: Seq[expr]): String =
    "(" + value.map((v) => translate(v)).mkString(", ") + ")"

  override def userTypeField(value: expr, attrName: String): String =
    s"${translate(value)}->${doName(attrName)}"

  override def doLocalName(s: String) = {
    s match {
      case "_" => "$_"
      case _ => s"$$self->${doName(s)}"
    }
  }

  override def doName(s: String) = {
    s match {
      case "_" => "$_"
      case _ => s"{$s}"
    }
  }

  override def doEnumByLabel(enumType: String, label: String): String =
    s"$$${enumType.toUpperCase}_${label.toUpperCase}"

  override def doSubscript(container: expr, idx: expr): String =
    s"${translate(container)}[${translate(idx)}]"
  override def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String =
    s"${translate(condition)} ? ${translate(ifTrue)} : ${translate(ifFalse)}"

  // Predefined methods of various types
  override def strToInt(s: Ast.expr, base: Ast.expr): String = {
    base match {
      case Ast.expr.IntNum(baseNum) =>
        baseNum.toInt match {
          case 10 =>
            translate(s)
          case 8 =>
            s"oct(${translate(s)})"
          case 16 =>
            s"hex(${translate(s)})"
        }
    }
  }
  override def strLength(value: Ast.expr): String =
    s"length(${translate(value)})"
  override def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): String =
    s"${translate(s)}[${translate(from)}:${translate(to)}]"

  override def arrayFirst(a: expr): String =
    s"${translate(a)}[0]"
  override def arrayLast(a: expr): String =
    s"${translate(a)}[-1]"

  override def kaitaiStreamSize(value: Ast.expr): String =
    s"${translate(value)}->size()"
}
