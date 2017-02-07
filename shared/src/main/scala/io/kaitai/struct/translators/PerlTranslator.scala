package io.kaitai.struct.translators

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType.{BaseType, IntType}

class PerlTranslator(provider: TypeProvider) extends BaseTranslator(provider) {
  override def numericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Div) =>
        s"int(${translate(left)} / ${translate(right)})"
      case _ =>
        super.numericBinOp(left, op, right)
    }
  }

  override def doBoolLiteral(n: Boolean): String = if (n) "1" else "0"

  override def doArrayLiteral(t: BaseType, value: Seq[expr]): String =
    "(" + value.map((v) => translate(v)).mkString(", ") + ")"

  override def doByteArrayLiteral(arr: Seq[Byte]): String =
    s"pack('C*', (${arr.map(_ & 0xff).mkString(", ")}))"

  override def userTypeField(value: expr, attrName: String): String =
    s"${translate(value)}->${doName(attrName)}"

  override def doLocalName(s: String) = {
    s match {
      case "_" | "_on" => "$" + s
      case _ => s"$$self->${doName(s)}"
    }
  }

  override def doName(s: String) = {
    s match {
      case "_" => "$_"
      case _ => s"$s()"
    }
  }

  override def doEnumByLabel(enumType: List[String], label: String): String =
    s"$$${enumType.last.toUpperCase}_${label.toUpperCase}"
  override def doEnumById(enumTypeAbs: List[String], id: String): String =
    // Just an integer, without any casts / resolutions - one would have to look up constants manually
    id

  override def doStrCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr) = {
    val opStr = op match {
      case Ast.cmpop.Eq => "eq"
      case Ast.cmpop.NotEq => "ne"
      case Ast.cmpop.Lt => "lt"
      case Ast.cmpop.LtE => "le"
      case Ast.cmpop.Gt => "gt"
      case Ast.cmpop.GtE => "ge"
    }
    s"${translate(left)} $opStr ${translate(right)}"
  }

  override def doSubscript(container: expr, idx: expr): String =
    s"${translate(container)}[${translate(idx)}]"
  override def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String =
    s"(${translate(condition)} ? ${translate(ifTrue)} : ${translate(ifFalse)})"

  // Predefined methods of various types
  override def strConcat(left: Ast.expr, right: Ast.expr): String =
    s"${translate(left)} . ${translate(right)}"
  override def strToInt(s: Ast.expr, base: Ast.expr): String = {
    val baseStr = translate(base)
    baseStr match {
      case "8" =>
        s"oct(${translate(s)})"
      case "10" =>
        translate(s)
      case "16" =>
        s"hex(${translate(s)})"
      case _ => throw new UnsupportedOperationException(baseStr)
    }
  }
  override def intToStr(i: Ast.expr, base: Ast.expr): String = {
    val baseStr = translate(base)
    val format = baseStr match {
      case "8" =>
        s"%o"
      case "10" =>
        s"%d"
      case "16" =>
        s"0x%X"
      case _ => throw new UnsupportedOperationException(baseStr)
    }

    s"sprintf('$format', ${translate(i)})"
  }
  override def strLength(value: Ast.expr): String =
    s"length(${translate(value)})"
  override def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): String =
    s"${translate(s)}[${translate(from)}:${translate(to)}]"

  override def arrayFirst(a: expr): String =
    s"${translate(a)}[0]"
  override def arrayLast(a: expr): String =
    s"${translate(a)}[-1]"
  override def arraySize(a: expr): String =
    s"scalar(${translate(a)})"

  override def kaitaiStreamSize(value: Ast.expr): String =
    s"${translate(value)}->size()"
}
