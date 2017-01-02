package io.kaitai.struct.translators

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType.{BaseType, IntType}
import io.kaitai.struct.translators.components.CTernaryOperator

class PerlTranslator(provider: TypeProvider) extends BaseTranslator(provider) with CTernaryOperator {
  override def numericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Div) =>
        val prio = binOpPriority(Ast.operator.Div)
        (s"int(${translate(left, prio)} / ${translate(right, prio)})", 0)
      case _ =>
        super.numericBinOp(left, op, right)
    }
  }

  override def doBoolLiteral(n: Boolean): (String, Int) = (if (n) "1" else "0", 0)

  override def doArrayLiteral(t: BaseType, value: Seq[expr]): (String, Int) =
    ("(" + value.map((v) => translate(v)).mkString(", ") + ")", 0)

  override def doByteArrayLiteral(arr: Seq[Byte]): (String, Int) =
    (s"pack('C*', (${arr.map(_ & 0xff).mkString(", ")}))", 0)

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

  override def doEnumByLabel(enumType: List[String], label: String): (String, Int) =
    (s"$$${enumType.last.toUpperCase}_${label.toUpperCase}", 0)
  override def doEnumById(enumTypeAbs: List[String], id: String): (String, Int) =
    // Just an integer, without any casts / resolutions - one would have to look up constants manually
    (id, 0)

  override def doStrCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr) = {
    val prio = cmpOpPriority(op)
    val opStr = op match {
      case Ast.cmpop.Eq => "eq"
      case Ast.cmpop.NotEq => "ne"
      case Ast.cmpop.Lt => "lt"
      case Ast.cmpop.LtE => "le"
      case Ast.cmpop.Gt => "gt"
      case Ast.cmpop.GtE => "ge"
    }
    (s"${translate(left, prio)} $opStr ${translate(right, prio)}", prio)
  }

  override def doSubscript(container: expr, idx: expr): String =
    s"${translate(container)}[${translate(idx)}]"

  // Predefined methods of various types
  override def strConcat(left: Ast.expr, right: Ast.expr): (String, Int) = {
    val prio = binOpPriority(Ast.operator.Add)
    (s"${translate(left, prio)} . ${translate(right, prio)}", prio)
  }
  override def strToInt(s: Ast.expr, base: Ast.expr): String = {
    base match {
      case Ast.expr.IntNum(baseNum) =>
        baseNum.toInt match {
          case 10 =>
            s"(${translate(s, 0)})"
          case 8 =>
            s"oct(${translate(s, 0)})"
          case 16 =>
            s"hex(${translate(s, 0)})"
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
