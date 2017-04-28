package io.kaitai.struct.translators

import io.kaitai.struct.exprlang.Ast

trait CommonOps extends AbstractTranslator {
  def numericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) = {
    s"(${translate(left)} ${binOp(op)} ${translate(right)})"
  }

  def binOp(op: Ast.operator): String = {
    op match {
      case Ast.operator.Add => "+"
      case Ast.operator.Sub => "-"
      case Ast.operator.Mult => "*"
      case Ast.operator.Div => "/"
      case Ast.operator.Mod => "%"
      case Ast.operator.BitAnd => "&"
      case Ast.operator.BitOr => "|"
      case Ast.operator.BitXor => "^"
      case Ast.operator.LShift => "<<"
      case Ast.operator.RShift => ">>"
    }
  }

  def doNumericCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): String =
    s"${translate(left)} ${cmpOp(op)} ${translate(right)}"

  def doStrCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): String =
    s"${translate(left)} ${cmpOp(op)} ${translate(right)}"

  def doEnumCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): String =
    s"${translate(left)} ${cmpOp(op)} ${translate(right)}"

  def doBytesCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): String =
    s"${translate(left)} ${cmpOp(op)} ${translate(right)}"

  def cmpOp(op: Ast.cmpop): String = {
    op match {
      case Ast.cmpop.Lt => "<"
      case Ast.cmpop.LtE => "<="
      case Ast.cmpop.Gt => ">"
      case Ast.cmpop.GtE => ">="
      case Ast.cmpop.Eq => "=="
      case Ast.cmpop.NotEq => "!="
    }
  }

  def doBooleanOp(op: Ast.boolop, values: Seq[Ast.expr]): String = {
    val opStr = s"${booleanOp(op)}"
    val dividerStr = s") $opStr ("
    val valuesStr = values.map(translate).mkString("(", dividerStr, ")")

    // Improve compatibility for statements like: ( ... && ... || ... ) ? ... : ...
    s" ($valuesStr) "
  }

  def booleanOp(op: Ast.boolop): String = op match {
    case Ast.boolop.Or => "||"
    case Ast.boolop.And => "&&"
  }

  def unaryOp(op: Ast.unaryop): String = op match {
    case Ast.unaryop.Invert => "~"
    case Ast.unaryop.Minus => "-"
    case Ast.unaryop.Not => "!"
  }
}
