package io.kaitai.struct.translators

import io.kaitai.struct.exprlang.Ast

trait CommonOps extends AbstractTranslator {
  /**
   * Provides operator precedence table, used for deciding whether
   * parenthesis guarding expression are necessary or not.
   *
   * This is the default table, based on C++ operator precedence model.
   * This is good enough for most C-like languages. Individual languages'
   * translators can override it if and when necessary to alter behavior.
   *
   * @see https://en.cppreference.com/w/cpp/language/operator_precedence
   * @see https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/
   * @see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_precedence
   * @see https://docs.oracle.com/javase/tutorial/java/nutsandbolts/operators.html
   * @see https://www.php.net/manual/en/language.operators.precedence.php
   */

  val OPERATOR_PRECEDENCE = Map[Ast.binaryop, Int](
    Ast.operator.Mult -> 130,
    Ast.operator.Div -> 130,
    Ast.operator.Mod -> 130,
    Ast.operator.Add -> 120,
    Ast.operator.Sub -> 120,
    Ast.operator.LShift -> 110,
    Ast.operator.RShift -> 110,
    Ast.cmpop.Lt -> 100,
    Ast.cmpop.LtE -> 100,
    Ast.cmpop.Gt -> 100,
    Ast.cmpop.GtE -> 100,
    Ast.cmpop.Eq -> 90,
    Ast.cmpop.NotEq -> 90,
    Ast.operator.BitAnd -> 80,
    Ast.operator.BitXor -> 70,
    Ast.operator.BitOr -> 60
  )

  def genericBinOp(left: Ast.expr, op: Ast.binaryop, right: Ast.expr, extPrec: Int): String = {
    val opStr = op match {
      case op: Ast.operator => binOp(op)
      case op: Ast.cmpop => cmpOp(op)
    }
    genericBinOpStr(left, op, opStr, right, extPrec)
  }

  def genericBinOpStr(left: Ast.expr, op: Ast.binaryop, opStr: String, right: Ast.expr, extPrec: Int): String = {
    val thisPrec = OPERATOR_PRECEDENCE(op)
    val leftStr = op match {
      case op: Ast.operator => translateBinaryOperand(left, Ast.expr.BinOp(left, op, right), thisPrec)
      case _ => translate(left, thisPrec)
    }
    val rightStr = op match {
      case Ast.operator.LShift | Ast.operator.RShift => translateShiftDistance(right, thisPrec)
      case op: Ast.operator => translateBinaryOperand(right, Ast.expr.BinOp(left, op, right), thisPrec)
      case _ => translate(right, thisPrec)
    }
    if (thisPrec <= extPrec) {
      s"($leftStr $opStr $rightStr)"
    } else {
      s"$leftStr $opStr $rightStr"
    }
  }

  /**
   * Translates `operand` (either the left or the right one) of the binary
   * operator expression `binExpr`. It isn't used for the shift distance
   * (the right operand of `<<` and `>>`), which is handled by
   * `translateShiftDistance()`.
   *
   * Languages that require both operands of an arithmetic or bitwise operator
   * to have the same type (instead of implicitly converting them like C)
   * should override this method to convert the operand to the result type of
   * `binExpr`.
   */
  def translateBinaryOperand(operand: Ast.expr, binExpr: Ast.expr.BinOp, extPrec: Int): String =
    translate(operand, extPrec)

  /**
   * Translates the shift distance `distance` (the right operand of `<<` or
   * `>>`). The result type of a shift depends only on its left operand, so
   * by default, the distance is translated as is.
   *
   * Languages that require a specific type for the shift distance should
   * override this method to convert it.
   */
  def translateShiftDistance(distance: Ast.expr, extPrec: Int): String =
    translate(distance, extPrec)

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

  def doNumericCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr, extPrec: Int): String =
    genericBinOp(left, op, right, extPrec)

  def doStrCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr, extPrec: Int): String =
    genericBinOp(left, op, right, extPrec)

  def doEnumCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr, extPrec: Int): String =
    genericBinOp(left, op, right, extPrec)

  def doBytesCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr, extPrec: Int): String =
    genericBinOp(left, op, right, extPrec)

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
