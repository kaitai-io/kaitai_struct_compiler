package io.kaitai.struct.exprlang

import io.kaitai.struct.exprlang.Ast._

/**
  * Namespace which holds a bunch of methods and case classes related to
  * evaluation of constant expressions, e.g. it can predict that `1 + 2`
  * will be always constant and equal to `3`, and anything with a variable
  * in it is potentially non-constant.
  *
  * Evaluators below are relatively naive: expect no complex logic or symbolic
  * simplification of expressions here: something like `x - x`, which is
  * known to be always 0, will still report it as "potentially variable".
  */
object ConstEvaluator {
  /**
    * Evaluates the expression, if it's possible to get an integer constant
    * as the result of evaluation (i.e. if it does not involve any variables
    * or anything like that).
    *
    * @param ex expression to evaluate.
    * @return integer result of evaluation if it's constant or None, if it's
    *         variable or potentially variable.
    */
  def evaluateIntConst(ex: Ast.expr): Option[BigInt] = {
    evaluate(ex) match {
      case value.Int(x) => Some(x)
      case _ => None
    }
  }

  /**
    * Evaluates the expression, if it's possible to get a constant as the result
    * of evaluation (i.e. if it does not involve any variables or anything like
    * that).
    *
    * @param ex expression to evaluate.
    * @return [[value]] container.
    */
  def evaluate(ex: Ast.expr): value = ex match {
    case expr.IntNum(x) => value.Int(x)
    case expr.Bool(x) => value.Bool(x)
    case expr.Str(x) => value.Str(x)

    case expr.UnaryOp(op, expr.IntNum(operand)) =>
      value.Int(op match {
        case unaryop.Invert => ~operand
        case unaryop.Minus => -operand
        case _ => return value.NonConst
      })
    case expr.UnaryOp(unaryop.Not, expr.Bool(operand)) => value.Bool(!operand)

    case expr.BinOp(left, op, right) =>
      val leftValue = evaluate(left)
      val rightValue = evaluate(right)
      (op, leftValue, rightValue) match {
        case (operator.Add, value.Str(l), value.Str(r)) => value.Str(l + r)
        case (_, value.Int(l), value.Int(r)) => value.Int(op match {
          case operator.Add => l + r
          case operator.Sub => l - r
          case operator.Mult => l * r
          case operator.Div => l / r
          case operator.Mod =>
            val res = l % r
            if (res < 0) res + r else res
          case operator.LShift => l << r.toInt
          case operator.RShift => l >> r.toInt
          case operator.BitOr => l | r
          case operator.BitXor => l ^ r
          case operator.BitAnd => l & r
        })
        case _ => value.NonConst
      }

    case expr.BoolOp(op, values) =>
      value.Bool(values.foldLeft(true)((acc, right) => {
        val rightValue = evaluate(right) match {
          case value.Bool(x) => x
          case _ => return value.NonConst
        }
        op match {
          case boolop.And => acc && rightValue
          case boolop.Or => acc || rightValue
        }
      }))

    case expr.Compare(left, op, right) =>
      val leftValue = evaluate(left)
      val rightValue = evaluate(right)
      value.Bool((op, leftValue, rightValue) match {
        case (cmpop.Eq, value.Int(l), value.Int(r) ) => l == r
        case (cmpop.Eq, value.Bool(l), value.Bool(r)) => l == r
        case (cmpop.Eq, value.Str(l), value.Str(r)) => l == r

        case (cmpop.NotEq, value.Int(l), value.Int(r) ) => l != r
        case (cmpop.NotEq, value.Bool(l), value.Bool(r)) => l != r
        case (cmpop.NotEq, value.Str(l), value.Str(r)) => l != r

        case (cmpop.Lt,  value.Int(l), value.Int(r)) => l < r
        case (cmpop.LtE, value.Int(l), value.Int(r)) => l <= r
        case (cmpop.Gt,  value.Int(l), value.Int(r)) => l > r
        case (cmpop.GtE, value.Int(l), value.Int(r)) => l >= r

        case (cmpop.Lt,  value.Str(l), value.Str(r)) => l < r
        case (cmpop.LtE, value.Str(l), value.Str(r)) => l <= r
        case (cmpop.Gt,  value.Str(l), value.Str(r)) => l > r
        case (cmpop.GtE, value.Str(l), value.Str(r)) => l >= r

        case _ => return value.NonConst
      })

    case expr.IfExp(condition, ifTrue, ifFalse) => evaluate(condition) match {
      case value.Bool(cond) =>
        if (cond) {
          evaluate(ifTrue)
        } else {
          evaluate(ifFalse)
        }
      case _ => value.NonConst
    }

    case expr.List(list) => value.List(list.map(evaluate))
    case expr.Subscript(container, index) =>
      val idx = evaluate(index) match {
        case value.Int(x) if x >= 0 => x
        case _ => return value.NonConst
      }
      evaluate(container) match {
        case value.List(list) if idx < list.length => list(idx.toInt)
        case _ => value.NonConst
      }

    case _ => value.NonConst
  }

  /**
    * Result of the AST evaluation.
    *
    * Represents either a known-to-be constant value of certain type, or knowledge that this
    * expression is non-constant.
    * */
  sealed trait value
  object value {
    /** Result known to potentially non-constant */
    case object NonConst extends value

    /** AST node evaluated to the logical value */
    case class Bool(value: Boolean) extends value
    /** AST node evaluated to the numerical value */
    case class Int(value: BigInt) extends value
    /** AST node evaluated to the string value */
    case class Str(value: String) extends value
    /** AST node evaluated to the array */
    case class List(list: Seq[value]) extends value
  }
}
