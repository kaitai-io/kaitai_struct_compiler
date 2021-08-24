package io.kaitai.struct.exprlang

import io.kaitai.struct.exprlang.Ast.expr.List

/**
  * Loosely based on /pythonparse/shared/src/main/scala/pythonparse/
  * from FastParse, Copyright (c) 2014 Li Haoyi (haoyi.sg@gmail.com)
  * https://com-lihaoyi.github.io/fastparse/
  *
  * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
  * documentation files (the "Software"), to deal in the Software without restriction, including without limitation
  * the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and
  * to permit persons to whom the Software is furnished to do so, subject to the following conditions:
  *
  * The above copyright notice and this permission notice shall be included in all copies or substantial portions of
  * the Software.
  *
  * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
  * THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
  * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
  * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  * IN THE SOFTWARE.
  */
object Ast {
  case class identifier(name: String)
  case class typeId(absolute: Boolean, names: Seq[String], isArray: Boolean = false) {
    /**
      * @return Type designation name as human-readable string, to be used in compiler
      *         error messages.
      */
    def nameAsStr: String =
      (if (absolute) "::" else "") +
        names.mkString("::") +
        (if (isArray) "[]" else "")
  }

  val EmptyTypeId = typeId(false, Seq())

  // BoolOp() can use left & right?
  sealed trait expr {
    /**
      * Evaluates the expression, if it's possible to get a static integer
      * constant as the result of evaluation (i.e. if it does not involve any
      * variables or anything like that). Expect no complex logic or symbolic
      * simplification of expressions here: something like "x - x", which is
      * known to be always 0, will still report it as "None".
      *
      * @return integer result of evaluation if it's constant or None, if it's
      *         variable
      */
    def evaluateIntConst: Option[BigInt] = {
      this.evaluate match {
        case Some(value.Int(x)) => Some(x)
        case _ => None
      }
    }
    def evaluate: Option[value] = this match {
      case expr.IntNum(x) => Some(value.Int(x))
      case expr.Bool(x) => Some(value.Bool(x))
      case expr.Str(x) => Some(value.Str(x))

      case expr.UnaryOp(op, expr.IntNum(operand)) =>
        Some(value.Int(op match {
          case unaryop.Invert => ~operand
          case unaryop.Not => return None
          case unaryop.Minus  => -operand
        }))
      case expr.UnaryOp(unaryop.Not, expr.Bool(operand)) => Some(value.Bool(!operand))

      case expr.BinOp(left, op, right) =>
        val leftValue = left.evaluate match {
          case Some(x) => x
          case _ => return None
        }
        val rightValue = right.evaluate match {
          case Some(x) => x
          case _ => return None
        }
        Some((op, leftValue, rightValue) match {
          case (operator.Add, value.Str(l), value.Str(r)) => value.Str(l + r)

          case (operator.Add, value.Int(l), value.Int(r)) => value.Int(l + r)
          case (operator.Sub, value.Int(l), value.Int(r)) => value.Int(l - r)
          case (operator.Mult, value.Int(l), value.Int(r)) => value.Int(l * r)
          case (operator.Div, value.Int(l), value.Int(r)) => value.Int(l / r)
          case (operator.Mod, value.Int(l), value.Int(r)) => value.Int(l % r)
          case (operator.LShift, value.Int(l), value.Int(r)) => value.Int(l << r.toInt)
          case (operator.RShift, value.Int(l), value.Int(r)) => value.Int(l >> r.toInt)
          case (operator.BitOr, value.Int(l), value.Int(r)) => value.Int(l | r)
          case (operator.BitXor, value.Int(l), value.Int(r)) => value.Int(l ^ r)
          case (operator.BitAnd, value.Int(l), value.Int(r)) => value.Int(l & r)

          case _ => return None
        })

      case expr.BoolOp(op, values) =>
        Some(value.Bool(values.foldLeft(true)((acc, right) => {
          val rightValue = right.evaluate match {
            case Some(value.Bool(x)) => x
            case _ => return None;
          }
          op match {
            case boolop.And => acc && rightValue
            case boolop.Or  => acc || rightValue
          }
        })))

      case expr.Compare(left, op, right) =>
        val leftValue = left.evaluate match {
          case Some(x) => x
          case _ => return None
        }
        val rightValue = right.evaluate match {
          case Some(x) => x
          case _ => return None
        }
        Some(value.Bool((op, leftValue, rightValue) match {
          case (cmpop.Eq, value.Int(l),  value.Int(r) ) => l == r
          case (cmpop.Eq, value.Bool(l), value.Bool(r)) => l == r
          case (cmpop.Eq, value.Str(l),  value.Str(r)) => l == r

          case (cmpop.NotEq, value.Int(l),  value.Int(r) ) => l != r
          case (cmpop.NotEq, value.Bool(l), value.Bool(r)) => l != r
          case (cmpop.NotEq, value.Str(l),  value.Str(r)) => l != r

          case (cmpop.Lt,  value.Int(l), value.Int(r)) => l < r
          case (cmpop.LtE, value.Int(l), value.Int(r)) => l <= r
          case (cmpop.Gt,  value.Int(l), value.Int(r)) => l > r
          case (cmpop.GtE, value.Int(l), value.Int(r)) => l >= r

          case (cmpop.Lt,  value.Str(l), value.Str(r)) => l < r
          case (cmpop.LtE, value.Str(l), value.Str(r)) => l <= r
          case (cmpop.Gt,  value.Str(l), value.Str(r)) => l > r
          case (cmpop.GtE, value.Str(l), value.Str(r)) => l >= r

          case _ => return None
        }))

      case expr.IfExp(condition, ifTrue, ifFalse) => condition.evaluate match {
        case Some(value.Bool(cond)) =>
          if (cond) {
            ifTrue.evaluate
          } else {
            ifFalse.evaluate
          }
        case _ => return None
      }

      case expr.List(list) => Some(value.List(list.map(_.evaluate)))
      case expr.Subscript(container, index) =>
        val idx = index.evaluate match {
          case Some(value.Int(x)) if x >= 0 => x
          case _ => return None
        }
        container.evaluate match {
          case Some(value.List(list)) if idx < list.length => list(idx.toInt)
          case _ => None
        }

      case _ => None
    }
  }

  object expr{
    case class BoolOp(op: boolop, values: Seq[expr]) extends expr
    case class BinOp(left: expr, op: operator, right: expr) extends expr
    case class UnaryOp(op: unaryop, operand: expr) extends expr
    case class IfExp(condition: expr, ifTrue: expr, ifFalse: expr) extends expr
    // case class Dict(keys: Seq[expr], values: Seq[expr]) extends expr
    /** Represents `X < Y`, `X > Y` and so on. */
    case class Compare(left: expr, ops: cmpop, right: expr) extends expr
    case class Call(func: expr, args: Seq[expr]) extends expr
    case class IntNum(n: BigInt) extends expr
    case class FloatNum(n: BigDecimal) extends expr
    case class Str(s: String) extends expr
    case class Bool(n: Boolean) extends expr
    case class EnumByLabel(enumName: identifier, label: identifier, inType: typeId = EmptyTypeId) extends expr
    case class EnumById(enumName: identifier, id: expr, inType: typeId = EmptyTypeId) extends expr

    case class Attribute(value: expr, attr: identifier) extends expr
    case class CastToType(value: expr, typeName: typeId) extends expr
    case class ByteSizeOfType(typeName: typeId) extends expr
    case class BitSizeOfType(typeName: typeId) extends expr
    /** Represents `X[Y]`. */
    case class Subscript(value: expr, idx: expr) extends expr
    case class Name(id: identifier) extends expr
    case class List(elts: Seq[expr]) extends expr
  }

  sealed trait boolop
  object boolop {
    /** Boolean conjunction. Applicable only to `Boolean`s */
    case object And extends boolop
    /** Boolean disjunction. Applicable only to `Boolean`s */
    case object Or extends boolop
  }

  sealed trait operator
  case object operator {
    case object Add extends operator
    case object Sub  extends operator
    case object Mult  extends operator
    case object Div  extends operator
    case object Mod  extends operator
    // case object Pow  extends operator
    case object LShift  extends operator
    case object RShift  extends operator
    case object BitOr  extends operator
    case object BitXor  extends operator
    case object BitAnd  extends operator
  }

  sealed trait unaryop
  object unaryop {
    /** Bitwise negation operator. Applicable only to `IntNum`s */
    case object Invert extends unaryop
    /** Boolean negation operator. Applicable only to `Boolean`s */
    case object Not extends unaryop
    /** Arithmetic negation operator. Applicable only to `IntNum`s / `FloatNum`s */
    case object Minus extends unaryop
  }

  sealed trait cmpop
  object cmpop {
    case object Eq extends cmpop
    case object NotEq extends cmpop
    case object Lt extends cmpop
    case object LtE extends cmpop
    case object Gt extends cmpop
    case object GtE extends cmpop
  }

  /** Result of the AST evaluation */
  sealed trait value
  object value {
    /** AST node evaluated to the logical value */
    case class Bool(value: Boolean) extends value
    /** AST node evaluated to the numerical value */
    case class Int(value: BigInt) extends value
    /** AST node evaluated to the string value */
    case class Str(value: String) extends value
    /** AST node evaluated to the array */
    case class List(list: Seq[Option[value]]) extends value
  }

  case class TypeWithArguments(typeName: typeId, arguments: expr.List)
}
