package io.kaitai.struct.exprlang

import io.kaitai.struct.format.{Identifier, InstanceIdentifier}

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
    def evaluateIntConst: Option[BigInt] = ConstEvaluator.evaluateIntConst(this)

    /**
      * Evaluates the expression, if it's possible to get a string constant as
      * the result of evaluation.
      *
      * @return string result of evaluation if it's constant or None, if it's
      *         variable
      */
    def evaluateStrConst: Option[String] = ConstEvaluator.evaluate(this) match {
      case ConstEvaluator.value.Str(x) => Some(x)
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
    /** For internal use in the compiler. It cannot appear in an AST parsed from a user-supplied string. */
    case class InternalName(id: Identifier) extends expr
    case class List(elts: Seq[expr]) extends expr
    case class InterpolatedStr(elts: Seq[expr]) extends expr

    /**
     * Implicit declaration of ordering, so expressions can be used for ordering operations, e.g.
     * for `SortedMap.from(...)`
     */
    implicit val ExprIdentifierOrdering: Ordering[expr] = Ordering.by(_.toString)
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

  case class TypeWithArguments(typeName: typeId, arguments: expr.List)
}
