package io.kaitai.struct

import scala.collection.mutable.ListBuffer

/*
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader

trait ExpressionSyntax {
  sealed abstract class Expression

  case class IntegerLiteral(i: Int) extends Expression
  case class Sum(e1: Expression, e2: Expression) extends Expression
  case class Product(e1: Expression, e2: Expression) extends Expression
}

trait ExpressionParsers extends RegexParsers with ExpressionSyntax {
  def integer:Parser[IntegerLiteral] = {
    """-?\d+""".r ^^ {
      s => new IntegerLiteral(s.toInt)
    }
  }

  def sum:Parser[Sum] = operand ~ "+" ~ operand ^^ {
    case (x ~ "+" ~ y) => Sum(x, y)
  }

  def product:Parser[Product] = operand ~ "*" ~ operand ^^ {
    case (x ~ "*" ~ y) => Product(x, y)
  }

  def parenthesizedExpression = "(" ~> expression <~ ")"

  def operand = (integer | parenthesizedExpression)

  def expression:Parser[Expression] = ( sum | product | integer | parenthesizedExpression )
}

trait SimpleParsers extends RegexParsers with ExpressionSyntax {
  def integer:Parser[IntegerLiteral] = {
    """-?\d+""".r ^^ {
      s => new IntegerLiteral(s.toInt)
    }
  }
  def binaryOperator: Parser[String] = {
    """[+-/*]""".r ^^ {
      s => s
    }
  }

  def term = (integer | binaryOperator)
  def expression: Parser[Expression] = term
}

object ExpressionParsers extends ExpressionParsers {
  def parseExpression(s: CharSequence): Expression = {
    parseExpression(new CharSequenceReader(s))
  }

  def parseExpression(input: CharSequenceReader): ExpressionParsers.Expression = {
    parsePhrase(input) match {
      case Success(t, _) => t
      case NoSuccess(msg, next) => throw new IllegalArgumentException(
        "Could not parse '" + input + "' near '" + next.pos.longString + ": " + msg)
    }
  }

  def parsePhrase(input: CharSequenceReader): ParseResult[Expression] = {
    phrase(expression)(input)
  }

  def transform(e: Expression): String = e match {
    case IntegerLiteral(i) => i.toString
    case Sum(e1, e2) => transform(e1) + " + " + transform(e2)
    case Product(e1, e2) => transform(e1) + " * " + transform(e2)
    case _ => 0
  }
}
*/
*/

//object MyFiddle {
//  def myMultiOutputMap[T, R](coll: TraversableOnce[T], func: (T, ListBuffer[R]) => Unit): List[R] = {
//    val out = ListBuffer[R]()
//    coll.foreach((x) => func.apply(x, out))
//    out.toList
//  }

//  def main(args : Array[String]): Unit = {
//    Console.println(ExpressionParsers.parseExpression("(2 + 2) * 3"))

//    val input = Range(0, 15)
    //val output = ListBuffer[String]()
    //    input.foreach((x) => {
    //      if (x % 3 == 0)
    //        output += s"${x}/3"
    //      if (x % 5 == 0)
    //        output += s"${x}/5"
    //    })

//    val output = myMultiOutputMap[Int, String](input, (x, out) => {
//      if (x % 3 == 0)
//        out += s"${x}/3"
//      if (x % 5 == 0)
//        out += s"${x}/5"
//    })

    //    val output = input.flatMap((x) => {
//      if (x % 3 == 0) {
//        if (x % 5 == 0) {
//          List(s"${x}/3", s"${x}/5")
//        } else {
//          List(s"${x}/3")
//        }
//      } else if (x % 5 == 0) {
//        List(s"${x}/5")
//      } else {
//        List()
//      }
//    })

    //    val output = input.flatMap((x) => {
//      val r = ListBuffer[String]()
//      if (x % 3 == 0)
//        r += s"${x}/3"
//      if (x % 5 == 0)
//        r += s"${x}/5"
//      r
//    })

//    val output = input.flatMap((x) => {
//      val v1 = if (x % 3 == 0) {
//        Some(s"${x}/3")
//      } else {
//        None
//      }
//      val v2 = if (x % 5 == 0) {
//        Some(s"${x}/5")
//      } else {
//        None
//      }
//      List(v1, v2).flatten
//    })
//    Console.println(output)
//  }
//}

object MyFiddle {
  type Identifier = String

  def doWithIdentifier(a: Identifier): Unit = {

  }

  def main(args : Array[String]): Unit = {
    val incoming = "Incoming"
    val i1: Identifier = new Identifier("a")

    val v1: Byte = -42
  }
}
