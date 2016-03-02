package io.kaitai.struct.exprlang

import io.kaitai.struct.exprlang.Ast._
import io.kaitai.struct.exprlang.Ast.expr._
import io.kaitai.struct.exprlang.Ast.operator._
import io.kaitai.struct.exprlang.Ast.cmpop._
import io.kaitai.struct.exprlang.Ast.unaryop._
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class ExpressionsSpec extends FunSpec {
  describe("Expressions.parse") {
    it("parses single positive integer") {
      Expressions.parse("123") should be (Num(123))
    }

    it("parses single negative integer") {
      Expressions.parse("-456") should be (UnaryOp(Minus, Num(456)))
    }

    it("parses hex integer") {
      Expressions.parse("0x1234") should be (Num(0x1234))
    }

    it("parses 1 + 2") {
      Expressions.parse("1 + 2") should be (BinOp(Num(1), Add, Num(2)))
    }

    it("parses 1 + 2 + 5") {
      Expressions.parse("1 + 2 + 5") should be (
        BinOp(BinOp(Num(1), Add, Num(2)), Add, Num(5))
      )
    }

    it("parses (1 + 2) / (7 * 8)") {
      Expressions.parse("(1 + 2) / (7 * 8)") should be (
        BinOp(
          BinOp(Num(1), Add, Num(2)),
          Div,
          BinOp(Num(7), Mult, Num(8))
        )
      )
    }

    it("parses 1 < 2") {
      Expressions.parse("1 < 2") should be (Compare(Num(1), Lt, Num(2)))
    }

    it("parses a[42]") {
      Expressions.parse("a[42]") should be (Subscript(Name(identifier("a")), Num(42)))
    }

    it("parses a[42 - 2]") {
      Expressions.parse("a[42 - 2]") should be (
        Subscript(
          Name(identifier("a")),
          BinOp(Num(42), Sub, Num(2))
        )
      )
    }

    it("parses 2 < 3 ? \"foo\" : \"bar\"") {
      Expressions.parse("2 < 3 ? \"foo\" : \"bar\"") should be (
        IfExp(
          Compare(Num(2), Lt, Num(3)),
          Str("foo"),
          Str("bar")
        )
      )
    }

    it("parses bitwise invert operation") {
      Expressions.parse("~777") should be (UnaryOp(Invert, Num(777)))
    }

    it("parses ~(7+3)") {
      Expressions.parse("~(7+3)") should be (UnaryOp(Invert, BinOp(Num(7), Add, Num(3))))
    }
  }
}
