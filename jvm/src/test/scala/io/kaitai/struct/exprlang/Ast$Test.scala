package io.kaitai.struct.exprlang

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class Ast$Test extends AnyFunSpec {
  describe("Ast.expr.evaluateIntConst") {
    it ("considers `42` constant") {
      Expressions.parse("42").evaluateIntConst should be(Some(42))
    }

    it ("considers `-1` constant") {
      Expressions.parse("-1").evaluateIntConst should be(Some(-1))
    }

    it ("considers `42 - 2` constant") {
      Expressions.parse("42 - 2").evaluateIntConst should be(Some(40))
    }

    it ("considers `(-3 + 7) * 8 / 2` constant") {
      Expressions.parse("(-3 + 7) * 8 / 2").evaluateIntConst should be(Some(16))
    }

    it ("considers `-5 % 3` constant") {
      Expressions.parse("-5 % 3").evaluateIntConst should be(Some(1))
    }

    it ("considers `[3, 1, 4][2]` constant") {
      Expressions.parse("[3, 1, 4][2]").evaluateIntConst should be(Some(4))
    }

    it ("considers `4 > 2 ? 1 : 5` constant") {
      Expressions.parse("4 > 2 ? 1 : 5").evaluateIntConst should be(Some(1))
    }

    it ("considers `x` variable") {
      Expressions.parse("x").evaluateIntConst should be(None)
    }

    it ("considers `[3, 1, 4][x]` variable") {
      Expressions.parse("[3, 1, 4][x]").evaluateIntConst should be(None)
    }

    // The following tests probably can be improved by some point to deliver
    // constant result by introduction of more complex symbolic execution engine.
    it ("considers `x - x` non-constant") {
      Expressions.parse("x - x").evaluateIntConst should be(None) // be(Some(0))
    }

    it ("considers `(x + 1) * (x + 1) - (x * x + 2 * x + 2)` non-constant") {
      Expressions.parse("(x + 1) * (x + 1) - (x * x + 2 * x + 2)").evaluateIntConst should be(None) // be(Some(0))
    }
  }
}
