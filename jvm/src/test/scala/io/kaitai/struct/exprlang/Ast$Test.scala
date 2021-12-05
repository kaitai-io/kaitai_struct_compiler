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

  describe("Ast.expr.evaluateBoolConst") {
    it ("considers `true` constant") {
      Expressions.parse("true").evaluateBoolConst should be(Some(true))
    }

    it ("considers `false` constant") {
      Expressions.parse("false").evaluateBoolConst should be(Some(false))
    }

    it ("considers `false and ?` constant") {
      Expressions.parse("false and false").evaluateBoolConst should be(Some(false))
      Expressions.parse("false and true").evaluateBoolConst should be(Some(false))
      Expressions.parse("false and x").evaluateBoolConst should be(Some(false))
      Expressions.parse("false and (1==1)").evaluateBoolConst should be(Some(false))
    }

    it ("considers `? and false` constant") {
      Expressions.parse("false and false").evaluateBoolConst should be(Some(false))
      Expressions.parse("true and false").evaluateBoolConst should be(Some(false))
      Expressions.parse("x and false").evaluateBoolConst should be(Some(false))
      Expressions.parse("(1==1) and false").evaluateBoolConst should be(Some(false))
    }

    it ("considers `true or ?` constant") {
      Expressions.parse("true or false" ).evaluateBoolConst should be(Some(true))
      Expressions.parse("true or true"  ).evaluateBoolConst should be(Some(true))
      Expressions.parse("true or x"     ).evaluateBoolConst should be(Some(true))
      Expressions.parse("true or (1==1)").evaluateBoolConst should be(Some(true))
    }

    it ("considers `? or true` constant") {
      Expressions.parse("false  or true").evaluateBoolConst should be(Some(true))
      Expressions.parse("true   or true").evaluateBoolConst should be(Some(true))
      Expressions.parse("x      or true").evaluateBoolConst should be(Some(true))
      Expressions.parse("(1==1) or true").evaluateBoolConst should be(Some(true))
    }

    it ("evaluates `? == ?`") {
      Expressions.parse("true  == true" ).evaluateBoolConst should be(Some(true))
      Expressions.parse("false == false").evaluateBoolConst should be(Some(true))
      Expressions.parse("42    == 42"   ).evaluateBoolConst should be(Some(true))
      Expressions.parse("field == field").evaluateBoolConst should be(None)//(Some(true))//TODO: symbolic calculations

      Expressions.parse("true  == false").evaluateBoolConst should be(Some(false))
      Expressions.parse("false == true" ).evaluateBoolConst should be(Some(false))
      Expressions.parse("42    == 420"  ).evaluateBoolConst should be(Some(false))
      Expressions.parse("field == other").evaluateBoolConst should be(None)
    }

    it ("evaluates `? != ?`") {
      Expressions.parse("true  != true" ).evaluateBoolConst should be(Some(false))
      Expressions.parse("false != false").evaluateBoolConst should be(Some(false))
      Expressions.parse("42    != 42"   ).evaluateBoolConst should be(Some(false))
      Expressions.parse("field != field").evaluateBoolConst should be(None)//(Some(false))//TODO: symbolic calculations

      Expressions.parse("true  != false").evaluateBoolConst should be(Some(true))
      Expressions.parse("false != true" ).evaluateBoolConst should be(Some(true))
      Expressions.parse("42    != 420"  ).evaluateBoolConst should be(Some(true))
      Expressions.parse("field != other").evaluateBoolConst should be(None)
    }

    it ("evaluates `? < ?`") {
      Expressions.parse("42 < 10").evaluateBoolConst should be(Some(false))
      Expressions.parse("42 < 42").evaluateBoolConst should be(Some(false))
      Expressions.parse("42 < 99").evaluateBoolConst should be(Some(true))
      Expressions.parse("42 < xx").evaluateBoolConst should be(None)
      Expressions.parse("xx < xx").evaluateBoolConst should be(None)//(Some(false))//TODO: symbolic calculations
      Expressions.parse("xx < yy").evaluateBoolConst should be(None)

      Expressions.parse("10 < 42").evaluateBoolConst should be(Some(true))
      Expressions.parse("42 < 42").evaluateBoolConst should be(Some(false))
      Expressions.parse("99 < 42").evaluateBoolConst should be(Some(false))
      Expressions.parse("xx < 42").evaluateBoolConst should be(None)
    }

    it ("evaluates `? <= ?`") {
      Expressions.parse("42 <= 10").evaluateBoolConst should be(Some(false))
      Expressions.parse("42 <= 42").evaluateBoolConst should be(Some(true))
      Expressions.parse("42 <= 99").evaluateBoolConst should be(Some(true))
      Expressions.parse("42 <= xx").evaluateBoolConst should be(None)
      Expressions.parse("xx <= xx").evaluateBoolConst should be(None)//(Some(true))//TODO: symbolic calculations
      Expressions.parse("xx <= yy").evaluateBoolConst should be(None)

      Expressions.parse("10 <= 42").evaluateBoolConst should be(Some(true))
      Expressions.parse("42 <= 42").evaluateBoolConst should be(Some(true))
      Expressions.parse("99 <= 42").evaluateBoolConst should be(Some(false))
      Expressions.parse("xx <= 42").evaluateBoolConst should be(None)
    }

    it ("evaluates `? > ?`") {
      Expressions.parse("42 > 10").evaluateBoolConst should be(Some(true))
      Expressions.parse("42 > 42").evaluateBoolConst should be(Some(false))
      Expressions.parse("42 > 99").evaluateBoolConst should be(Some(false))
      Expressions.parse("42 > xx").evaluateBoolConst should be(None)
      Expressions.parse("xx > xx").evaluateBoolConst should be(None)//(Some(false))//TODO: symbolic calculations
      Expressions.parse("xx > yy").evaluateBoolConst should be(None)

      Expressions.parse("10 > 42").evaluateBoolConst should be(Some(false))
      Expressions.parse("42 > 42").evaluateBoolConst should be(Some(false))
      Expressions.parse("99 > 42").evaluateBoolConst should be(Some(true))
      Expressions.parse("xx > 42").evaluateBoolConst should be(None)
    }

    it ("evaluates `? >= ?`") {
      Expressions.parse("42 >= 10").evaluateBoolConst should be(Some(true))
      Expressions.parse("42 >= 42").evaluateBoolConst should be(Some(true))
      Expressions.parse("42 >= 99").evaluateBoolConst should be(Some(false))
      Expressions.parse("42 >= xx").evaluateBoolConst should be(None)
      Expressions.parse("xx >= xx").evaluateBoolConst should be(None)//(Some(true))//TODO: symbolic calculations
      Expressions.parse("xx >= yy").evaluateBoolConst should be(None)

      Expressions.parse("10 >= 42").evaluateBoolConst should be(Some(false))
      Expressions.parse("42 >= 42").evaluateBoolConst should be(Some(true))
      Expressions.parse("99 >= 42").evaluateBoolConst should be(Some(true))
      Expressions.parse("xx >= 42").evaluateBoolConst should be(None)
    }

    it ("considers `[true, false, 7==7][2]` constant") {
      Expressions.parse("[true, false, 7==7][2]").evaluateBoolConst should be(Some(true))
    }

    it ("considers `4 > 2 ? true : false` constant") {
      Expressions.parse("4 > 2 ? true : false").evaluateBoolConst should be(Some(true))
    }

    it ("considers `x` variable") {
      Expressions.parse("x").evaluateBoolConst should be(None)
    }

    it ("considers `[true, false, 7==7][x]` variable") {
      Expressions.parse("[true, false, 7==7][x]").evaluateBoolConst should be(None)
    }
  }
}
