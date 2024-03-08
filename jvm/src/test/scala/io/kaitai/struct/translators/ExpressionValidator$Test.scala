package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Expressions
import io.kaitai.struct.exprlang.Expressions.ParseException
import io.kaitai.struct.precompile.{MethodNotFoundErrorWithArg, TypeMismatchError, WrongMethodCall}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ExpressionValidator$Test extends AnyFunSpec {
  val alwaysInt = TestTypeProviders.Always(CalcIntType)
  val alwaysIntValidator = new ExpressionValidator(alwaysInt)

  describe("simple literals") {
    describe("valid") {
      it("123") {
        val ex = Expressions.parse("123")
        alwaysIntValidator.validate(ex)
      }

      it("123.456e12") {
        val ex = Expressions.parse("123.456e12")
        alwaysIntValidator.validate(ex)
      }

      it("\"foo\"") {
        val ex = Expressions.parse("\"foo\"")
        alwaysIntValidator.validate(ex)
      }
    }
  }

  describe("integer methods") {
    describe("valid") {
      it("123.to_s") {
        val ex = Expressions.parse("123.to_s")
        alwaysIntValidator.validate(ex)
      }

      it("123.to_s()") {
        val ex = Expressions.parse("123.to_s()")
        alwaysIntValidator.validate(ex)
      }
    }

    describe("broken") {
      it("123.to_s(3)") {
        val ex = Expressions.parse("123.to_s(3)")
        val thrown = the[WrongMethodCall] thrownBy alwaysIntValidator.validate(ex)
        thrown.getMessage should be("wrong arguments to method call `to_s` on integer: expected (), got (IntNum(3))")
      }

      it("123.unknown_method") {
        val ex = Expressions.parse("123.unknown_method")
        val thrown = the[MethodNotFoundErrorWithArg] thrownBy alwaysIntValidator.validate(ex)
        thrown.getMessage should be("don't know how to call method 'unknown_method' of object type 'integer'")
      }

      it("123.unknown_method_with_param(true)") {
        val ex = Expressions.parse("123.unknown_method_with_param(true)")
        val thrown = the[MethodNotFoundErrorWithArg] thrownBy alwaysIntValidator.validate(ex)
        thrown.getMessage should be("don't know how to call method 'unknown_method_with_param' of object type 'integer'")
      }
    }
  }

  describe("float methods") {
    describe("valid") {
      it("1.234.to_i") {
        val ex = Expressions.parse("1.234.to_i")
        alwaysIntValidator.validate(ex)
      }
    }

    describe("broken") {
      it("1.234.unknown_method") {
        val ex = Expressions.parse("1.234.unknown_method")
        val thrown = the[MethodNotFoundErrorWithArg] thrownBy alwaysIntValidator.validate(ex)
        thrown.getMessage should be("don't know how to call method 'unknown_method' of object type 'float'")
      }

      it("1.234.unknown_method_with_param(true)") {
        val ex = Expressions.parse("1.234.unknown_method_with_param(true)")
        val thrown = the[MethodNotFoundErrorWithArg] thrownBy alwaysIntValidator.validate(ex)
        thrown.getMessage should be("don't know how to call method 'unknown_method_with_param' of object type 'float'")
      }
    }
  }

  describe("string methods") {
    it("\"123\".to_i") {
      val ex = Expressions.parse("\"123\".to_i")
      alwaysIntValidator.validate(ex)
    }

    it("\"123\".to_i()") {
      val ex = Expressions.parse("\"123\".to_i()")
      alwaysIntValidator.validate(ex)
    }

    it("\"123\".to_i(16)") {
      val ex = Expressions.parse("\"123\".to_i(16)")
      alwaysIntValidator.validate(ex)
    }

    it("\"123\".to_i(true)") {
      val ex = Expressions.parse("\"123\".to_i(true)")
      val thrown = the [WrongMethodCall] thrownBy alwaysIntValidator.validate(ex)
      thrown.getMessage should be("wrong arguments to method call `to_i` on string: expected () or (integer), got (Bool(true))")
    }

    it("\"123\".to_i(16, true)") {
      val ex = Expressions.parse("\"123\".to_i(16, true)")
      val thrown = the [WrongMethodCall] thrownBy alwaysIntValidator.validate(ex)
      thrown.getMessage should be("wrong arguments to method call `to_i` on string: expected () or (integer), got (IntNum(16), Bool(true))")
    }

    it("\"foobar\".substring(2, 3)") {
      val ex = Expressions.parse("\"foobar\".substring(2, 3)")
      alwaysIntValidator.validate(ex)
    }

    it("\"foobar\".substring(2, 3, 5)") {
      val ex = Expressions.parse("\"foobar\".substring(2, 3, 5)")
      val thrown = the [WrongMethodCall] thrownBy alwaysIntValidator.validate(ex)
      thrown.getMessage should be("wrong arguments to method call `substring` on string: expected (integer, integer), got (IntNum(2), IntNum(3), IntNum(5))")
    }

    it("\"foobar\".substring(\"foo\", 5)") {
      val ex = Expressions.parse("\"foobar\".substring(\"foo\", 5)")
      val thrown = the [WrongMethodCall] thrownBy alwaysIntValidator.validate(ex)
      thrown.getMessage should be("wrong arguments to method call `substring` on string: expected (integer, integer), got (Str(foo), IntNum(5))")
    }
  }

  describe("array methods") {
    describe("valid") {
      it("[\"foo\", \"bar\"].size") {
        val ex = Expressions.parse("[\"foo\", \"bar\"].size")
        alwaysIntValidator.validate(ex)
      }

      it("[\"foo\", \"bar\"].min") {
        val ex = Expressions.parse("[\"foo\", \"bar\"].min")
        alwaysIntValidator.validate(ex)
      }

      it("[\"foo\", \"bar\"].min()") {
        val ex = Expressions.parse("[\"foo\", \"bar\"].min")
        alwaysIntValidator.validate(ex)
      }
    }

    describe("broken") {
      it("[\"foo\", \"bar\"].min(42)") {
        val ex = Expressions.parse("[\"foo\", \"bar\"].min(42)")
        val thrown = the[WrongMethodCall] thrownBy alwaysIntValidator.validate(ex)
        thrown.getMessage should be("wrong arguments to method call `min` on array: expected (), got (IntNum(42))")
      }
    }
  }

  describe("subscripts") {
    it("[1, 3, 14][2]") {
      val ex = Expressions.parse("[1, 3, 14][2]")
      alwaysIntValidator.validate(ex)
    }

    it("[1, 3, 14][\"foo\"]") {
      val ex = Expressions.parse("[1, 3, 14][\"foo\"]")
      val thrown = the [TypeMismatchError] thrownBy alwaysIntValidator.validate(ex)
      thrown.getMessage should be("subscript operation on arrays require index to be integer, but found CalcStrType")
    }

    it("x[4]") {
      val ex = Expressions.parse("x[4]")
      val thrown = the [TypeMismatchError] thrownBy alwaysIntValidator.validate(ex)
      thrown.getMessage should be("subscript operation is not supported on object type CalcIntType")
    }
  }
}
