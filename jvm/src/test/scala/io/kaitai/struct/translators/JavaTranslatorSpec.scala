package io.kaitai.struct.translators

import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.exprlang.Expressions
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class JavaTranslatorSpec extends FunSpec with BaseTranslatorSpec {
  override def getTranslator(tp: TypeProvider): BaseTranslator = new JavaTranslator(tp)

  describe("JavaTranslator.translate") {
    it("parses single positive integer") {
      tryOne("123", "123", CalcIntType)
    }

    it("parses single negative integer") {
      tryOne("-456", "-456", CalcIntType)
    }

    it("parses hex integer") {
      tryOne("0x1234", "4660", CalcIntType)
    }

    it("parses 1 + 2") {
      tryOne("1 + 2", "(1 + 2)", CalcIntType)
    }

    it("parses 3 / 2") {
      tryOne("3 / 2", "(3 / 2)", CalcIntType)
    }

    it("parses 1 + 2 + 5") {
      tryOne("1 + 2 + 5", "((1 + 2) + 5)", CalcIntType)
    }

    it("parses (1 + 2) / (7 * 8)") {
      tryOne("(1 + 2) / (7 * 8)", "((1 + 2) / (7 * 8))", CalcIntType)
    }

    it("parses 1 < 2") {
      tryOne("1 < 2", "1 < 2", BooleanType)
    }

    it("parses a[42]") {
      tryOne(ArrayType(CalcStrType), "a[42]", "a().get(42)", CalcStrType)
    }

    it("parses a[42 - 2]") {
      tryOne(ArrayType(CalcStrType), "a[42 - 2]", "a().get((42 - 2))", CalcStrType)
    }

    it("parses 2 < 3 ? \"foo\" : \"bar\"") {
      tryOne("2 < 3 ? \"foo\" : \"bar\"", "2 < 3 ? \"foo\" : \"bar\"", CalcStrType)
    }

    it("parses bitwise invert operation") {
      tryOne("~777", "~777", CalcIntType)
    }

    it("parses ~(7+3)") {
      tryOne("~(7+3)", "~(7 + 3)", CalcIntType)
    }

    it("parses foo of string type") {
      tryOne(CalcStrType, "foo", "foo()", CalcStrType)
    }

    it("parses foo of user type") {
      tryOne(userType("block"), "foo", "foo()", userType("block"))
    }

    it("parses foo.bar") {
      tryOne(new FooBarProvider, "foo.bar", "foo().bar()", CalcStrType)
    }

    it("parses foo.inner.baz") {
      tryOne(new FooBarProvider, "foo.inner.baz", "foo().inner().baz()", CalcIntType)
    }

    it("parses _root.foo") {
      tryOne(userType("block"), "_root.foo", "_root.foo()", userType("block"))
    }

    it("parses a != 2 and a != 5") {
      tryOne(CalcIntType, "a != 2 and a != 5", "a() != 2 && a() != 5", BooleanType)
    }

    it("parses a.first when a is array") {
      tryOne(ArrayType(CalcIntType), "a.first", "a().get(0)", CalcIntType)
    }

    it("parses a.last when a is array") {
      tryOne(ArrayType(CalcIntType), "a.last", "a().get(a().size() - 1)", CalcIntType)
    }
  }
}
