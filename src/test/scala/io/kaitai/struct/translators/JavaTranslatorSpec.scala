package io.kaitai.struct.translators

import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.exprlang.Expressions
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class JavaTranslatorSpec extends FunSpec with BaseTranslatorSpec {
  override def getTranslator(tp: TypeProvider): BaseTranslator = new JavaTranslator(tp)

  describe("JavaTranslator.translate") {
    it("parses single positive integer") {
      tryOne(CalcIntType, "123", "123", CalcIntType)
    }

    it("parses single negative integer") {
      tryOne(CalcIntType, "-456", "-456", CalcIntType)
    }

    it("parses hex integer") {
      tryOne(CalcIntType, "0x1234", "4660", CalcIntType)
    }

    it("parses 1 + 2") {
      tryOne(CalcIntType, "1 + 2", "(1 + 2)", CalcIntType)
    }

    it("parses 3 / 2") {
      tryOne(CalcIntType, "3 / 2", "(3 / 2)", CalcIntType)
    }

    it("parses 1 + 2 + 5") {
      tryOne(CalcIntType, "1 + 2 + 5", "((1 + 2) + 5)", CalcIntType)
    }

    it("parses (1 + 2) / (7 * 8)") {
      tryOne(CalcIntType, "(1 + 2) / (7 * 8)", "((1 + 2) / (7 * 8))", CalcIntType)
    }

    it("parses 1 < 2") {
      tryOne(CalcIntType, "1 < 2", "1 < 2", BooleanType)
    }

    it("parses a[42]") {
      tryOne(ArrayType(CalcStrType), "a[42]", "a().get(42)", CalcStrType)
    }

    it("parses a[42 - 2]") {
      tryOne(ArrayType(CalcStrType), "a[42 - 2]", "a().get((42 - 2))", CalcStrType)
    }

    it("parses 2 < 3 ? \"foo\" : \"bar\"") {
      tryOne(CalcIntType, "2 < 3 ? \"foo\" : \"bar\"", "2 < 3 ? \"foo\" : \"bar\"", CalcStrType)
    }

    it("parses bitwise invert operation") {
      tryOne(CalcIntType, "~777", "~777", CalcIntType)
    }

    it("parses ~(7+3)") {
      tryOne(CalcIntType, "~(7+3)", "~(7 + 3)", CalcIntType)
    }

    it("parses foo of string type") {
      tryOne(CalcStrType, "foo", "foo()", CalcStrType)
    }

    it("parses foo of user type") {
      tryOne(UserTypeInstream("block"), "foo", "foo()", UserTypeInstream("block"))
    }

    class FooBarProvider extends TypeProvider {
      override def determineType(name: String): BaseType = {
        name match {
          case "foo" => UserTypeInstream("block")
        }
      }

      override def determineType(parentType: String, name: String): BaseType = {
        (parentType, name) match {
          case ("block", "bar") => CalcStrType
          case ("block", "inner") => UserTypeInstream("innerblock")
          case ("innerblock", "baz") => CalcIntType
        }
      }
    }

    it("parses foo.bar") {
      tryOne(new FooBarProvider, "foo.bar", "foo().bar()", CalcStrType)
    }

    it("parses foo.inner.baz") {
      tryOne(new FooBarProvider, "foo.inner.baz", "foo().inner().baz()", CalcIntType)
    }

    it("parses _root.foo") {
      tryOne(UserTypeInstream("block"), "_root.foo", "_root.foo()", UserTypeInstream("block"))
    }
  }
}
