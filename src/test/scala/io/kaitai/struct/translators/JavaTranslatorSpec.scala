package io.kaitai.struct.translators

import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.exprlang.Expressions
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class JavaTranslatorSpec extends FunSpec {
  case class Always(t: BaseType) extends TypeProvider {
    override def determineType(name: String): BaseType = t
  }

  def tryOne(t: BaseType, srcStr: String, expectedStr: String, expectedType: BaseType): Unit = {
    val e = Expressions.parse(srcStr)
    val tr = new JavaTranslator(Always(t))
    tr.detectType(e) should be(expectedType)
    tr.translate(e) should be(expectedStr)
  }

  describe("JavaTranslator.translate") {
    it("parses single positive integer") {
      tryOne(IntType, "123", "123", IntType)
    }

    it("parses single negative integer") {
      tryOne(IntType, "-456", "-456", IntType)
    }

    it("parses hex integer") {
      tryOne(IntType, "0x1234", "4660", IntType)
    }

    it("parses 1 + 2") {
      tryOne(IntType, "1 + 2", "(1 + 2)", IntType)
    }

    it("parses 1 + 2 + 5") {
      tryOne(IntType, "1 + 2 + 5", "((1 + 2) + 5)", IntType)
    }

    it("parses (1 + 2) / (7 * 8)") {
      tryOne(IntType, "(1 + 2) / (7 * 8)", "((1 + 2) / (7 * 8))", IntType)
    }

    it("parses 1 < 2") {
      tryOne(IntType, "1 < 2", "1 < 2", BooleanType)
    }

    it("parses a[42]") {
      tryOne(ArrayType(StrType), "a[42]", "a().get(42)", StrType)
    }

    it("parses a[42 - 2]") {
      tryOne(ArrayType(StrType), "a[42 - 2]", "a().get((42 - 2))", StrType)
    }

    it("parses 2 < 3 ? \"foo\" : \"bar\"") {
      tryOne(IntType, "2 < 3 ? \"foo\" : \"bar\"", "2 < 3 ? \"foo\" : \"bar\"", StrType)
    }

    it("parses bitwise invert operation") {
      tryOne(IntType, "~777", "~777", IntType)
    }

    it("parses ~(7+3)") {
      tryOne(IntType, "~(7+3)", "~(7 + 3)", IntType)
    }
  }
}
