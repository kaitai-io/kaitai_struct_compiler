package io.kaitai.struct.languages

import io.kaitai.struct.format.{Identifier, NamedIdentifier, InstanceIdentifier}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class PythonCompilerSpec extends AnyFunSpec {
  describe("PythonCompiler") {
    describe("reserved keyword escaping") {
      it("should escape Python reserved keyword 'class'") {
        val id = NamedIdentifier("class")
        val result = PythonCompiler.idToStr(id)
        result should be("class_")
      }

      it("should escape Python reserved keyword 'def'") {
        val id = NamedIdentifier("def")
        val result = PythonCompiler.idToStr(id)
        result should be("def_")
      }

      it("should escape Python reserved keyword 'if'") {
        val id = NamedIdentifier("if")
        val result = PythonCompiler.idToStr(id)
        result should be("if_")
      }

      it("should escape Python reserved keyword 'lambda'") {
        val id = NamedIdentifier("lambda")
        val result = PythonCompiler.idToStr(id)
        result should be("lambda_")
      }

      it("should escape Python reserved keyword 'return'") {
        val id = NamedIdentifier("return")
        val result = PythonCompiler.idToStr(id)
        result should be("return_")
      }

      it("should escape Python reserved keyword 'async'") {
        val id = NamedIdentifier("async")
        val result = PythonCompiler.idToStr(id)
        result should be("async_")
      }

      it("should escape Python reserved keyword 'await'") {
        val id = NamedIdentifier("await")
        val result = PythonCompiler.idToStr(id)
        result should be("await_")
      }

      it("should not escape non-reserved word 'class_name'") {
        val id = NamedIdentifier("class_name")
        val result = PythonCompiler.idToStr(id)
        result should be("class_name")
      }

      it("should not escape non-reserved word 'my_field'") {
        val id = NamedIdentifier("my_field")
        val result = PythonCompiler.idToStr(id)
        result should be("my_field")
      }

      it("should escape reserved keyword in InstanceIdentifier") {
        val id = InstanceIdentifier("class")
        val result = PythonCompiler.idToStr(id)
        result should be("_m_class_")
      }

      it("should handle privateMemberName with reserved keyword") {
        val id = NamedIdentifier("class")
        val result = PythonCompiler.privateMemberName(id)
        result should be("self.class_")
      }

      it("should handle privateMemberName with normal identifier") {
        val id = NamedIdentifier("normal_field")
        val result = PythonCompiler.privateMemberName(id)
        result should be("self.normal_field")
      }
    }
  }
}
