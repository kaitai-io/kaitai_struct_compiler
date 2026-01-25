package io.kaitai.struct.exprlang

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class EnumRefSpec extends AnyFunSpec {
  describe("Expressions.parseEnumRef") {
    describe("parses local enum refs") {
      it("some_enum") {
        Expressions.parseEnumRef("some_enum") should be(Ast.EnumRef(
          false, Seq(), "some_enum"
        ))
      }
      it("with spaces: '  some_enum  '") {
        Expressions.parseEnumRef("  some_enum  ") should be(Ast.EnumRef(
          false, Seq(), "some_enum"
        ))
      }

      it("::some_enum") {
        Expressions.parseEnumRef("::some_enum") should be(Ast.EnumRef(
          true, Seq(), "some_enum"
        ))
      }
      it("with spaces: '  ::  some_enum  '") {
        Expressions.parseEnumRef("  ::  some_enum  ") should be(Ast.EnumRef(
          true, Seq(), "some_enum"
        ))
      }
    }

    describe("parses path enum refs") {
      it("some::enum") {
        Expressions.parseEnumRef("some::enum") should be(Ast.EnumRef(
          false, Seq("some"), "enum"
        ))
      }
      it("with spaces: '  some  ::  enum  '") {
        Expressions.parseEnumRef("  some  ::  enum  ") should be(Ast.EnumRef(
          false, Seq("some"), "enum"
        ))
      }

      it("::some::enum") {
        Expressions.parseEnumRef("::some::enum") should be(Ast.EnumRef(
          true, Seq("some"), "enum"
        ))
      }
      it("with spaces: '  ::  some  ::  enum  '") {
        Expressions.parseEnumRef("  ::  some  ::  enum  ") should be(Ast.EnumRef(
          true, Seq("some"), "enum"
        ))
      }
    }
  }
}
