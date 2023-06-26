package io.kaitai.struct.exprlang

import io.kaitai.struct.exprlang.Ast._
import io.kaitai.struct.exprlang.Ast.expr.{BinOp, IntNum, List, Name}
import io.kaitai.struct.exprlang.Ast.identifier
import io.kaitai.struct.exprlang.Ast.operator.Add
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class TypesRefSpec extends AnyFunSpec {
  describe("Expressions.parseTypeRef") {
    describe("parses local type refs") {
      it("some_type") {
        Expressions.parseTypeRef("some_type") should be(Ast.TypeWithArguments(
          typeId(false, Seq("some_type")),
          Ast.expr.List(Seq())
        ))
      }
      it("with spaces: '  some_type  '") {
        Expressions.parseTypeRef("  some_type  ") should be(Ast.TypeWithArguments(
          typeId(false, Seq("some_type")),
          Ast.expr.List(Seq())
        ))
      }
      it("some_type(1+2,data)") {
        Expressions.parseTypeRef("some_type(1+2,data)") should be(Ast.TypeWithArguments(
          typeId(false, Seq("some_type")),
          Ast.expr.List(Seq(
            BinOp(IntNum(1), Add, IntNum(2)),
            Name(identifier("data"))
          ))
        ))
      }
      it("with spaces: ' some_type ( 1 + 2 , data ) '") {
        Expressions.parseTypeRef(" some_type ( 1 + 2 , data ) ") should be(Ast.TypeWithArguments(
          typeId(false, Seq("some_type")),
          Ast.expr.List(Seq(
            BinOp(IntNum(1), Add, IntNum(2)),
            Name(identifier("data"))
          ))
        ))
      }
    }
    describe("parses path type refs") {
      it("some::type") {
        Expressions.parseTypeRef("some::type") should be(Ast.TypeWithArguments(
          typeId(false, Seq("some", "type")),
          Ast.expr.List(Seq())
        ))
      }
      it("with spaces: '  some  ::  type  '") {
        Expressions.parseTypeRef("  some  ::  type  ") should be(Ast.TypeWithArguments(
          typeId(false, Seq("some", "type")),
          Ast.expr.List(Seq())
        ))
      }
      it("some::type(1+2,data)") {
        Expressions.parseTypeRef("some::type(1+2,data)") should be(Ast.TypeWithArguments(
          typeId(false, Seq("some", "type")),
          Ast.expr.List(Seq(
            BinOp(IntNum(1), Add, IntNum(2)),
            Name(identifier("data"))
          ))
        ))
      }
      it("with spaces: ' some :: type ( 1 + 2 , data ) '") {
        Expressions.parseTypeRef(" some :: type ( 1 + 2 , data ) ") should be(Ast.TypeWithArguments(
          typeId(false, Seq("some", "type")),
          Ast.expr.List(Seq(
            BinOp(IntNum(1), Add, IntNum(2)),
            Name(identifier("data"))
          ))
        ))
      }
    }
  }
}
