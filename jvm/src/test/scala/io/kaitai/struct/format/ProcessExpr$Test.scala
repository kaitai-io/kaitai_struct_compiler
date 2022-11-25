package io.kaitai.struct.format

import io.kaitai.struct.exprlang.Ast
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ProcessExpr$Test extends AnyFunSpec {
  describe("ProcessExpr.fromStr") {
    it("foo(5)") {
      ProcessExpr.fromStr(Some("foo(5)"), List("some", "path")) should be(Some(ProcessCustom(
        List("foo"),
        Seq(Ast.expr.IntNum(5))
      )))
    }

    it("foo(3, 14)") {
      ProcessExpr.fromStr(Some("foo(3, 14)"), List("some", "path")) should be(Some(ProcessCustom(
        List("foo"),
        Seq(Ast.expr.IntNum(3), Ast.expr.IntNum(14))
      )))
    }
  }
}
