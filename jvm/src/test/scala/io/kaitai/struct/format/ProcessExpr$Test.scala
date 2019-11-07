package io.kaitai.struct.format

import io.kaitai.struct.exprlang.Ast
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class ProcessExpr$Test extends FunSpec {
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
