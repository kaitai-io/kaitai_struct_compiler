package io.kaitai.struct.problems

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ProblemCoords$Test extends AnyFunSpec {
  describe("ProblemCoords.message") {
    it("full message works") {
      val pc = ProblemCoords(
        file = Some("filename"),
        path = Some(List("a", "b", "c")),
        line = Some(123),
        col = Some(456)
      )

      pc.message should be("filename:123:456: /a/b/c")
    }

    it("no message works") {
      val pc = ProblemCoords()
      pc.message should be("(main)")
    }

    it("YAML path only message works") {
      val pc = ProblemCoords(
        path = Some(List("a", "b", "c")),
      )

      pc.message should be("(main): /a/b/c")
    }

    it("Coords with file message works") {
      val pc = ProblemCoords(
        file = Some("filename"),
        line = Some(123),
        col = Some(456)
      )

      pc.message should be("filename:123:456")
    }
  }
}
