package io.kaitai.struct

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class LanguageOutputWriterSpec extends AnyFunSpec with Matchers {
  describe("StringLanguageOutputWriter") {
    describe("puts") {
      val out = new StringLanguageOutputWriter("\t")

      it("adds lines") {
        out.puts("foo")
        out.result shouldEqual "foo\n"
      }

      it("adds indented lines") {
        out.inc
        out.puts("bar")
        out.result shouldEqual "foo\n\tbar\n"
      }
    }

    describe("putsLines") {
      it("adds lines with prefix") {
        val out = new StringLanguageOutputWriter("\t")
        out.putsLines("* ", "1\n2\n3")
        out.result shouldEqual "* 1\n* 2\n* 3\n"
      }

      it("adds lines with prefix and hanging indent") {
        val out = new StringLanguageOutputWriter("\t")
        out.putsLines("* ", "1\n2\n3", "__")
        out.result shouldEqual "* 1\n* __2\n* __3\n"
      }
    }
  }
}
