package io.kaitai.struct

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class LanguageOutputWriterSpec extends AnyFunSpec with Matchers {
  describe("StringLanguageOutputWriter") {
    describe("capture") {
      it("defers output while preserving indentation, blank lines and ordering") {
        val out = new StringLanguageOutputWriter("\t")
        out.puts("before")
        out.inc
        val (value, captured) = out.capture {
          out.puts("first")
          out.puts
          out.inc
          out.puts("second")
          42
        }
        value shouldEqual 42
        captured.result shouldEqual "\tfirst\n\n\t\tsecond\n"
        out.result shouldEqual "before\n"
        out.indentLevel shouldEqual 1
        out.puts("middle")
        out.add(captured)
        out.puts("after")
        out.result shouldEqual "before\n\tmiddle\n\tfirst\n\n\t\tsecond\n\tafter\n"
      }

      it("supports nested captures without leaking or losing output") {
        val out = new StringLanguageOutputWriter("  ")
        val (_, outer) = out.capture {
          out.puts("outer")
          val (_, inner) = out.capture {
            out.inc
            out.puts("inner")
          }
          out.puts("between")
          out.add(inner)
          out.puts("end")
        }
        out.result shouldEqual ""
        out.add(outer)
        out.puts("last")
        out.result shouldEqual "outer\nbetween\n  inner\nend\nlast\n"
        out.indentLevel shouldEqual 0
      }

      it("restores the destination and indentation when the body throws") {
        val out = new StringLanguageOutputWriter("\t")
        out.puts("before")
        intercept[IllegalArgumentException] {
          out.capture {
            out.inc
            out.puts("discard")
            throw new IllegalArgumentException("test")
          }
        }
        out.puts("after")
        out.result shouldEqual "before\nafter\n"
        out.indentLevel shouldEqual 0
      }

      it("keeps an empty capture empty") {
        val out = new StringLanguageOutputWriter("\t")
        val (value, captured) = out.capture { "expression" }
        value shouldEqual "expression"
        captured.result shouldEqual ""
        out.add(captured)
        out.result shouldEqual ""
      }
    }

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
