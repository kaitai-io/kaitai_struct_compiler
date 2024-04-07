package io.kaitai.struct.exprlang

import io.kaitai.struct.exprlang.Ast._
import io.kaitai.struct.exprlang.Ast.expr._
import io.kaitai.struct.exprlang.Ast.operator._
import io.kaitai.struct.exprlang.Ast.cmpop._
import io.kaitai.struct.exprlang.Ast.unaryop._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

import scala.collection.mutable.ArrayBuffer

class ExpressionsSpec extends AnyFunSpec {
  describe("Expressions.parse") {
    it("parses single positive integer") {
      Expressions.parse("123") should be (IntNum(123))
    }

    it("parses single negative integer") {
      Expressions.parse("-456") should be (UnaryOp(Minus, IntNum(456)))
    }

    it("parses positive integer with underscores") {
      Expressions.parse("100_500") should be (IntNum(100500))
    }

    it("parses hex integer") {
      Expressions.parse("0x1234") should be (IntNum(0x1234))
    }

    it("parses hex integer with underscores") {
      Expressions.parse("0x12_34") should be (IntNum(0x1234))
    }

    it("parses octal integer") {
      Expressions.parse("0o644") should be (IntNum(420))
    }

    it("parses octal integer with undescores") {
      Expressions.parse("0o06_44") should be (IntNum(420))
    }

    it("parses binary integer") {
      Expressions.parse("0b10101010") should be (IntNum(0xaa))
    }

    it("parses binary integer with undescores") {
      Expressions.parse("0b1010_1_010") should be (IntNum(0xaa))
    }

    it("parses simple float") {
      Expressions.parse("1.2345") should be (FloatNum(1.2345))
    }

    it("parses float with positive exponent") {
      Expressions.parse("123e4") should be (FloatNum(123e4))
    }

    it("parses float with positive exponent with plus sign") {
      Expressions.parse("123e+4") should be (FloatNum(123e4))
    }

    it("parses float with negative exponent") {
      Expressions.parse("123e-7") should be (FloatNum(123e-7))
    }

    it("parses float + non-integral part with positive exponent") {
      Expressions.parse("1.2345e7") should be (FloatNum(1.2345e7))
    }

    it("parses float + non-integral part with positive exponent with plus sign") {
      Expressions.parse("123.45e+7") should be (FloatNum(123.45e7))
    }

    it("parses float + non-integral part with negative exponent") {
      Expressions.parse("123.45e-7") should be (FloatNum(123.45e-7))
    }

    it("parses 1 + 2") {
      Expressions.parse("1 + 2") should be (BinOp(IntNum(1), Add, IntNum(2)))
    }

    it("parses 1 + 2 + 5") {
      Expressions.parse("1 + 2 + 5") should be (
        BinOp(BinOp(IntNum(1), Add, IntNum(2)), Add, IntNum(5))
      )
    }

    it("parses (1 + 2) / (7 * 8)") {
      Expressions.parse("(1 + 2) / (7 * 8)") should be (
        BinOp(
          BinOp(IntNum(1), Add, IntNum(2)),
          Div,
          BinOp(IntNum(7), Mult, IntNum(8))
        )
      )
    }

    it("parses 1 < 2") {
      Expressions.parse("1 < 2") should be (Compare(IntNum(1), Lt, IntNum(2)))
    }

    it("parses a[42]") {
      Expressions.parse("a[42]") should be (Subscript(Name(identifier("a")), IntNum(42)))
    }

    it("parses a[42 - 2]") {
      Expressions.parse("a[42 - 2]") should be (
        Subscript(
          Name(identifier("a")),
          BinOp(IntNum(42), Sub, IntNum(2))
        )
      )
    }

    it("parses 2 < 3 ? \"foo\" : \"bar\"") {
      Expressions.parse("2 < 3 ? \"foo\" : \"bar\"") should be (
        IfExp(
          Compare(IntNum(2), Lt, IntNum(3)),
          Str("foo"),
          Str("bar")
        )
      )
    }

    it("parses bitwise invert operation") {
      Expressions.parse("~777") should be (UnaryOp(Invert, IntNum(777)))
    }

    it("parses ~(7+3)") {
      Expressions.parse("~(7+3)") should be (UnaryOp(Invert, BinOp(IntNum(7), Add, IntNum(3))))
    }

    // Enums
    it("parses port::http") {
      Expressions.parse("port::http") should be (EnumByLabel(identifier("port"), identifier("http")))
    }

    it("parses some_type::port::http") {
      Expressions.parse("some_type::port::http") should be (
        EnumByLabel(
          identifier("port"),
          identifier("http"),
          typeId(absolute = false, Seq("some_type"))
        )
      )
    }

    it("parses parent_type::child_type::port::http") {
      Expressions.parse("parent_type::child_type::port::http") should be (
        EnumByLabel(
          identifier("port"),
          identifier("http"),
          typeId(absolute = false, Seq("parent_type", "child_type"))
        )
      )
    }

    it("parses ::parent_type::child_type::port::http") {
      Expressions.parse("::parent_type::child_type::port::http") should be (
        EnumByLabel(
          identifier("port"),
          identifier("http"),
          typeId(absolute = true, Seq("parent_type", "child_type"))
        )
      )
    }

    it("parses port::http.to_i + 8000 == 8080") {
      Expressions.parse("port::http.to_i + 8000 == 8080") should be (
        Compare(
          BinOp(
            Attribute(
              EnumByLabel(identifier("port"),identifier("http")),
              identifier("to_i")
            ),
            Add,
            IntNum(8000)
          ),
          Eq,
          IntNum(8080)
        )
      )
    }

    it("parses [1, 2, 0x1234]") {
      Expressions.parse("[1, 2, 0x1234]") should be (
        List(Seq(IntNum(1), IntNum(2), IntNum(4660)))
      )
    }

    describe("boolean literals") {
      it("parses true") {
        Expressions.parse("true") should be(Bool(true))
      }

      it("parses false") {
        Expressions.parse("false") should be(Bool(false))
      }

      it("parses truer") {
        Expressions.parse("truer") should be(Name(identifier("truer")))
      }
    }

    describe("boolean operations") {
      it("parses not foo") {
        Expressions.parse("not foo") should be(
          UnaryOp(
            Ast.unaryop.Not,
            Name(identifier("foo"))
          )
        )
      }

      it("parses note_len") {
        Expressions.parse("note_len") should be(Name(identifier("note_len")))
      }

      it("parses notnot") {
        Expressions.parse("notnot") should be(Name(identifier("notnot")))
      }

      it("parses not not true") {
        Expressions.parse("not not true") should be(
          UnaryOp(
            Ast.unaryop.Not,
            UnaryOp(
              Ast.unaryop.Not,
              Bool(true)
            )
          )
        )
      }
    }

    describe("strings literals") {
      it("parses simple string") {
        Expressions.parse("\"abc\"") should be(Str("abc"))
      }

      it("parses simple string with space at the start") {
        Expressions.parse("\" abc\"") should be(Str(" abc"))
      }

      it("parses simple string with space at the end") {
        Expressions.parse("\"abc \"") should be(Str("abc "))
      }

      it("parses interpolated string with newline") {
        Expressions.parse("\"abc\\ndef\"") should be(Str("abc\ndef"))
      }

      it("parses non-interpolated string with newline") {
        Expressions.parse("'abc\\ndef'") should be(Str("abc\\ndef"))
      }

      it("parses interpolated string with zero char") {
        Expressions.parse("\"abc\\0def\"") should be(Str("abc\u0000def"))
      }

      it("parses non-interpolated string with zero char") {
        Expressions.parse("'abc\\0def'") should be(Str("abc\\0def"))
      }

      it("parses interpolated string with octal char") {
        Expressions.parse("\"abc\\75def\"") should be(Str("abc=def"))
      }

      it("parses interpolated string with hex unicode char") {
        Expressions.parse("\"abc\\u21bbdef\"") should be(Str("abc\u21bbdef"))
      }

      it("parses double-quoted string with double quote") {
        Expressions.parse("\"this \\\" is a quote\"") should be(Str("this \" is a quote"))
      }
    }

    // Casts
    it("parses 123.as<u4>") {
      Expressions.parse("123.as<u4>") should be (
        CastToType(IntNum(123), typeId(false, Seq("u4")))
      )
    }

    it("parses (123).as<u4>") {
      Expressions.parse("(123).as<u4>") should be (
        CastToType(IntNum(123), typeId(false, Seq("u4")))
      )
    }

    it("parses \"str\".as<x>") {
      Expressions.parse("\"str\".as<x>") should be (
        CastToType(Str("str"), typeId(false, Seq("x")))
      )
    }

    it("parses foo.as<x>") {
      Expressions.parse("foo.as<x>") should be (
        CastToType(Name(identifier("foo")), typeId(false, Seq("x")))
      )
    }

    it("parses foo.as < x  >  ") {
      Expressions.parse("foo.as < x  >  ") should be (
        CastToType(Name(identifier("foo")), typeId(false, Seq("x")))
      )
    }

    it("parses foo.as<bar::baz>") {
      Expressions.parse("foo.as<bar::baz>") should be (
        CastToType(Name(identifier("foo")), typeId(false, Seq("bar", "baz")))
      )
    }

    it("parses foo.as<::bar::baz>") {
      Expressions.parse("foo.as<::bar::baz>") should be (
        CastToType(Name(identifier("foo")), typeId(true, Seq("bar", "baz")))
      )
    }

    it("parses foo.as<bar[]>") {
      Expressions.parse("foo.as<bar[]>") should be (
        CastToType(Name(identifier("foo")), typeId(false, Seq("bar"), true))
      )
    }

    it("parses foo.as<::bar::baz[]>") {
      Expressions.parse("foo.as<::bar::baz[]>") should be (
        CastToType(Name(identifier("foo")), typeId(true, Seq("bar", "baz"), true))
      )
    }

    it("parses foo.as") {
      Expressions.parse("foo.as") should be (Attribute(Name(identifier("foo")),identifier("as")))
    }

    it("parses foo.as<x") {
      Expressions.parse("foo.as<x") should be (
        Compare(
          Attribute(Name(identifier("foo")),identifier("as")),
          Lt,
          Name(identifier("x"))
        )
      )
    }

    // sizeof keyword
    it("parses sizeof<foo>") {
      Expressions.parse("sizeof<foo>") should be (
        ByteSizeOfType(typeId(false, Seq("foo")))
      )
    }

    it("parses sizeof<foo::bar>") {
      Expressions.parse("sizeof<foo::bar>") should be (
        ByteSizeOfType(typeId(false, Seq("foo", "bar")))
      )
    }

    it("parses sizeof<::foo::bar>") {
      Expressions.parse("sizeof<::foo::bar>") should be (
        ByteSizeOfType(typeId(true, Seq("foo", "bar")))
      )
    }

    it("parses sizeof<foo") {
      Expressions.parse("sizeof<foo") should be (
        Compare(
          Name(identifier("sizeof")),
          Lt,
          Name(identifier("foo"))
        )
      )
    }

    // bitsizeof keyword
    it("parses bitsizeof<foo>") {
      Expressions.parse("bitsizeof<foo>") should be (
        BitSizeOfType(typeId(false, Seq("foo")))
      )
    }

    it("parses bitsizeof<foo") {
      Expressions.parse("bitsizeof<foo") should be (
        Compare(
          Name(identifier("bitsizeof")),
          Lt,
          Name(identifier("foo"))
        )
      )
    }

    // Attribute / method call
    it("parses 123.to_s") {
      Expressions.parse("123.to_s") should be (Attribute(IntNum(123),identifier("to_s")))
    }

    it("parses 123.4.to_s") {
      Expressions.parse("123.4.to_s") should be (Attribute(FloatNum(123.4),identifier("to_s")))
    }

    it("parses foo.bar") {
      Expressions.parse("foo.bar") should be (Attribute(Name(identifier("foo")),identifier("bar")))
    }

    describe("strings") {
      it("single-quoted") {
        // \" -> \"
        // \\ -> \\
        Expressions.parse(""" ' \" \\ ' """) should be(Str(" \\\" \\\\ "))
        Expressions.parse(""" 'ASCII\\x' """) should be(Str("ASCII\\\\x"))
      }
      it("double-quoted") {
        // \" -> "
        // \\ -> \
        Expressions.parse(""" " \" \\ " """) should be(Str(" \" \\ "))
        Expressions.parse(""" "ASCII\\'x" """) should be(Str("ASCII\\'x"))
      }
    }

    describe("f-strings") {
      it("parses f-string with just a string") {
        Expressions.parse("f\"abc\"") should be(InterpolatedStr(Seq(
          Str("abc")
        )))
      }

      it("parses f-string with just one expression") {
        Expressions.parse("f\"{123}\"") should be(InterpolatedStr(Seq(
          IntNum(123)
        )))
      }

      it("parses f-string with string + expression") {
        Expressions.parse("f\"foo={123}\"") should be(InterpolatedStr(Seq(
          Str("foo="),
          IntNum(123)
        )))
      }

      it("parses f-string with expression + string") {
        Expressions.parse("f\"{123}=abc\"") should be(InterpolatedStr(Seq(
          IntNum(123),
          Str("=abc")
        )))
      }

      it("parses f-string with str + expression + str") {
        Expressions.parse("f\"abc={123}=def\"") should be(InterpolatedStr(Seq(
          Str("abc="),
          IntNum(123),
          Str("=def")
        )))
      }

      it("parses f-string string with newline in the middle") {
        Expressions.parse("f\"abc\\ndef\"") should be(InterpolatedStr(Seq(Str("abc\ndef"))))
      }

      it("parses f-string with double quote in the middle") {
        Expressions.parse("f\"this \\\" is a quote\"") should be(InterpolatedStr(Seq(
          Str("this \" is a quote")
        )))
      }

      it("parses f-string with string in it") {
        Expressions.parse("f\"abc{\"def\"}ghi\"") should be(InterpolatedStr(Seq(
          Str("abc"),
          Str("def"),
          Str("ghi"),
        )))
      }

      it("parses f-string with space at the start") {
        Expressions.parse("f\" foo\"") should be(InterpolatedStr(Seq(
          Str(" foo")
        )))
      }

      it("parses f-string with space at the end") {
        Expressions.parse("f\"foo \"") should be(InterpolatedStr(Seq(
          Str("foo ")
        )))
      }

      it("parses f-string with double quote at the start") {
        Expressions.parse("f\"\\\" is a quote\"") should be(InterpolatedStr(Seq(
          Str("\" is a quote")
        )))
      }

      it("parses f-string with space and double quote at the start") {
        Expressions.parse("f\" \\\" is a quote\"") should be(InterpolatedStr(Seq(
          Str(" \" is a quote")
        )))
      }

      it("parses f-string with f-string in it") {
        Expressions.parse("f\"abc{f\"def\"}ghi\"") should be(InterpolatedStr(Seq(
          Str("abc"),
          InterpolatedStr(Seq(Str("def"))),
          Str("ghi"),
        )))
      }
    }
  }
}
