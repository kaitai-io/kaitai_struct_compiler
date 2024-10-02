package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.{Ast, Expressions}
import io.kaitai.struct.languages._
import io.kaitai.struct.languages.components.{CppImportList, LanguageCompilerStatic}
import io.kaitai.struct.translators.TestTypeProviders._
import io.kaitai.struct.{ImportList, RuntimeConfig, StringLanguageOutputWriter}
import org.scalactic.source.Position
import org.scalatest.Tag
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import scala.collection.immutable.ListMap

class TranslatorSpec extends AnyFunSpec {
  describe("integer literals + unary minus") {
    describe("simple") {
      everybody("123", "123", Int1Type(true))
      everybody("223", "223", Int1Type(false))
      everybody("1234", "1234")
      everybody("-456", "-456")
      everybody("0x1234", "4660")
      // less and more than 32 Bit signed int
      everybody("1000000000", "1000000000")
      everybodyExcept("100000000000", "100000000000", ResultMap(
        CppCompiler -> "100000000000LL",
        GoCompiler -> "int64(100000000000)",
        JavaCompiler -> "100000000000L"
      ))
    }

    describe("extreme") {
      // 0x7fff_ffff
      everybody("2147483647", "2147483647")
      // 0x8000_0000
      everybodyExcept("2147483648", "2147483648", ResultMap(
        CppCompiler -> "2147483648UL",
        GoCompiler -> "uint32(2147483648)",
        JavaCompiler -> "2147483648L",
      ))
      // 0xffff_ffff
      everybodyExcept("4294967295", "4294967295", ResultMap(
        CppCompiler -> "4294967295UL",
        GoCompiler -> "uint32(4294967295)",
        JavaCompiler -> "4294967295L",
      ))
      // 0x1_0000_0000
      everybodyExcept("4294967296", "4294967296", ResultMap(
        CppCompiler -> "4294967296LL",
        GoCompiler -> "int64(4294967296)",
        JavaCompiler -> "4294967296L",
      ))

      // -0x7fff_ffff
      everybody("-2147483647", "-2147483647")
      // -0x8000_0000
      everybodyExcept("-2147483648", "-2147483648", ResultMap(
        CppCompiler -> "-2147483647 - 1",
        LuaCompiler -> "-2147483647 - 1",
        PHPCompiler -> "-2147483647 - 1",
      ))
      // -0x8000_0001
      everybodyExcept("-2147483649", "-2147483649", ResultMap(
        CppCompiler -> "-2147483649LL",
        GoCompiler -> "int64(-2147483649)",
        JavaCompiler -> "-2147483649L",
      ))

      // 0x7fff_ffff_ffff_ffff
      everybodyExcept("9223372036854775807", "9223372036854775807", ResultMap(
        CppCompiler -> "9223372036854775807LL",
        GoCompiler -> "int64(9223372036854775807)",
        JavaCompiler -> "9223372036854775807L",
      ))
      // 0x8000_0000_0000_0000
      everybodyExcept("9223372036854775808", "9223372036854775808", ResultMap(
        CppCompiler -> "9223372036854775808ULL",
        GoCompiler -> "uint64(9223372036854775808)",
        JavaCompiler -> "0x8000000000000000L",
        LuaCompiler -> "0x8000000000000000",
        PHPCompiler -> "-9223372036854775807 - 1",
      ))
      // 0xffff_ffff_ffff_ffff
      everybodyExcept("18446744073709551615", "18446744073709551615", ResultMap(
        CppCompiler -> "18446744073709551615ULL",
        GoCompiler -> "uint64(18446744073709551615)",
        JavaCompiler -> "0xffffffffffffffffL",
        LuaCompiler -> "0xffffffffffffffff",
        PHPCompiler -> "-1",
      ))
      // -0x7fff_ffff_ffff_ffff
      everybodyExcept("-9223372036854775807", "-9223372036854775807", ResultMap(
        CppCompiler -> "-9223372036854775807LL",
        GoCompiler -> "int64(-9223372036854775807)",
        JavaCompiler -> "-9223372036854775807L",
      ))
      // -0x8000_0000_0000_0000
      everybodyExcept("-9223372036854775808", "-9223372036854775808", ResultMap(
        CppCompiler -> "-9223372036854775807LL - 1",
        GoCompiler -> "int64(-9223372036854775808)",
        JavaCompiler -> "-9223372036854775808L",
        LuaCompiler -> "-9223372036854775807 - 1",
        PHPCompiler -> "-9223372036854775807 - 1",
      ))
    }
  }

  describe("extreme integer literals and operators") {
    // TODO: repeat "integer literals / extreme" but add some math operations on top
  }

  describe("float literals") {
    everybody("1.0", "1.0", CalcFloatType)
    everybody("123.456", "123.456", CalcFloatType)
    everybody("-123.456", "-123.456", CalcFloatType)
  }

  describe("simple integer operations") {
    everybody("1 + 2", "1 + 2")

    everybodyExcept("3 / 2", "3 / 2", ResultMap(
      JavaScriptCompiler -> "Math.floor(3 / 2)",
      LuaCompiler -> "math.floor(3 / 2)",
      PerlCompiler -> "int(3 / 2)",
      PHPCompiler -> "intval(3 / 2)",
      PythonCompiler -> "3 // 2"
    ))

    everybody("1 + 2 + 5", "(1 + 2) + 5") // TODO: everybody("1 + 2 + 5", "1 + 2 + 5")
    everybody("(1 + 2) + 5", "(1 + 2) + 5") // TODO: everybody("(1 + 2) + 5", "1 + 2 + 5")
    everybody("1 + (2 + 5)", "1 + (2 + 5)") // TODO: everybody("1 + (2 + 5)", "1 + 2 + 5")

    everybody("1 - 2 + 5", "(1 - 2) + 5") // TODO: everybody("1 - 2 + 5", "1 - 2 + 5")
    everybody("(1 - 2) + 5", "(1 - 2) + 5") // TODO: everybody("(1 - 2) + 5", "1 - 2 + 5")
    everybody("1 - (2 + 5)", "1 - (2 + 5)")

    everybody("1 + 2 * 5", "1 + 2 * 5")
    everybody("1 + (2 * 5)", "1 + 2 * 5")
    everybody("(1 + 2) * 5", "(1 + 2) * 5")

    everybody("1 * 2 + 5", "1 * 2 + 5")
    everybody("1 * (2 + 5)", "1 * (2 + 5)")
    everybody("(1 * 2) + 5", "1 * 2 + 5")

    everybodyExcept("(1 + 2) / (7 * 8)", "(1 + 2) / (7 * 8)", ResultMap(
      JavaScriptCompiler -> "Math.floor((1 + 2) / (7 * 8))",
      LuaCompiler -> "math.floor((1 + 2) / (7 * 8))",
      PerlCompiler -> "int((1 + 2) / (7 * 8))",
      PHPCompiler -> "intval((1 + 2) / (7 * 8))",
      PythonCompiler -> "(1 + 2) // (7 * 8)"
    ))

    everybodyExcept("3 - ((1 + 2) / (7 * 8) + 5)", "3 - ((1 + 2) / (7 * 8) + 5)", ResultMap(
      JavaScriptCompiler -> "3 - (Math.floor((1 + 2) / (7 * 8)) + 5)",
      LuaCompiler -> "3 - (math.floor((1 + 2) / (7 * 8)) + 5)",
      PerlCompiler -> "3 - (int((1 + 2) / (7 * 8)) + 5)",
      PHPCompiler -> "3 - (intval((1 + 2) / (7 * 8)) + 5)",
      PythonCompiler -> "3 - ((1 + 2) // (7 * 8) + 5)"
    ))

    everybody("1 + 2 << 5", "1 + 2 << 5")
    everybody("(1 + 2) << 5", "1 + 2 << 5")
    everybody("1 + (2 << 5)", "1 + (2 << 5)")

    everybodyExcept("~777", "~777", ResultMap(
      GoCompiler -> "^777"
    ))
    everybodyExcept("~(7+3)", "~(7 + 3)", ResultMap(
      GoCompiler -> "^(7 + 3)"
    ))

    everybody("(0b01 & 0b11) << 1", "(1 & 3) << 1")

    everybodyExcept("(0b01 & 0b10) >> 1", "(1 & 2) >> 1", ResultMap(
      JavaScriptCompiler -> "(1 & 2) >>> 1"
    ))
  }

  describe("integer comparisons") {
    everybody("1 < 2", "1 < 2", CalcBooleanType)
    everybody("1 == 2", "1 == 2", CalcBooleanType)
  }

  describe("integer method calls") {
    describe(".to_s") {
      full("42.to_s", CalcIntType, CalcStrType, ResultMap(
        CppCompiler -> "kaitai::kstream::to_string(42)",
        CSharpCompiler -> "42.ToString()",
        GoCompiler -> "strconv.FormatInt(int64(42), 10)",
        JavaCompiler -> "Long.toString(42)",
        JavaScriptCompiler -> "(42).toString()",
        LuaCompiler -> "tostring(42)",
        PerlCompiler -> "sprintf('%d', 42)",
        PHPCompiler -> "strval(42)",
        PythonCompiler -> "str(42)",
        RubyCompiler -> "42.to_s"
      ))

      full("(a + 42).to_s", CalcIntType, CalcStrType, ResultMap(
        CppCompiler -> "kaitai::kstream::to_string(a() + 42)",
        CSharpCompiler -> "(A + 42).ToString()",
        GoCompiler -> "strconv.FormatInt(int64(this.A + 42), 10)",
        JavaCompiler -> "Long.toString(a() + 42)",
        JavaScriptCompiler -> "(this.a + 42).toString()",
        LuaCompiler -> "tostring(self.a + 42)",
        PerlCompiler -> "sprintf('%d', $self->a() + 42)",
        PHPCompiler -> "strval($this->a() + 42)",
        PythonCompiler -> "str(self.a + 42)",
        RubyCompiler -> "(a + 42).to_s"
      ))

      full("a + 42.to_s", CalcStrType, CalcStrType, ResultMap(
        CppCompiler -> "a() + kaitai::kstream::to_string(42)",
        CSharpCompiler -> "A + 42.ToString()",
        GoCompiler -> "this.A + strconv.FormatInt(int64(42), 10)",
        JavaCompiler -> "a() + Long.toString(42)",
        JavaScriptCompiler -> "this.a + (42).toString()",
        LuaCompiler -> "self.a .. tostring(42)",
        PerlCompiler -> "$self->a() . sprintf('%d', 42)",
        PHPCompiler -> "$this->a() . strval(42)",
        PythonCompiler -> "self.a + str(42)",
        RubyCompiler -> "a + 42.to_s"
      ))
    }
  }

  describe("ternary operator") {
    full("2 < 3 ? \"foo\" : \"bar\"", CalcIntType, CalcStrType, ResultMap(
      CppCompiler -> "((2 < 3) ? (std::string(\"foo\")) : (std::string(\"bar\")))",
      CSharpCompiler -> "(2 < 3 ? \"foo\" : \"bar\")",
      GoCompiler ->
        """var tmp1 string;
          |if (2 < 3) {
          |  tmp1 = "foo"
          |} else {
          |  tmp1 = "bar"
          |}
          |tmp1""".stripMargin,
      JavaCompiler -> "(2 < 3 ? \"foo\" : \"bar\")",
      JavaScriptCompiler -> "(2 < 3 ? \"foo\" : \"bar\")",
      LuaCompiler -> "utils.box_unwrap((2 < 3) and utils.box_wrap(\"foo\") or (\"bar\"))",
      PerlCompiler -> "(2 < 3 ? \"foo\" : \"bar\")",
      PHPCompiler -> "(2 < 3 ? \"foo\" : \"bar\")",
      PythonCompiler -> "(u\"foo\" if 2 < 3 else u\"bar\")",
      RubyCompiler -> "(2 < 3 ? \"foo\" : \"bar\")"
    ))
  }

  describe("simple float operations") {
    everybody("1.2 + 3.4", "1.2 + 3.4", CalcFloatType)
    everybody("1.2 + 3", "1.2 + 3", CalcFloatType)
    everybody("1 + 3.4", "1 + 3.4", CalcFloatType)

    everybody("1.0 < 2", "1.0 < 2", CalcBooleanType)

    everybody("3 / 2.0", "3 / 2.0", CalcFloatType)

    everybody("(1 + 2) / (7 * 8.1)", "(1 + 2) / (7 * 8.1)", CalcFloatType)
  }

  describe("boolean literals") {
    full("true", CalcBooleanType, CalcBooleanType, ResultMap(
      CppCompiler -> "true",
      CSharpCompiler -> "true",
      GoCompiler -> "true",
      JavaCompiler -> "true",
      JavaScriptCompiler -> "true",
      LuaCompiler -> "true",
      PerlCompiler -> "1",
      PHPCompiler -> "true",
      PythonCompiler -> "True",
      RubyCompiler -> "true"
    ))

    full("false", CalcBooleanType, CalcBooleanType, ResultMap(
      CppCompiler -> "false",
      CSharpCompiler -> "false",
      GoCompiler -> "false",
      JavaCompiler -> "false",
      JavaScriptCompiler -> "false",
      LuaCompiler -> "false",
      PerlCompiler -> "0",
      PHPCompiler -> "false",
      PythonCompiler -> "False",
      RubyCompiler -> "false"
    ))
  }

  describe("boolean operations") {
    full("some_bool.to_i", CalcBooleanType, CalcIntType, ResultMap(
      CppCompiler -> "((some_bool()) ? 1 : 0)",
      CSharpCompiler -> "(SomeBool ? 1 : 0)",
      GoCompiler ->
        """tmp1 := 0
          |if this.SomeBool {
          |  tmp1 = 1
          |}
          |tmp1""".stripMargin,
      JavaCompiler -> "(someBool() ? 1 : 0)",
      JavaScriptCompiler -> "(this.someBool | 0)",
      LuaCompiler -> "(self.some_bool and 1 or 0)",
      PerlCompiler -> "$self->some_bool()",
      PHPCompiler -> "intval($this->someBool())",
      PythonCompiler -> "int(self.some_bool)",
      RubyCompiler -> "(some_bool ? 1 : 0)"
    ))
  }

  describe("member access") {
    full("foo_str", CalcStrType, CalcStrType, ResultMap(
      CppCompiler -> "foo_str()",
      CSharpCompiler -> "FooStr",
      GoCompiler -> "this.FooStr",
      JavaCompiler -> "fooStr()",
      JavaScriptCompiler -> "this.fooStr",
      LuaCompiler -> "self.foo_str",
      PerlCompiler -> "$self->foo_str()",
      PHPCompiler -> "$this->fooStr()",
      PythonCompiler -> "self.foo_str",
      RubyCompiler -> "foo_str"
    ))

    full("foo_block", userOwnedType(List("block")), userBorrowedType(List("block")), ResultMap(
      CppCompiler -> "foo_block()",
      CSharpCompiler -> "FooBlock",
      GoCompiler -> "this.FooBlock",
      JavaCompiler -> "fooBlock()",
      JavaScriptCompiler -> "this.fooBlock",
      LuaCompiler -> "self.foo_block",
      PerlCompiler -> "$self->foo_block()",
      PHPCompiler -> "$this->fooBlock()",
      PythonCompiler -> "self.foo_block",
      RubyCompiler -> "foo_block"
    ))

    full("foo.bar", FooBarProvider, CalcStrType, ResultMap(
      CppCompiler -> "foo()->bar()",
      CSharpCompiler -> "Foo.Bar",
      GoCompiler -> "this.Foo.Bar",
      JavaCompiler -> "foo().bar()",
      JavaScriptCompiler -> "this.foo.bar",
      LuaCompiler -> "self.foo.bar",
      PerlCompiler -> "$self->foo()->bar()",
      PHPCompiler -> "$this->foo()->bar()",
      PythonCompiler -> "self.foo.bar",
      RubyCompiler -> "foo.bar"
    ))

    full("foo.inner.baz", FooBarProvider, CalcIntType, ResultMap(
      CppCompiler -> "foo()->inner()->baz()",
      CSharpCompiler -> "Foo.Inner.Baz",
      GoCompiler -> "this.Foo.Inner.Baz",
      JavaCompiler -> "foo().inner().baz()",
      JavaScriptCompiler -> "this.foo.inner.baz",
      LuaCompiler -> "self.foo.inner.baz",
      PerlCompiler -> "$self->foo()->inner()->baz()",
      PHPCompiler -> "$this->foo()->inner()->baz()",
      PythonCompiler -> "self.foo.inner.baz",
      RubyCompiler -> "foo.inner.baz"
    ))

    full("_root.foo", userOwnedType(List("top_class", "block")), userBorrowedType(List("top_class", "block")), ResultMap(
      CppCompiler -> "_root()->foo()",
      CSharpCompiler -> "M_Root.Foo",
      GoCompiler -> "this._root.Foo",
      JavaCompiler -> "_root().foo()",
      JavaScriptCompiler -> "this._root.foo",
      LuaCompiler -> "self._root.foo",
      PerlCompiler -> "$self->_root()->foo()",
      PHPCompiler -> "$this->_root()->foo()",
      PythonCompiler -> "self._root.foo",
      RubyCompiler -> "_root.foo"
    ))

    full("a != 2 and a != 5", CalcIntType, CalcBooleanType, ResultMap(
      CppCompiler -> " ((a() != 2) && (a() != 5)) ",
      CSharpCompiler -> " ((A != 2) && (A != 5)) ",
      GoCompiler -> " ((this.A != 2) && (this.A != 5)) ",
      JavaCompiler -> " ((a() != 2) && (a() != 5)) ",
      JavaScriptCompiler -> " ((this.a != 2) && (this.a != 5)) ",
      LuaCompiler -> " ((self.a ~= 2) and (self.a ~= 5)) ",
      PerlCompiler -> " (($self->a() != 2) && ($self->a() != 5)) ",
      PHPCompiler -> " (($this->a() != 2) && ($this->a() != 5)) ",
      PythonCompiler -> " ((self.a != 2) and (self.a != 5)) ",
      RubyCompiler -> " ((a != 2) && (a != 5)) "
    ))
  }

  describe("arrays") {
    describe("literals") {
      full("[0, 1, 100500]", CalcIntType, ArrayTypeInStream(CalcIntType), ResultMap(
        CSharpCompiler -> "new List<int> { 0, 1, 100500 }",
        GoCompiler -> "[]int{0, 1, 100500}",
        JavaCompiler -> "new ArrayList<Integer>(Arrays.asList(0, 1, 100500))",
        JavaScriptCompiler -> "[0, 1, 100500]",
        LuaCompiler -> "{0, 1, 100500}",
        PerlCompiler -> "[0, 1, 100500]",
        PHPCompiler -> "[0, 1, 100500]",
        PythonCompiler -> "[0, 1, 100500]",
        RubyCompiler -> "[0, 1, 100500]"
      ))

      full("[34, 0, 10, 64, 65, 66, 92]", CalcIntType, CalcBytesType, ResultMap(
        CppCompiler -> "std::string(\"\\x22\\x00\\x0A\\x40\\x41\\x42\\x5C\", 7)",
        CSharpCompiler -> "new byte[] { 34, 0, 10, 64, 65, 66, 92 }",
        GoCompiler -> "[]uint8{34, 0, 10, 64, 65, 66, 92}",
        JavaCompiler -> "new byte[] { 34, 0, 10, 64, 65, 66, 92 }",
        JavaScriptCompiler -> "new Uint8Array([34, 0, 10, 64, 65, 66, 92])",
        LuaCompiler -> "\"\\034\\000\\010\\064\\065\\066\\092\"",
        PerlCompiler -> "pack('C*', (34, 0, 10, 64, 65, 66, 92))",
        PHPCompiler -> "\"\\x22\\x00\\x0A\\x40\\x41\\x42\\x5C\"",
        PythonCompiler -> "b\"\\x22\\x00\\x0A\\x40\\x41\\x42\\x5C\"",
        RubyCompiler -> "[34, 0, 10, 64, 65, 66, 92].pack('C*')"
      ))

      full("[255, 0, 255]", CalcIntType, CalcBytesType, ResultMap(
        CppCompiler -> "std::string(\"\\xFF\\x00\\xFF\", 3)",
        CSharpCompiler -> "new byte[] { 255, 0, 255 }",
        GoCompiler -> "[]uint8{255, 0, 255}",
        JavaCompiler -> "new byte[] { -1, 0, -1 }",
        JavaScriptCompiler -> "new Uint8Array([255, 0, 255])",
        LuaCompiler -> "\"\\255\\000\\255\"",
        PerlCompiler -> "pack('C*', (255, 0, 255))",
        PHPCompiler -> "\"\\xFF\\x00\\xFF\"",
        PythonCompiler -> "b\"\\xFF\\x00\\xFF\"",
        RubyCompiler -> "[255, 0, 255].pack('C*')"
      ))
    }

    describe("operations") {
      full("[0, 1, 2].length", CalcIntType, CalcIntType, ResultMap(
        CppCompiler -> "std::string(\"\\x00\\x01\\x02\", 3).length()",
        CSharpCompiler -> "new byte[] { 0, 1, 2 }.Length",
        GoCompiler -> "len([]uint8{0, 1, 2})",
        JavaCompiler -> "new byte[] { 0, 1, 2 }.length",
        JavaScriptCompiler -> "new Uint8Array([0, 1, 2]).length",
        LuaCompiler -> "#\"\\000\\001\\002\"",
        PerlCompiler -> "length(pack('C*', (0, 1, 2)))",
        PHPCompiler -> "strlen(\"\\x00\\x01\\x02\")",
        PythonCompiler -> "len(b\"\\x00\\x01\\x02\")",
        RubyCompiler -> "[0, 1, 2].pack('C*').size"
      ))

      full("a[42]", ArrayTypeInStream(CalcStrType), CalcStrType, ResultMap(
        CppCompiler -> "a()->at(42)",
        CSharpCompiler -> "A[42]",
        GoCompiler -> "this.A[42]",
        JavaCompiler -> "a().get((int) 42)",
        JavaScriptCompiler -> "this.a[42]",
        LuaCompiler -> "self.a[42 + 1]", // TODO: self.a[43]
        PerlCompiler -> "@{$self->a()}[42]",
        PHPCompiler -> "$this->a()[42]",
        PythonCompiler -> "self.a[42]",
        RubyCompiler -> "a[42]"
      ))

      full("a[42 - 2]", ArrayTypeInStream(CalcStrType), CalcStrType, ResultMap(
        CppCompiler -> "a()->at(42 - 2)",
        CSharpCompiler -> "A[42 - 2]",
        GoCompiler -> "this.A[42 - 2]",
        JavaCompiler -> "a().get((int) (42 - 2))",
        JavaScriptCompiler -> "this.a[42 - 2]",
        LuaCompiler -> "self.a[42 - 2 + 1]", // TODO: self.a[41]
        PerlCompiler -> "@{$self->a()}[42 - 2]",
        PHPCompiler -> "$this->a()[42 - 2]",
        PythonCompiler -> "self.a[42 - 2]",
        RubyCompiler -> "a[42 - 2]"
      ))

      full("a.first", ArrayTypeInStream(CalcIntType), CalcIntType, ResultMap(
        CppCompiler -> "a()->front()",
        CSharpCompiler -> "A[0]",
        GoCompiler -> "this.A[0]",
        JavaCompiler -> "a().get(0)",
        JavaScriptCompiler -> "this.a[0]",
        LuaCompiler -> "self.a[1]",
        PerlCompiler -> "@{$self->a()}[0]",
        PHPCompiler -> "$this->a()[0]",
        PythonCompiler -> "self.a[0]",
        RubyCompiler -> "a.first"
      ))

      full("a.last", ArrayTypeInStream(CalcIntType), CalcIntType, ResultMap(
        CppCompiler -> "a()->back()",
        CSharpCompiler -> "A[A.Count - 1]",
        GoCompiler ->
          """tmp1 := this.A
            |tmp1[len(tmp1) - 1]""".stripMargin,
        JavaCompiler -> "a().get(a().size() - 1)",
        JavaScriptCompiler -> "this.a[this.a.length - 1]",
        LuaCompiler -> "self.a[#self.a]",
        PerlCompiler -> "@{$self->a()}[-1]",
        PHPCompiler -> "$this->a()[count($this->a()) - 1]",
        PythonCompiler -> "self.a[-1]",
        RubyCompiler -> "a.last"
      ))

      full("a.size", ArrayTypeInStream(CalcIntType), CalcIntType, ResultMap(
        CppCompiler -> "a()->size()",
        CSharpCompiler -> "A.Count",
        GoCompiler -> "len(this.A)",
        JavaCompiler -> "a().size()",
        JavaScriptCompiler -> "this.a.length",
        LuaCompiler -> "#self.a",
        PHPCompiler -> "count($this->a())",
        PerlCompiler -> "scalar(@{$self->a()})",
        PythonCompiler -> "len(self.a)",
        RubyCompiler -> "a.length"
      ))
    }
  }

  describe("strings") {
    describe("literals") {
      full("\"str\"", CalcIntType, CalcStrType, ResultMap(
        CppCompiler -> "std::string(\"str\")",
        CSharpCompiler -> "\"str\"",
        GoCompiler -> "\"str\"",
        JavaCompiler -> "\"str\"",
        JavaScriptCompiler -> "\"str\"",
        LuaCompiler -> "\"str\"",
        PerlCompiler -> "\"str\"",
        PHPCompiler -> "\"str\"",
        PythonCompiler -> "u\"str\"",
        RubyCompiler -> "\"str\""
      ))

      full("\"str\\nnext\"", CalcIntType, CalcStrType, ResultMap(
        CppCompiler -> "std::string(\"str\\nnext\")",
        CSharpCompiler -> "\"str\\nnext\"",
        GoCompiler -> "\"str\\nnext\"",
        JavaCompiler -> "\"str\\nnext\"",
        JavaScriptCompiler -> "\"str\\nnext\"",
        LuaCompiler -> "\"str\\nnext\"",
        PerlCompiler -> "\"str\\nnext\"",
        PHPCompiler -> "\"str\\nnext\"",
        PythonCompiler -> "u\"str\\nnext\"",
        RubyCompiler -> "\"str\\nnext\""
      ))

      full("\"str\\u000anext\"", CalcIntType, CalcStrType, ResultMap(
        CppCompiler -> "std::string(\"str\\nnext\")",
        CSharpCompiler -> "\"str\\nnext\"",
        GoCompiler -> "\"str\\nnext\"",
        JavaCompiler -> "\"str\\nnext\"",
        JavaScriptCompiler -> "\"str\\nnext\"",
        LuaCompiler -> "\"str\\nnext\"",
        PerlCompiler -> "\"str\\nnext\"",
        PHPCompiler -> "\"str\\nnext\"",
        PythonCompiler -> "u\"str\\nnext\"",
        RubyCompiler -> "\"str\\nnext\""
      ))

      full("\"str\\0next\"", CalcIntType, CalcStrType, ResultMap(
        CppCompiler -> "std::string(\"str\\000next\", 8)",
        CSharpCompiler -> "\"str\\0next\"",
        GoCompiler -> "\"str\\000next\"",
        JavaCompiler -> "\"str\\000next\"",
        JavaScriptCompiler -> "\"str\\x00next\"",
        LuaCompiler -> "\"str\\000next\"",
        PerlCompiler -> "\"str\\000next\"",
        PHPCompiler -> "\"str\\000next\"",
        PythonCompiler -> "u\"str\\000next\"",
        RubyCompiler -> "\"str\\000next\""
      ))
    }

    describe("operators") {
      everybodyExcept("\"str1\" + \"str2\"", "\"str1\" + \"str2\"", ResultMap(
        CppCompiler -> "std::string(\"str1\") + std::string(\"str2\")",
        LuaCompiler -> "\"str1\" .. \"str2\"",
        PerlCompiler -> "\"str1\" . \"str2\"",
        PHPCompiler -> "\"str1\" . \"str2\"",
        PythonCompiler -> "u\"str1\" + u\"str2\""
      ), CalcStrType)

      everybodyExcept("\"str1\" == \"str2\"", "\"str1\" == \"str2\"", ResultMap(
        CppCompiler -> "std::string(\"str1\") == (std::string(\"str2\"))",
        JavaCompiler -> "\"str1\".equals(\"str2\")",
        LuaCompiler -> "\"str1\" == \"str2\"",
        PerlCompiler -> "\"str1\" eq \"str2\"",
        PythonCompiler -> "u\"str1\" == u\"str2\""
      ), CalcBooleanType)

      everybodyExcept("\"str1\" != \"str2\"", "\"str1\" != \"str2\"", ResultMap(
        CppCompiler -> "std::string(\"str1\") != std::string(\"str2\")",
        JavaCompiler -> "!(\"str1\").equals(\"str2\")",
        LuaCompiler -> "\"str1\" ~= \"str2\"",
        PerlCompiler -> "\"str1\" ne \"str2\"",
        PythonCompiler -> "u\"str1\" != u\"str2\""
      ), CalcBooleanType)

      everybodyExcept("\"str1\" < \"str2\"", "\"str1\" < \"str2\"", ResultMap(
        CppCompiler -> "(std::string(\"str1\").compare(std::string(\"str2\")) < 0)",
        CSharpCompiler -> "(\"str1\".CompareTo(\"str2\") < 0)",
        JavaCompiler -> "(\"str1\".compareTo(\"str2\") < 0)",
        LuaCompiler -> "\"str1\" < \"str2\"",
        PerlCompiler -> "\"str1\" lt \"str2\"",
        PythonCompiler -> "u\"str1\" < u\"str2\""
      ), CalcBooleanType)
    }

    describe("methods") {
      full("\"str\".length", CalcIntType, CalcIntType, ResultMap(
        CppCompiler -> "std::string(\"str\").length()",
        CSharpCompiler -> "\"str\".Length",
        GoCompiler -> "utf8.RuneCountInString(\"str\")",
        JavaCompiler -> "\"str\".length()",
        JavaScriptCompiler -> "\"str\".length",
        LuaCompiler -> "string.len(\"str\")",
        PerlCompiler -> "length(\"str\")",
        PHPCompiler -> "strlen(\"str\")",
        PythonCompiler -> "len(u\"str\")",
        RubyCompiler -> "\"str\".size"
      ))

      full("(foo + bar).length", CalcStrType, CalcIntType, ResultMap(
        CppCompiler -> "(foo() + bar()).length()",
        CSharpCompiler -> "(Foo + Bar).Length",
        GoCompiler -> "utf8.RuneCountInString(this.Foo + this.Bar)",
        JavaCompiler -> "(foo() + bar()).length()",
        JavaScriptCompiler -> "(this.foo + this.bar).length",
        LuaCompiler -> "string.len(self.foo .. self.bar)",
        PerlCompiler -> "length($self->foo() . $self->bar())",
        PHPCompiler -> "strlen($this->foo() . $this->bar())",
        PythonCompiler -> "len(self.foo + self.bar)",
        RubyCompiler -> "(foo + bar).size"
      ))

      full("\"str\".reverse", CalcIntType, CalcStrType, ResultMap(
        CppCompiler -> "kaitai::kstream::reverse(std::string(\"str\"))",
        CSharpCompiler -> "KaitaiStream.StringReverse(\"str\")",
        GoCompiler -> "kaitai.StringReverse(\"str\")",
        JavaCompiler -> "new StringBuilder(\"str\").reverse().toString()",
        JavaScriptCompiler -> "Array.from(\"str\").reverse().join('')",
        LuaCompiler -> "string.reverse(\"str\")",
        PerlCompiler -> "scalar(reverse(\"str\"))",
        PHPCompiler -> "strrev(\"str\")",
        PythonCompiler -> "(u\"str\")[::-1]",
        RubyCompiler -> "\"str\".reverse"
      ))

      full("(foo + bar).reverse", CalcStrType, CalcStrType, ResultMap(
        CppCompiler -> "kaitai::kstream::reverse(foo() + bar())",
        CSharpCompiler -> "KaitaiStream.StringReverse(Foo + Bar)",
        GoCompiler -> "kaitai.StringReverse(this.Foo + this.Bar)",
        JavaCompiler -> "new StringBuilder(foo() + bar()).reverse().toString()",
        JavaScriptCompiler -> "Array.from(this.foo + this.bar).reverse().join('')",
        LuaCompiler -> "string.reverse(self.foo .. self.bar)",
        PerlCompiler -> "scalar(reverse($self->foo() . $self->bar()))",
        PHPCompiler -> "strrev($this->foo() . $this->bar())",
        PythonCompiler -> "(self.foo + self.bar)[::-1]",
        RubyCompiler -> "(foo + bar).reverse"
      ))

      full("\"12345\".to_i", CalcIntType, CalcIntType, ResultMap(
        CppCompiler -> "kaitai::kstream::string_to_int(std::string(\"12345\"))",
        CSharpCompiler -> "Convert.ToInt64(\"12345\", 10)",
        GoCompiler ->
          """tmp1, err := strconv.ParseInt("12345", 10, 0)
            |if err != nil {
            |  return err
            |}
            |tmp1""".stripMargin,
        JavaCompiler -> "Long.parseLong(\"12345\", 10)",
        JavaScriptCompiler -> "Number.parseInt(\"12345\", 10)",
        LuaCompiler -> "tonumber(\"12345\")",
        PerlCompiler -> "\"12345\" + 0",
        PHPCompiler -> "intval(\"12345\", 10)",
        PythonCompiler -> "int(u\"12345\")",
        RubyCompiler -> "\"12345\".to_i"
      ))

      full("\"1234fe\".to_i(16)", CalcIntType, CalcIntType, ResultMap(
        CppCompiler -> "kaitai::kstream::string_to_int(std::string(\"1234fe\"), 16)",
        CSharpCompiler -> "Convert.ToInt64(\"1234fe\", 16)",
        GoCompiler ->
          """tmp1, err := strconv.ParseInt("1234fe", 16, 0)
            |if err != nil {
            |  return err
            |}
            |tmp1""".stripMargin,
        JavaCompiler -> "Long.parseLong(\"1234fe\", 16)",
        JavaScriptCompiler -> "Number.parseInt(\"1234fe\", 16)",
        LuaCompiler -> "tonumber(\"1234fe\", 16)",
        PerlCompiler -> "hex(\"1234fe\")",
        PHPCompiler -> "intval(\"1234fe\", 16)",
        PythonCompiler -> "int(u\"1234fe\", 16)",
        RubyCompiler -> "\"1234fe\".to_i(16)"
      ))

      // substring() call on a single string literal
      full("\"foobar\".substring(2, 4)", CalcStrType, CalcStrType, ResultMap(
        CppCompiler -> "std::string(\"foobar\").substr(2, 4 - 2)",
        CSharpCompiler -> "\"foobar\".Substring(2, 4 - 2)",
        GoCompiler -> "\"foobar\"[2:4]",
        JavaCompiler -> "\"foobar\".substring(2, 4)",
        JavaScriptCompiler -> "\"foobar\".substring(2, 4)",
        LuaCompiler -> "string.sub(\"foobar\", 2 + 1, 4)",
        PerlCompiler -> "substr(\"foobar\", 2, 4 - 2)",
        PHPCompiler -> "\\Kaitai\\Struct\\Stream::substring(\"foobar\", 2, 4)",
        PythonCompiler -> "u\"foobar\"[2:4]",
        RubyCompiler -> "\"foobar\"[2...4]"
      ))

      // substring() call on concatenation of strings: for some languages, concatenation needs to be
      // parenthesized
      full("(foo + bar).substring(2, 4)", CalcStrType, CalcStrType, ResultMap(
        CppCompiler -> "(foo() + bar()).substr(2, 4 - 2)",
        CSharpCompiler -> "(Foo + Bar).Substring(2, 4 - 2)",
        GoCompiler -> "(this.Foo + this.Bar)[2:4]",
        JavaCompiler -> "(foo() + bar()).substring(2, 4)",
        JavaScriptCompiler -> "(this.foo + this.bar).substring(2, 4)",
        LuaCompiler -> "string.sub(self.foo .. self.bar, 2 + 1, 4)",
        PerlCompiler -> "substr($self->foo() . $self->bar(), 2, 4 - 2)",
        PHPCompiler -> "\\Kaitai\\Struct\\Stream::substring($this->foo() . $this->bar(), 2, 4)",
        PythonCompiler -> "(self.foo + self.bar)[2:4]",
        RubyCompiler -> "(foo + bar)[2...4]"
      ))

      // substring() call with non-left-associative "from" and "to": for languages where subtraction
      // is used, it should be rendered as "to - (from)".
      full("foo.substring(10 - 7, 10 - 3)", CalcStrType, CalcStrType, ResultMap(
        CppCompiler -> "foo().substr(10 - 7, (10 - 3) - (10 - 7))", // TODO: CppCompiler -> "foo().substr(10 - 7, 10 - 3 - (10 - 7))",
        CSharpCompiler -> "Foo.Substring(10 - 7, (10 - 3) - (10 - 7))", // TODO: CSharpCompiler -> "Foo.Substring(10 - 7, 10 - 3 - (10 - 7))",
        GoCompiler -> "this.Foo[10 - 7:10 - 3]",
        JavaCompiler -> "foo().substring(10 - 7, 10 - 3)",
        JavaScriptCompiler -> "this.foo.substring(10 - 7, 10 - 3)",
        LuaCompiler -> "string.sub(self.foo, (10 - 7) + 1, 10 - 3)", // TODO: LuaCompiler -> "string.sub(self.foo, 10 - 7 + 1, 10 - 3)",
        PerlCompiler -> "substr($self->foo(), 10 - 7, (10 - 3) - (10 - 7))", // TODO: PerlCompiler -> "substr($self->foo(), 10 - 7, 10 - 3 - (10 - 7))",
        PHPCompiler -> "\\Kaitai\\Struct\\Stream::substring($this->foo(), 10 - 7, 10 - 3)",
        PythonCompiler -> "self.foo[10 - 7:10 - 3]",
        RubyCompiler -> "foo[10 - 7...10 - 3]"
      ))

      // substring() call with "to" using `<<` which is lower precedence than `+` or `-`: if such
      // math is used on "to" in rendering, "to" should be parenthesized (Ruby)
      full("foo.substring(10 - 7, 10 << 2)", CalcStrType, CalcStrType, ResultMap(
        CppCompiler -> "foo().substr(10 - 7, (10 << 2) - (10 - 7))",
        CSharpCompiler -> "Foo.Substring(10 - 7, (10 << 2) - (10 - 7))",
        GoCompiler -> "this.Foo[10 - 7:10 << 2]",
        JavaCompiler -> "foo().substring(10 - 7, 10 << 2)",
        JavaScriptCompiler -> "this.foo.substring(10 - 7, 10 << 2)",
        LuaCompiler -> "string.sub(self.foo, (10 - 7) + 1, 10 << 2)", // TODO: LuaCompiler -> "string.sub(self.foo, 10 - 7 + 1, 10 << 2)",
        PerlCompiler -> "substr($self->foo(), 10 - 7, (10 << 2) - (10 - 7))",
        PHPCompiler -> "\\Kaitai\\Struct\\Stream::substring($this->foo(), 10 - 7, 10 << 2)",
        PythonCompiler -> "self.foo[10 - 7:10 << 2]",
        RubyCompiler -> "foo[10 - 7...10 << 2]"
      ))

      // substring() call with "from" using `<<` which is lower precedence than `+` or `-`: if such
      // math is used on "from" in rendering, "from" should be parenthesized (Lua)
      full("foo.substring(10 << 1, 42)", CalcStrType, CalcStrType, ResultMap(
        CppCompiler -> "foo().substr(10 << 1, 42 - (10 << 1))",
        CSharpCompiler -> "Foo.Substring(10 << 1, 42 - (10 << 1))",
        GoCompiler -> "this.Foo[10 << 1:42]",
        JavaCompiler -> "foo().substring(10 << 1, 42)",
        JavaScriptCompiler -> "this.foo.substring(10 << 1, 42)",
        LuaCompiler -> "string.sub(self.foo, (10 << 1) + 1, 42)",
        PerlCompiler -> "substr($self->foo(), 10 << 1, 42 - (10 << 1))",
        PHPCompiler -> "\\Kaitai\\Struct\\Stream::substring($this->foo(), 10 << 1, 42)",
        PythonCompiler -> "self.foo[10 << 1:42]",
        RubyCompiler -> "foo[10 << 1...42]"
      ))
    }
  }

  describe("casting") {
    describe("for user types") {
      full("other.as<block>.bar", FooBarProvider, CalcStrType, ResultMap(
        CppCompiler -> "static_cast<top_class_t::block_t*>(other())->bar()",
        CSharpCompiler -> "((TopClass.Block) (Other)).Bar",
        GoCompiler -> "this.Other.(TopClass.Block).Bar",
        JavaCompiler -> "((TopClass.Block) (other())).bar()",
        JavaScriptCompiler -> "this.other.bar",
        LuaCompiler -> "self.other.bar",
        PerlCompiler -> "$self->other()->bar()",
        PHPCompiler -> "$this->other()->bar()",
        PythonCompiler -> "self.other.bar",
        RubyCompiler -> "other.bar"
      ))

      full("other.as<block::innerblock>.baz", FooBarProvider, CalcIntType, ResultMap(
        CppCompiler -> "static_cast<top_class_t::block_t::innerblock_t*>(other())->baz()",
        CSharpCompiler -> "((TopClass.Block.Innerblock) (Other)).Baz",
        GoCompiler -> "this.Other.(TopClass.Block.Innerblock).Baz",
        JavaCompiler -> "((TopClass.Block.Innerblock) (other())).baz()",
        JavaScriptCompiler -> "this.other.baz",
        LuaCompiler -> "self.other.baz",
        PerlCompiler -> "$self->other()->baz()",
        PHPCompiler -> "$this->other()->baz()",
        PythonCompiler -> "self.other.baz",
        RubyCompiler -> "other.baz"
      ))
    }

    describe("for primitive pure types") {
      full("(1 + 2).as<s2>", CalcIntType, IntMultiType(true, Width2, None), ResultMap(
        CppCompiler -> "static_cast<int16_t>(1 + 2)",
        CSharpCompiler -> "((short) (1 + 2))",
        GoCompiler -> "int16(1 + 2)",
        JavaCompiler -> "((short) (1 + 2))",
        JavaScriptCompiler -> "1 + 2",
        LuaCompiler -> "1 + 2",
        PerlCompiler -> "1 + 2",
        PHPCompiler -> "1 + 2",
        PythonCompiler -> "1 + 2",
        RubyCompiler -> "1 + 2"
      ))
    }

    describe("for empty arrays") {
      full("[].as<bytes>", CalcIntType, CalcBytesType, ResultMap(
        CppCompiler -> "std::string(\"\", 0)",
        CSharpCompiler -> "new byte[] {  }",
        GoCompiler -> "[]uint8{}",
        JavaCompiler -> "new byte[] {  }",
        JavaScriptCompiler -> "new Uint8Array([])",
        LuaCompiler -> "\"\"",
        PerlCompiler -> "pack('C*', ())",
        PHPCompiler -> "\"\"",
        PythonCompiler -> "b\"\"",
        RubyCompiler -> "[].pack('C*')"
      ))

      full("[].as<u1[]>", CalcIntType, ArrayTypeInStream(Int1Type(false)), ResultMap(
        CppCompiler -> "std::string(\"\")",
        CSharpCompiler -> "new List<byte> {  }",
        GoCompiler -> "[]uint8{}",
        JavaCompiler -> "new ArrayList<Integer>(Arrays.asList())",
        JavaScriptCompiler -> "[]",
        LuaCompiler -> "{}",
        PerlCompiler -> "[]",
        PHPCompiler -> "[]",
        PythonCompiler -> "[]",
        RubyCompiler -> "[]"
      ))

      full("[].as<f8[]>", CalcIntType, ArrayTypeInStream(FloatMultiType(Width8, None)), ResultMap(
        CppCompiler -> "std::string(\"\", 0)",
        CSharpCompiler -> "new List<double> {  }",
        GoCompiler -> "[]float64{}",
        JavaCompiler -> "new ArrayList<Double>(Arrays.asList())",
        JavaScriptCompiler -> "[]",
        LuaCompiler -> "{}",
        PerlCompiler -> "[]",
        PHPCompiler -> "[]",
        PythonCompiler -> "[]",
        RubyCompiler -> "[]"
      ))
    }

    describe("to do type enforcement") {
      // type enforcement: casting to non-literal byte array
      full("[0 + 1, 5].as<bytes>", CalcIntType, CalcBytesType, ResultMap(
        CppCompiler -> "std::string({static_cast<char>(0 + 1), static_cast<char>(5)})",
        CSharpCompiler -> "new byte[] { 0 + 1, 5 }",
        GoCompiler -> "[]uint8{0 + 1, 5}",
        JavaCompiler -> "new byte[] { 0 + 1, 5 }",
        JavaScriptCompiler -> "new Uint8Array([0 + 1, 5])",
        LuaCompiler -> "string.char(0 + 1, 5)",
        PerlCompiler -> "pack('C*', (0 + 1, 5))",
        PHPCompiler -> "pack('C*', 0 + 1, 5)",
        PythonCompiler -> "struct.pack('2B', 0 + 1, 5)",
        RubyCompiler -> "[0 + 1, 5].pack('C*')"
      ))

      // type enforcement: casting to array of integers
      full("[0, 1, 2].as<u1[]>", CalcIntType, ArrayTypeInStream(Int1Type(false)), ResultMap(
        CSharpCompiler -> "new List<byte> { 0, 1, 2 }",
        GoCompiler -> "[]uint8{0, 1, 2}",
        JavaCompiler -> "new ArrayList<Integer>(Arrays.asList(0, 1, 2))",
        JavaScriptCompiler -> "[0, 1, 2]",
        LuaCompiler -> "{0, 1, 2}",
        PerlCompiler -> "[0, 1, 2]",
        PHPCompiler -> "[0, 1, 2]",
        PythonCompiler -> "[0, 1, 2]",
        RubyCompiler -> "[0, 1, 2]"
      ))
    }
  }

  describe("sizeof") {
    describe("of primitive types") {
      everybody("sizeof<b1>", "1", CalcIntType)
      everybody("sizeof<b7>", "1", CalcIntType)
      everybody("sizeof<b8>", "1", CalcIntType)
      everybody("sizeof<b9>", "2", CalcIntType)
      everybody("sizeof<s1>", "1", CalcIntType)
      everybody("sizeof<s2>", "2", CalcIntType)
      everybody("sizeof<u4>", "4", CalcIntType)
      everybody("sizeof<f8>", "8", CalcIntType)
    }

    describe("of fixed user type") {
      everybody("sizeof<block>", "7", CalcIntType)
    }
  }

  describe("bitsizeof") {
    describe("of primitive types") {
      everybody("bitsizeof<b1>", "1", CalcIntType)
      everybody("bitsizeof<b7>", "7", CalcIntType)
      everybody("bitsizeof<b8>", "8", CalcIntType)
      everybody("bitsizeof<b9>", "9", CalcIntType)
      everybody("bitsizeof<s1>", "8", CalcIntType)
      everybody("bitsizeof<s2>", "16", CalcIntType)
      everybody("bitsizeof<u4>", "32", CalcIntType)
      everybody("bitsizeof<f8>", "64", CalcIntType)
    }

    describe("of fixed user type") {
      everybody("bitsizeof<block>", "56", CalcIntType)
    }
  }

  describe("f-strings") {
    everybodyExcept("f\"abc\"", "\"abc\"", ResultMap(
      CppCompiler -> "std::string(\"abc\")",
      PythonCompiler -> "u\"abc\""
    ), CalcStrType)

    full("f\"abc{1}%def\"", CalcIntType, CalcStrType, ResultMap(
      CppCompiler -> "std::string(\"abc\") + kaitai::kstream::to_string(1) + std::string(\"%def\")",
      CSharpCompiler -> "\"abc\" + 1.ToString() + \"%def\"",
      GoCompiler -> "fmt.Sprintf(\"abc%v%%def\", 1)",
      JavaCompiler -> "\"abc\" + Long.toString(1) + \"%def\"",
      JavaScriptCompiler -> "\"abc\" + (1).toString() + \"%def\"",
      LuaCompiler -> "\"abc\" .. tostring(1) .. \"%def\"",
      PerlCompiler -> "\"abc\" . sprintf('%d', 1) . \"\\%def\"",
      PHPCompiler -> "\"abc\" . strval(1) . \"%def\"",
      PythonCompiler -> "u\"abc\" + str(1) + u\"%def\"",
      RubyCompiler -> "\"abc\" + 1.to_s + \"%def\"",
    ))
  }

  /**
    * Checks translation of expression `src` into target languages
    *
    * @param src KS expression to translate
    * @param tp Type model that provides information about used user-defined types in expression
    * @param expType Expected type that should be detected by [[TypeDetector]]
    * @param expOut Map with expected outputs for each language
    */
  def runTest(src: String, tp: TypeProvider, expType: DataType, expOut: ResultMap)(implicit pos: Position): Unit = {
    var eo: Option[Ast.expr] = None
    describe(src) {
      it(s"_expr:$src") {
        eo = Some(Expressions.parse(src))
      }

      val goOutput = new StringLanguageOutputWriter("  ")

      val langs = ListMap[LanguageCompilerStatic, AbstractTranslator with TypeDetector](
        CppCompiler -> new CppTranslator(tp, new CppImportList(), new CppImportList(), RuntimeConfig()),
        CSharpCompiler -> new CSharpTranslator(tp, new ImportList()),
        GoCompiler -> new GoTranslator(goOutput, tp, new ImportList()),
        JavaCompiler -> new JavaTranslator(tp, new ImportList()),
        JavaScriptCompiler -> new JavaScriptTranslator(tp, new ImportList()),
        LuaCompiler -> new LuaTranslator(tp, new ImportList()),
        PerlCompiler -> new PerlTranslator(tp, new ImportList()),
        PHPCompiler -> new PHPTranslator(tp, RuntimeConfig()),
        PythonCompiler -> new PythonTranslator(tp, new ImportList(), RuntimeConfig()),
        RubyCompiler -> new RubyTranslator(tp)
      )

      langs.foreach { case (langObj, tr) =>
        val langName = LanguageCompilerStatic.CLASS_TO_NAME(langObj)
        it(s"$langName:$src", Tag(langName), Tag(src)) {
          eo match {
            case Some(e) =>
              expOut.get(langObj) match {
                case Some(expResult) =>
                  tr.detectType(e) should be(expType)
                  val actResult1 = tr.translate(e)
                  val actResult2 = langObj match {
                    case GoCompiler => goOutput.result + actResult1
                    case _ => actResult1
                  }
                  actResult2 should be(expResult)
                case None =>
                  fail(s"no expected result, but actual result is ${tr.translate(e)}")
              }
            case None =>
              fail("expression didn't parse")
          }
        }
      }
    }
  }

  // We use `ListMap` so the tests run in the same order every time. This is to
  // give us logs that can be compared without manual sorting.
  type ResultMap = ListMap[LanguageCompilerStatic, String]
  def ResultMap(pairs: (LanguageCompilerStatic, String)*): ResultMap = ListMap(pairs: _*)

  type TestSpec = (String, TypeProvider, DataType, ResultMap)

  lazy val ALL_LANGS = LanguageCompilerStatic.NAME_TO_CLASS.values

  def full(src: String, srcType: DataType, expType: DataType, expOut: ResultMap)(implicit pos: Position) =
    runTest(src, Always(srcType), expType, expOut)

  def full(src: String, tp: TypeProvider, expType: DataType, expOut: ResultMap)(implicit pos: Position) =
    runTest(src, tp, expType, expOut)

  def everybody(src: String, expOut: String, expType: DataType = CalcIntType)(implicit pos: Position) =
    runTest(src, Always(CalcIntType), expType, ResultMap(ALL_LANGS.map((langObj) => langObj -> expOut).toSeq:_*))

  def everybodyExcept(src: String, commonExpOut: String, rm: ResultMap, expType: DataType = CalcIntType)(implicit pos: Position) =
    runTest(src, Always(CalcIntType), expType, ResultMap(ALL_LANGS.map((langObj) =>
      langObj -> rm.getOrElse(langObj, commonExpOut)
    ).toSeq:_*))
}
