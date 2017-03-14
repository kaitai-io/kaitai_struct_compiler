package io.kaitai.struct.translators

import io.kaitai.struct.{GraphvizClassCompiler, RuntimeConfig}
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.{Ast, Expressions}
import io.kaitai.struct.format.ClassSpec
import io.kaitai.struct.languages._
import io.kaitai.struct.languages.components.LanguageCompilerStatic
import org.scalatest.FunSuite
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

class TranslatorSpec extends FunSuite with TableDrivenPropertyChecks {
  val tests = Table(
    ("src", "srcType", "expType", "expOut"),

    // Integer literals + unary minus
    everybody("123", "123", Int1Type(true)),
    everybody("223", "223", Int1Type(false)),
    everybody("1234", "1234"),
    everybody("-456", "-456"),
    everybody("0x1234", "4660"),
    // less and more than 32 Bit signed int
    everybody("1000000000", "1000000000"),
    everybodyExcept("100000000000", "100000000000", Map[LanguageCompilerStatic, String](
        JavaCompiler -> "100000000000L"
    )),

    // Float literals
    everybody("1.0", "1.0", CalcFloatType),
    everybody("123.456", "123.456", CalcFloatType),
    everybody("-123.456", "-123.456", CalcFloatType),

    // Simple integer operations
    everybody("1 + 2", "(1 + 2)"),

    everybodyExcept("3 / 2", "(3 / 2)", Map(
      JavaScriptCompiler -> "Math.floor(3 / 2)",
      PerlCompiler -> "int(3 / 2)",
      PHPCompiler -> "intval(3 / 2)",
      PythonCompiler -> "3 // 2"
    )),

    everybody("1 + 2 + 5", "((1 + 2) + 5)"),

    everybodyExcept("(1 + 2) / (7 * 8)", "((1 + 2) / (7 * 8))", Map(
      JavaScriptCompiler -> "Math.floor((1 + 2) / (7 * 8))",
      PerlCompiler -> "int((1 + 2) / (7 * 8))",
      PHPCompiler -> "intval((1 + 2) / (7 * 8))",
      PythonCompiler -> "(1 + 2) // (7 * 8)"
    )),

    everybody("1 < 2", "1 < 2", CalcBooleanType),

    everybody("1 == 2", "1 == 2", CalcBooleanType),

    full("2 < 3 ? \"foo\" : \"bar\"", CalcIntType, CalcStrType, Map[LanguageCompilerStatic, String](
      CppCompiler -> "(2 < 3) ? (std::string(\"foo\")) : (std::string(\"bar\"))",
      CSharpCompiler -> "2 < 3 ? \"foo\" : \"bar\"",
      JavaCompiler -> "2 < 3 ? \"foo\" : \"bar\"",
      JavaScriptCompiler -> "2 < 3 ? \"foo\" : \"bar\"",
      PerlCompiler -> "2 < 3 ? \"foo\" : \"bar\"",
      PHPCompiler -> "2 < 3 ? \"foo\" : \"bar\"",
      PythonCompiler -> "u\"foo\" if 2 < 3 else u\"bar\"",
      RubyCompiler -> "2 < 3 ? \"foo\" : \"bar\""
    )),

    everybody("~777", "~777"),
    everybody("~(7+3)", "~(7 + 3)"),

    // Simple float operations
    everybody("1.2 + 3.4", "(1.2 + 3.4)", CalcFloatType),
    everybody("1.2 + 3", "(1.2 + 3)", CalcFloatType),
    everybody("1 + 3.4", "(1 + 3.4)", CalcFloatType),

    everybody("1.0 < 2", "1.0 < 2", CalcBooleanType),

    everybody("3 / 2.0", "(3 / 2.0)", CalcFloatType),

    everybody("(1 + 2) / (7 * 8.1)", "((1 + 2) / (7 * 8.1))", CalcFloatType),

    // Boolean literals
    full("true", CalcBooleanType, CalcBooleanType, Map[LanguageCompilerStatic, String](
      CppCompiler -> "true",
      CSharpCompiler -> "true",
      JavaCompiler -> "true",
      JavaScriptCompiler -> "true",
      PerlCompiler -> "1",
      PHPCompiler -> "true",
      PythonCompiler -> "True",
      RubyCompiler -> "true"
    )),

    full("false", CalcBooleanType, CalcBooleanType, Map[LanguageCompilerStatic, String](
      CppCompiler -> "false",
      CSharpCompiler -> "false",
      JavaCompiler -> "false",
      JavaScriptCompiler -> "false",
      PerlCompiler -> "0",
      PHPCompiler -> "false",
      PythonCompiler -> "False",
      RubyCompiler -> "false"
    )),

    full("some_bool.to_i", CalcBooleanType, CalcIntType, Map[LanguageCompilerStatic, String](
      CppCompiler -> "some_bool()",
      CSharpCompiler -> "(SomeBool ? 1 : 0)",
      JavaCompiler -> "(someBool() ? 1 : 0)",
      JavaScriptCompiler -> "(this.someBool | 0)",
      PerlCompiler -> "$self->some_bool()",
      PHPCompiler -> "intval($this->someBool())",
      PythonCompiler -> "int(self.some_bool)",
      RubyCompiler -> "(some_bool ? 1 : 0)"
    )),

    // Member access
    full("foo_str", CalcStrType, CalcStrType, Map[LanguageCompilerStatic, String](
      CppCompiler -> "foo_str()",
      CSharpCompiler -> "FooStr",
      JavaCompiler -> "fooStr()",
      JavaScriptCompiler -> "this.fooStr",
      PerlCompiler -> "$self->foo_str()",
      PHPCompiler -> "$this->fooStr()",
      PythonCompiler -> "self.foo_str",
      RubyCompiler -> "foo_str"
    )),

    full("foo_block", userType("block"), userType("block"), Map[LanguageCompilerStatic, String](
      CppCompiler -> "foo_block()",
      CSharpCompiler -> "FooBlock",
      JavaCompiler -> "fooBlock()",
      JavaScriptCompiler -> "this.fooBlock",
      PerlCompiler -> "$self->foo_block()",
      PHPCompiler -> "$this->fooBlock()",
      PythonCompiler -> "self.foo_block",
      RubyCompiler -> "foo_block"
    )),

    full("foo.bar", FooBarProvider, CalcStrType, Map[LanguageCompilerStatic, String](
      CppCompiler -> "foo()->bar()",
      CSharpCompiler -> "Foo.Bar",
      JavaCompiler -> "foo().bar()",
      JavaScriptCompiler -> "this.foo.bar",
      PerlCompiler -> "$self->foo()->bar()",
      PHPCompiler -> "$this->foo()->bar()",
      PythonCompiler -> "self.foo.bar",
      RubyCompiler -> "foo.bar"
    )),

    full("foo.inner.baz", FooBarProvider, CalcIntType, Map[LanguageCompilerStatic, String](
      CppCompiler -> "foo()->inner()->baz()",
      CSharpCompiler -> "Foo.Inner.Baz",
      JavaCompiler -> "foo().inner().baz()",
      JavaScriptCompiler -> "this.foo.inner.baz",
      PerlCompiler -> "$self->foo()->inner()->baz()",
      PHPCompiler -> "$this->foo()->inner()->baz()",
      PythonCompiler -> "self.foo.inner.baz",
      RubyCompiler -> "foo.inner.baz"
    )),

    full("_root.foo", userType("block"), userType("block"), Map[LanguageCompilerStatic, String](
      CppCompiler -> "_root()->foo()",
      CSharpCompiler -> "M_Root.Foo",
      JavaCompiler -> "_root.foo()",
      JavaScriptCompiler -> "this._root.foo",
      PerlCompiler -> "$self->_root()->foo()",
      PHPCompiler -> "$this->_root()->foo()",
      PythonCompiler -> "self._root.foo",
      RubyCompiler -> "_root.foo"
    )),

    full("a != 2 and a != 5", CalcIntType, CalcBooleanType, Map[LanguageCompilerStatic, String](
      CppCompiler -> "a() != 2 && a() != 5",
      CSharpCompiler -> "A != 2 && A != 5",
      JavaCompiler -> "a() != 2 && a() != 5",
      JavaScriptCompiler -> "this.a != 2 && this.a != 5",
      PerlCompiler -> "$self->a() != 2 && $self->a() != 5",
      PHPCompiler -> "$this->a() != 2 && $this->a() != 5",
      PythonCompiler -> "self.a != 2 and self.a != 5",
      RubyCompiler -> "a != 2 && a != 5"
    )),

    // Arrays
    full("[0, 1, 100500]", CalcIntType, ArrayType(CalcIntType), Map[LanguageCompilerStatic, String](
      CSharpCompiler -> "new List<int> { 0, 1, 100500 }",
      JavaCompiler -> "new ArrayList<Integer>(Arrays.asList(0, 1, 100500))",
      JavaScriptCompiler -> "[0, 1, 100500]",
      PerlCompiler -> "(0, 1, 100500)",
      PHPCompiler -> "[0, 1, 100500]",
      PythonCompiler -> "[0, 1, 100500]",
      RubyCompiler -> "[0, 1, 100500]"
    )),

    full("[34, 0, 10, 64, 65, 66, 92]", CalcIntType, CalcBytesType, Map[LanguageCompilerStatic, String](
      CppCompiler -> "std::string(\"\\x22\\x00\\x0A\\x40\\x41\\x42\\x5C\", 7)",
      CSharpCompiler -> "new byte[] { 34, 0, 10, 64, 65, 66, 92 }",
      JavaCompiler -> "new byte[] { 34, 0, 10, 64, 65, 66, 92 }",
      JavaScriptCompiler -> "[34, 0, 10, 64, 65, 66, 92]",
      PerlCompiler -> "pack('C*', (34, 0, 10, 64, 65, 66, 92))",
      PHPCompiler -> "\"\\x22\\x00\\x0A\\x40\\x41\\x42\\x5C\"",
      PythonCompiler -> "struct.pack('7b', 34, 0, 10, 64, 65, 66, 92)",
      RubyCompiler -> "[34, 0, 10, 64, 65, 66, 92].pack('C*')"
    )),

    full("[255, 0, 255]", CalcIntType, CalcBytesType, Map[LanguageCompilerStatic, String](
      CppCompiler -> "std::string(\"\\xFF\\x00\\xFF\", 3)",
      CSharpCompiler -> "new byte[] { 255, 0, 255 }",
      JavaCompiler -> "new byte[] { -1, 0, -1 }",
      JavaScriptCompiler -> "[255, 0, 255]",
      PerlCompiler -> "pack('C*', (255, 0, 255))",
      PHPCompiler -> "\"\\xFF\\x00\\xFF\"",
      PythonCompiler -> "struct.pack('3b', -1, 0, -1)",
      RubyCompiler -> "[255, 0, 255].pack('C*')"
    )),

    full("a[42]", ArrayType(CalcStrType), CalcStrType, Map[LanguageCompilerStatic, String](
      CppCompiler -> "a()->at(42)",
      CSharpCompiler -> "A[42]",
      JavaCompiler -> "a().get(42)",
      JavaScriptCompiler -> "this.a[42]",
      PythonCompiler -> "self.a[42]",
      RubyCompiler -> "a[42]"
    )),

    full("a[42 - 2]", ArrayType(CalcStrType), CalcStrType, Map[LanguageCompilerStatic, String](
      CppCompiler -> "a()->at((42 - 2))",
      CSharpCompiler -> "A[(42 - 2)]",
      JavaCompiler -> "a().get((42 - 2))",
      JavaScriptCompiler -> "this.a[(42 - 2)]",
      PythonCompiler -> "self.a[(42 - 2)]",
      RubyCompiler -> "a[(42 - 2)]"
    )),

    full("a.first", ArrayType(CalcIntType), CalcIntType, Map[LanguageCompilerStatic, String](
      CppCompiler -> "a()->front()",
      CSharpCompiler -> "A[0]",
      JavaCompiler -> "a().get(0)",
      JavaScriptCompiler -> "this.a[0]",
      PythonCompiler -> "self.a[0]",
      RubyCompiler -> "a.first"
    )),

    full("a.last", ArrayType(CalcIntType), CalcIntType, Map[LanguageCompilerStatic, String](
      CppCompiler -> "a()->back()",
      CSharpCompiler -> "A[A.Length - 1]",
      JavaCompiler -> "a().get(a().size() - 1)",
      JavaScriptCompiler -> "this.a[this.a.length - 1]",
      PythonCompiler -> "self.a[-1]",
      RubyCompiler -> "a.last"
    )),

    full("a.size", ArrayType(CalcIntType), CalcIntType, Map[LanguageCompilerStatic, String](
        CppCompiler -> "a()->size()",
        CSharpCompiler -> "A.Count",
        JavaCompiler -> "a().size()",
        JavaScriptCompiler -> "this.a.length",
        PHPCompiler -> "count(a)",
        PerlCompiler -> "scalar($self->a())",
        PythonCompiler -> "len(self.a)",
        RubyCompiler -> "a.length"
      )),

    // Strings
    full("\"str\"", CalcIntType, CalcStrType, Map[LanguageCompilerStatic, String](
      CppCompiler -> "std::string(\"str\")",
      CSharpCompiler -> "\"str\"",
      JavaCompiler -> "\"str\"",
      JavaScriptCompiler -> "\"str\"",
      PerlCompiler -> "\"str\"",
      PHPCompiler -> "\"str\"",
      PythonCompiler -> "u\"str\"",
      RubyCompiler -> "\"str\""
    )),

    full("\"str\\nnext\"", CalcIntType, CalcStrType, Map[LanguageCompilerStatic, String](
      CppCompiler -> "std::string(\"str\\nnext\")",
      CSharpCompiler -> "\"str\\nnext\"",
      JavaCompiler -> "\"str\\nnext\"",
      JavaScriptCompiler -> "\"str\\nnext\"",
      PerlCompiler -> "\"str\\nnext\"",
      PHPCompiler -> "\"str\\nnext\"",
      PythonCompiler -> "u\"str\\nnext\"",
      RubyCompiler -> "\"str\\nnext\""
    )),

    full("\"str\\u000anext\"", CalcIntType, CalcStrType, Map[LanguageCompilerStatic, String](
      CppCompiler -> "std::string(\"str\\nnext\")",
      CSharpCompiler -> "\"str\\nnext\"",
      JavaCompiler -> "\"str\\nnext\"",
      JavaScriptCompiler -> "\"str\\nnext\"",
      PerlCompiler -> "\"str\\nnext\"",
      PHPCompiler -> "\"str\\nnext\"",
      PythonCompiler -> "u\"str\\nnext\"",
      RubyCompiler -> "\"str\\nnext\""
    )),

    full("\"str\\0next\"", CalcIntType, CalcStrType, Map[LanguageCompilerStatic, String](
      CppCompiler -> "std::string(\"str\\000next\", 8)",
      CSharpCompiler -> "\"str\\0next\"",
      JavaCompiler -> "\"str\\000next\"",
      JavaScriptCompiler -> "\"str\\000next\"",
      PerlCompiler -> "\"str\\000next\"",
      PHPCompiler -> "\"str\\000next\"",
      PythonCompiler -> "u\"str\\000next\"",
      RubyCompiler -> "\"str\\000next\""
    )),

    everybodyExcept("\"str1\" + \"str2\"", "\"str1\" + \"str2\"", Map[LanguageCompilerStatic, String](
      CppCompiler -> "std::string(\"str1\") + std::string(\"str2\")",
      PerlCompiler -> "\"str1\" . \"str2\"",
      PHPCompiler -> "\"str1\" . \"str2\"",
      PythonCompiler -> "u\"str1\" + u\"str2\""
    ), CalcStrType),

    everybodyExcept("\"str1\" == \"str2\"", "\"str1\" == \"str2\"", Map[LanguageCompilerStatic, String](
      CppCompiler -> "std::string(\"str1\") == (std::string(\"str2\"))",
      JavaCompiler -> "\"str1\".equals(\"str2\")",
      PerlCompiler -> "\"str1\" eq \"str2\"",
      PythonCompiler -> "u\"str1\" == u\"str2\""
    ), CalcBooleanType),

    everybodyExcept("\"str1\" != \"str2\"", "\"str1\" != \"str2\"", Map[LanguageCompilerStatic, String](
      CppCompiler -> "std::string(\"str1\") != std::string(\"str2\")",
      JavaCompiler -> "!(\"str1\").equals(\"str2\")",
      PerlCompiler -> "\"str1\" ne \"str2\"",
      PythonCompiler -> "u\"str1\" != u\"str2\""
    ), CalcBooleanType),

    everybodyExcept("\"str1\" < \"str2\"", "\"str1\" < \"str2\"", Map[LanguageCompilerStatic, String](
      CppCompiler -> "(std::string(\"str1\").compare(std::string(\"str2\")) < 0)",
      CSharpCompiler -> "(\"str1\".CompareTo(\"str2\") < 0)",
      JavaCompiler -> "(\"str1\".compareTo(\"str2\") < 0)",
      PerlCompiler -> "\"str1\" lt \"str2\"",
      PythonCompiler -> "u\"str1\" < u\"str2\""
    ), CalcBooleanType),

    full("\"str\".length", CalcIntType, CalcIntType, Map[LanguageCompilerStatic, String](
      CppCompiler -> "std::string(\"str\").length()",
      CSharpCompiler -> "\"str\".Length",
      JavaCompiler -> "\"str\".length()",
      JavaScriptCompiler -> "\"str\".length",
      PerlCompiler -> "length(\"str\")",
      PHPCompiler -> "strlen(\"str\")",
      PythonCompiler -> "len(u\"str\")",
      RubyCompiler -> "\"str\".size"
    )),

    full("\"str\".reverse", CalcIntType, CalcStrType, Map[LanguageCompilerStatic, String](
        CppCompiler -> "kaitai::kstream::reverse(std::string(\"str\"))",
        CSharpCompiler -> "new string(Array.Reverse(\"str\".ToCharArray()))",
        JavaCompiler -> "new StringBuilder(\"str\").reverse().toString()",
        JavaScriptCompiler -> "Array.from(\"str\").reverse().join('')",
        PerlCompiler -> "scalar(reverse(\"str\"))",
        PHPCompiler -> "strrev(\"str\")",
        PythonCompiler -> "u\"str\"[::-1]",
        RubyCompiler -> "\"str\".reverse"
      )),

    full("\"12345\".to_i", CalcIntType, CalcIntType, Map[LanguageCompilerStatic, String](
      CppCompiler -> "std::stoi(std::string(\"12345\"))",
      CSharpCompiler -> "Convert.ToInt64(\"12345\", 10)",
      JavaCompiler -> "Long.parseLong(\"12345\", 10)",
      JavaScriptCompiler -> "Number.parseInt(\"12345\", 10)",
      PerlCompiler -> "\"12345\"",
      PHPCompiler -> "intval(\"12345\", 10)",
      PythonCompiler -> "int(u\"12345\")",
      RubyCompiler -> "\"12345\".to_i"
    )),

    full("\"1234fe\".to_i(16)", CalcIntType, CalcIntType, Map[LanguageCompilerStatic, String](
      CppCompiler -> "std::stoi(std::string(\"1234fe\"), 0, 16)",
      CSharpCompiler -> "Convert.ToInt64(\"1234fe\", 16)",
      JavaCompiler -> "Long.parseLong(\"1234fe\", 16)",
      JavaScriptCompiler -> "Number.parseInt(\"1234fe\", 16)",
      PerlCompiler -> "hex(\"1234fe\")",
      PHPCompiler -> "intval(\"1234fe\", 16)",
      PythonCompiler -> "int(u\"1234fe\", 16)",
      RubyCompiler -> "\"1234fe\".to_i(16)"
    )),

    // casts
    full("other.as<block>.bar", FooBarProvider, CalcStrType, Map[LanguageCompilerStatic, String](
      CppCompiler -> "static_cast<block_t*>(other())->bar()",
      CSharpCompiler -> "((Block) (Other)).Bar",
      JavaCompiler -> "((Block) (other())).bar()",
      JavaScriptCompiler -> "this.other.bar",
      PerlCompiler -> "$self->other()->bar()",
      PHPCompiler -> "$this->other()->bar()",
      PythonCompiler -> "self.other.bar",
      RubyCompiler -> "other.bar"
    )),

    // very simple workaround for Scala not having optional trailing commas
    everybody("999", "999")
  )

  for ((src, tp, expType, expOut) <- tests) {
    var eo: Option[Ast.expr] = None
    test(s"_expr:$src") {
      eo = Some(Expressions.parse(src))
    }

    LanguageCompilerStatic.NAME_TO_CLASS.
      filter { case (_, langObj) => langObj != GraphvizClassCompiler }.
      foreach { case (langName, langObj) =>
      test(s"$langName:$src") {
        eo match {
          case Some(e) =>
            val tr: BaseTranslator = langObj.getTranslator(tp, RuntimeConfig())
            expOut.get(langObj) match {
              case Some(expResult) =>
                tr.detectType(e) should be(expType)
                tr.translate(e) should be(expResult)
              case None =>
                fail("no expected result")
            }
          case None =>
            fail("expression didn't parse")
        }
      }
    }
  }

  type ResultMap = Map[LanguageCompilerStatic, String]
  type TestSpec = (String, TypeProvider, DataType, ResultMap)

  abstract class FakeTypeProvider extends TypeProvider {
    val nowClass = ClassSpec.opaquePlaceholder(List("top_class"))

    override def resolveEnum(enumName: String) =
      throw new NotImplementedError

    override def resolveType(typeName: String): DataType =
      throw new NotImplementedError
  }

  case class Always(t: DataType) extends FakeTypeProvider {
    override def determineType(name: String): DataType = t
    override def determineType(inClass: ClassSpec, name: String): DataType = t
  }

  case object FooBarProvider extends FakeTypeProvider {
    override def determineType(name: String): DataType = {
      name match {
        case "foo" => userType("block")
      }
    }

    override def determineType(inClass: ClassSpec, name: String): DataType = {
      (inClass.name, name) match {
        case (List("block"), "bar") => CalcStrType
        case (List("block"), "inner") => userType("innerblock")
        case (List("innerblock"), "baz") => CalcIntType
      }
    }

    override def resolveType(typeName: String): DataType = {
      typeName match {
        case "top_class" | "block" | "innerblock" => userType(typeName)
      }
    }
  }

  def userType(name: String) = {
    val lname = List(name)
    val cs = ClassSpec.opaquePlaceholder(lname)
    val ut = UserTypeInstream(lname, None)
    ut.classSpec = Some(cs)
    ut
  }

  lazy val ALL_LANGS = LanguageCompilerStatic.NAME_TO_CLASS.values

  def full(src: String, srcType: DataType, expType: DataType, expOut: ResultMap): TestSpec =
    (src, Always(srcType), expType, expOut)

  def full(src: String, tp: TypeProvider, expType: DataType, expOut: ResultMap): TestSpec =
    (src, tp, expType, expOut)

  def everybody(src: String, expOut: String, expType: DataType = CalcIntType): TestSpec = {
    (src, Always(CalcIntType), expType, ALL_LANGS.map((langObj) => langObj -> expOut).toMap)
  }

  def everybodyExcept(src: String, commonExpOut: String, rm: ResultMap, expType: DataType = CalcIntType): TestSpec = {
    (src, Always(CalcIntType), expType, ALL_LANGS.map((langObj) =>
      langObj -> rm.getOrElse(langObj, commonExpOut)
    ).toMap)
  }
}
