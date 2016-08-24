package io.kaitai.struct.translators

import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.exprlang.{Ast, Expressions}
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

    // Float literals
    everybody("1.0", "1.0", CalcFloatType),
    everybody("123.456", "123.456", CalcFloatType),
    everybody("-123.456", "-123.456", CalcFloatType),

    // Simple integer operations
    everybody("1 + 2", "(1 + 2)"),

    everybodyExcept("3 / 2", "(3 / 2)", Map(
      JavaScriptCompiler -> "Math.floor(3 / 2)"
    )),

    everybody("1 + 2 + 5", "((1 + 2) + 5)"),

    everybodyExcept("(1 + 2) / (7 * 8)", "((1 + 2) / (7 * 8))", Map(
      JavaScriptCompiler -> "Math.floor((1 + 2) / (7 * 8))"
    )),

    everybody("1 < 2", "1 < 2", BooleanType),

    full("2 < 3 ? \"foo\" : \"bar\"", CalcIntType, CalcStrType, Map(
      CSharpCompiler -> "2 < 3 ? \"foo\" : \"bar\"",
      JavaCompiler -> "2 < 3 ? \"foo\" : \"bar\"",
      JavaScriptCompiler -> "2 < 3 ? \"foo\" : \"bar\"",
      PythonCompiler -> "u\"foo\" if 2 < 3 else u\"bar\""
    )),

    everybody("~777", "~777"),
    everybody("~(7+3)", "~(7 + 3)"),

    // Simple float operations
    everybody("1.2 + 3.4", "(1.2 + 3.4)", CalcFloatType),
    everybody("1.2 + 3", "(1.2 + 3)", CalcFloatType),
    everybody("1 + 3.4", "(1 + 3.4)", CalcFloatType),

    everybody("1.0 < 2", "1.0 < 2", BooleanType),

    everybody("3 / 2.0", "(3 / 2.0)", CalcFloatType),

    everybody("(1 + 2) / (7 * 8.1)", "((1 + 2) / (7 * 8.1))", CalcFloatType),

    // Boolean literals
    full("true", BooleanType, BooleanType, Map(
      CppCompiler -> "true",
      CSharpCompiler -> "true",
      JavaCompiler -> "true",
      JavaScriptCompiler -> "true",
      PerlCompiler -> "1",
      PHPCompiler -> "TRUE",
      PythonCompiler -> "True",
      RubyCompiler -> "true"
    )),

    full("false", BooleanType, BooleanType, Map(
      CppCompiler -> "false",
      CSharpCompiler -> "false",
      JavaCompiler -> "false",
      JavaScriptCompiler -> "false",
      PerlCompiler -> "0",
      PHPCompiler -> "FALSE",
      PythonCompiler -> "False",
      RubyCompiler -> "false"
    )),

    // Member access
    full("foo_str", CalcStrType, CalcStrType, Map(
      CppCompiler -> "foo_str()",
      CSharpCompiler -> "FooStr",
      JavaCompiler -> "fooStr()",
      JavaScriptCompiler -> "this.fooStr",
      PythonCompiler -> "self.foo_str",
      RubyCompiler -> "foo_str"
    )),

    full("foo_block", userType("block"), userType("block"), Map(
      CppCompiler -> "foo_block()",
      CSharpCompiler -> "FooBlock",
      JavaCompiler -> "fooBlock()",
      JavaScriptCompiler -> "this.fooBlock",
      PythonCompiler -> "self.foo_block",
      RubyCompiler -> "foo_block"
    )),

    full("foo.bar", FooBarProvider, CalcStrType, Map(
      CSharpCompiler -> "Foo.Bar",
      JavaCompiler -> "foo().bar()",
      JavaScriptCompiler -> "this.foo.bar",
      PythonCompiler -> "self.foo.bar",
      RubyCompiler -> "foo.bar"
    )),

    full("foo.inner.baz", FooBarProvider, CalcIntType, Map(
      CSharpCompiler -> "Foo.Inner.Baz",
      JavaCompiler -> "foo().inner().baz()",
      JavaScriptCompiler -> "this.foo.inner.baz",
      PythonCompiler -> "self.foo.inner.baz",
      RubyCompiler -> "foo.inner.baz"
    )),

    full("_root.foo", userType("block"), userType("block"), Map(
      CSharpCompiler -> "M_Root.Foo",
      JavaCompiler -> "_root.foo()",
      JavaScriptCompiler -> "this._root.foo",
      PythonCompiler -> "self._root.foo",
      RubyCompiler -> "_root.foo"
    )),

    full("a != 2 and a != 5", CalcIntType, BooleanType, Map(
      CSharpCompiler -> "A != 2 && A != 5",
      JavaCompiler -> "a() != 2 && a() != 5",
      JavaScriptCompiler -> "this.a != 2 && this.a != 5",
      PythonCompiler -> "self.a != 2 and self.a != 5",
      RubyCompiler -> "a != 2 && a != 5"
    )),

    // Arrays
    full("[0, 1, 100500]", CalcIntType, ArrayType(CalcIntType), Map(
      CSharpCompiler -> "new List<int> { 0, 1, 100500 }",
      JavaCompiler -> "new ArrayList<Integer>(Arrays.asList(0, 1, 100500))",
      JavaScriptCompiler -> "[0, 1, 100500]",
      PythonCompiler -> "[0, 1, 100500]",
      RubyCompiler -> "[0, 1, 100500]"
    )),

    full("[34, 0, 10, 64, 65, 66, 92]", CalcIntType, CalcBytesType, Map(
      CppCompiler -> "std::string(\"\\x22\\x00\\x0A\\x40\\x41\\x42\\x5C\", 7)",
      CSharpCompiler -> "new byte[] { 34, 0, 10, 64, 65, 66, 92 }",
      JavaCompiler -> "new byte[] { (byte) 34, (byte) 0, (byte) 10, (byte) 64, (byte) 65, (byte) 66, (byte) 92 }",
      JavaScriptCompiler -> "[34, 0, 10, 64, 65, 66, 92]",
      PythonCompiler -> "str(bytearray([34, 0, 10, 64, 65, 66, 92]))",
      RubyCompiler -> "[34, 0, 10, 64, 65, 66, 92].pack('C*')"
    )),

    full("a[42]", ArrayType(CalcStrType), CalcStrType, Map(
      CSharpCompiler -> "A[42]",
      JavaCompiler -> "a().get(42)",
      JavaScriptCompiler -> "this.a[42]",
      PythonCompiler -> "self.a[42]",
      RubyCompiler -> "a[42]"
    )),

    full("a[42 - 2]", ArrayType(CalcStrType), CalcStrType, Map(
      CSharpCompiler -> "A[(42 - 2)]",
      JavaCompiler -> "a().get((42 - 2))",
      JavaScriptCompiler -> "this.a[(42 - 2)]",
      PythonCompiler -> "self.a[(42 - 2)]",
      RubyCompiler -> "a[(42 - 2)]"
    )),

    full("a.first", ArrayType(CalcIntType), CalcIntType, Map(
      CppCompiler -> "m_a[0]",
      CSharpCompiler -> "A[0]",
      JavaCompiler -> "a().get(0)",
      JavaScriptCompiler -> "this.a[0]",
      PythonCompiler -> "self.a[0]",
      RubyCompiler -> "a.first"
    )),

    full("a.last", ArrayType(CalcIntType), CalcIntType, Map(
      CSharpCompiler -> "A[A.Length - 1]",
      JavaCompiler -> "a().get(a().size() - 1)",
      JavaScriptCompiler -> "this.a[this.a.length - 1]",
      PythonCompiler -> "self.a[-1]",
      RubyCompiler -> "a.last"
    )),

    // very simple workaround for Scala not having optional trailing commas
    everybody("999", "999")
  )

  for ((src, tp, expType, expOut) <- tests) {
    var eo: Option[Ast.expr] = None
    test(s"_expr:$src") {
      eo = Some(Expressions.parse(src))
    }

    LanguageCompilerStatic.NAME_TO_CLASS.foreach { case (langName, langObj) =>
      test(s"$langName:$src") {
        eo match {
          case Some(e) =>
            val tr: BaseTranslator = langObj.getTranslator(tp)
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
  type TestSpec = (String, TypeProvider, BaseType, ResultMap)

  case class Always(t: BaseType) extends TypeProvider {
    override def determineType(name: String): BaseType = t
    override def determineType(parentType: List[String], name: String): BaseType = t
  }

  case object FooBarProvider extends TypeProvider {
    override def determineType(name: String): BaseType = {
      name match {
        case "foo" => userType("block")
      }
    }

    override def determineType(parentType: List[String], name: String): BaseType = {
      (parentType, name) match {
        case (List("block"), "bar") => CalcStrType
        case (List("block"), "inner") => userType("innerblock")
        case (List("innerblock"), "baz") => CalcIntType
      }
    }
  }

  def userType(name: String) = UserTypeInstream(List(name))

  lazy val ALL_LANGS = LanguageCompilerStatic.NAME_TO_CLASS.values

  def full(src: String, srcType: BaseType, expType: BaseType, expOut: ResultMap): TestSpec =
    (src, Always(srcType), expType, expOut)

  def full(src: String, tp: TypeProvider, expType: BaseType, expOut: ResultMap): TestSpec =
    (src, tp, expType, expOut)

  def everybody(src: String, expOut: String, expType: BaseType = CalcIntType): TestSpec = {
    (src, Always(CalcIntType), expType, ALL_LANGS.map((langObj) => langObj -> expOut).toMap)
  }

  def everybodyExcept(src: String, commonExpOut: String, rm: ResultMap, expType: BaseType = CalcIntType): TestSpec = {
    (src, Always(CalcIntType), expType, ALL_LANGS.map((langObj) =>
      langObj -> rm.getOrElse(langObj, commonExpOut)
    ).toMap)
  }
}
