package io.kaitai.struct

import io.kaitai.struct.datatype.{BigEndian, BigBitEndian}
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.{Ast, Expressions}
import io.kaitai.struct.format.{DynamicSized, FixedSized, MetaSpec, NamedIdentifier, Sized, YamlAttrArgs}
import io.kaitai.struct.format.{RepeatSpec, NoRepeat, RepeatExpr, RepeatUntil, RepeatEos}
import io.kaitai.struct.precompile.CalculateSeqSizes
import io.kaitai.struct.problems.CompilationProblemException

import scala.collection.immutable.SortedMap

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class CalculateSeqSizes$Test extends AnyFunSpec {
  private def parse(
    typeName: Option[String],
    size: Option[Int],
    terminator: Option[Seq[Byte]],
    contents: Option[Array[Byte]],
  ): DataType = {
    DataType.fromYaml(
      typeName,
      List(),
      MetaSpec(
        List(),             // path
        false,              // isOpaque
        None,               // id
        Some(BigEndian),    // endian
        Some(BigBitEndian), // bitEndian
        Some("utf-8"),      // encoding
        false,              // forceDebug
        None,               // opaqueTypes
        None,               // zeroCopySubstream
        List()              // imports
      ),
      YamlAttrArgs(
        size.map(s => Ast.expr.IntNum(s)),
        false,// sizeEos
        None, // encoding
        terminator,
        false,// include
        false,// consume
        false,// eosError
        None, // padRight
        contents,
        None, // enumRef
        None, // parent
        None, // process
      )
    )
  }
  private def sizeof(
    typeName: Option[String],
    size: Option[Int],
    terminator: Option[Seq[Byte]],
    contents: Option[Array[Byte]],
  ): Sized = {
    CalculateSeqSizes.dataTypeBitsSize(parse(typeName, size, terminator, contents))
  }
  /** Helper for testing built-in types */
  private def sizeof(typeName: String): Sized = {
    sizeof(Some(typeName), None, None, None)
  }
  /** Helper for testing unsized built-in types which requires explicit size boundary. */
  private def sizeof(typeName: String, terminator: Seq[Byte]): Sized = {
    sizeof(Some(typeName), None, Some(terminator), None)
  }
  /** Helper for testing the unnamed "bytes" built-in type defined implicitly via `size` key. */
  private def sizeof(size: Int): Sized = {
    sizeof(None, Some(size), None, None)
  }
  /** Helper for testing the `contents` size. */
  private def sizeof(contents: Array[Byte]): Sized = {
    sizeof(None, None, None, Some(contents))
  }

  /** Helper for testing the `switch-on` types. */
  private def switchOn(cases: SortedMap[String, String]): Sized = {
    CalculateSeqSizes.dataTypeBitsSize(SwitchType(
      NamedIdentifier("body"),
      Ast.expr.IntNum(0),
      cases.map { case (condition, typeName) =>
        Expressions.parse(condition) -> parse(Some(typeName), None, None, None)
      }
    ))
  }

  private def repeat(size: Sized, repeat: RepeatSpec): Sized = {
    CalculateSeqSizes.sizeMultiply(size, repeat, List())
  }

  describe("CalculateSeqSizes") {
    it("built-in types has correct size") {
      sizeof("s1") should be (FixedSized( 8))
      sizeof("s2") should be (FixedSized(16))
      sizeof("s4") should be (FixedSized(32))
      sizeof("s8") should be (FixedSized(64))

      sizeof("s2be") should be (FixedSized(16))
      sizeof("s4be") should be (FixedSized(32))
      sizeof("s8be") should be (FixedSized(64))

      sizeof("s2le") should be (FixedSized(16))
      sizeof("s4le") should be (FixedSized(32))
      sizeof("s8le") should be (FixedSized(64))

      //-----------------------------------------------------------------------

      sizeof("u1") should be (FixedSized( 8))
      sizeof("u2") should be (FixedSized(16))
      sizeof("u4") should be (FixedSized(32))
      sizeof("u8") should be (FixedSized(64))

      sizeof("u2be") should be (FixedSized(16))
      sizeof("u4be") should be (FixedSized(32))
      sizeof("u8be") should be (FixedSized(64))

      sizeof("u2le") should be (FixedSized(16))
      sizeof("u4le") should be (FixedSized(32))
      sizeof("u8le") should be (FixedSized(64))

      //-----------------------------------------------------------------------

      sizeof("f4") should be (FixedSized(32))
      sizeof("f8") should be (FixedSized(64))

      sizeof("f4be") should be (FixedSized(32))
      sizeof("f8be") should be (FixedSized(64))

      sizeof("f4le") should be (FixedSized(32))
      sizeof("f8le") should be (FixedSized(64))

      //-----------------------------------------------------------------------

      sizeof("b1") should be (FixedSized(1))
      sizeof("b2") should be (FixedSized(2))
      sizeof("b3") should be (FixedSized(3))
      sizeof("b4") should be (FixedSized(4))
      sizeof("b5") should be (FixedSized(5))
      sizeof("b6") should be (FixedSized(6))
      sizeof("b7") should be (FixedSized(7))
      sizeof("b8") should be (FixedSized(8))
      sizeof("b9") should be (FixedSized(9))

      sizeof("b2be") should be (FixedSized(2))
      sizeof("b3be") should be (FixedSized(3))
      sizeof("b4be") should be (FixedSized(4))
      sizeof("b5be") should be (FixedSized(5))
      sizeof("b6be") should be (FixedSized(6))
      sizeof("b7be") should be (FixedSized(7))
      sizeof("b8be") should be (FixedSized(8))
      sizeof("b9be") should be (FixedSized(9))

      sizeof("b2le") should be (FixedSized(2))
      sizeof("b3le") should be (FixedSized(3))
      sizeof("b4le") should be (FixedSized(4))
      sizeof("b5le") should be (FixedSized(5))
      sizeof("b6le") should be (FixedSized(6))
      sizeof("b7le") should be (FixedSized(7))
      sizeof("b8le") should be (FixedSized(8))
      sizeof("b9le") should be (FixedSized(9))

      //-----------------------------------------------------------------------

      sizeof("str", Seq(0)) should be (DynamicSized)
      sizeof("strz"       ) should be (DynamicSized)

      //TODO: Uncomment when https://github.com/kaitai-io/kaitai_struct/issues/799
      // will be implemented
      // sizeof("bytes") should be (DynamicSized)
      sizeof(10) should be (FixedSized(10*8))// size: 10

      sizeof("abcdef".getBytes()) should be (FixedSized(6*8))// content: 'abcdef'
    }

    describe("switch-on") {
      it("has a zero size if no cases present") {
        switchOn(SortedMap()) should be (FixedSized(0))
      }

      it("has a fixed size when all cases have the same fixed size") {
        switchOn(SortedMap(
          "0" -> "f4be",
          "1" -> "u4be",
          "_" -> "s4be",
        )) should be (FixedSized(4*8))
      }

      it("has a dynamic size when not all cases have the same size") {
        switchOn(SortedMap(
          "0" -> "f4be",
          "1" -> "u4be",
          "_" -> "u1",
        )) should be (DynamicSized)
      }

      it("has a dynamic size when contains a case with a dynamic size") {
        // Fixed + Dynamic
        switchOn(SortedMap(
          "0" -> "f4be",
          "1" -> "u4be",
          "_" -> "strz",
        )) should be (DynamicSized)

        // Dynamic + Fixed
        switchOn(SortedMap(
          "0" -> "strz",
          "1" -> "u4be",
          "_" -> "f4be",
        )) should be (DynamicSized)

        // Dynamic + Dynamic
        switchOn(SortedMap(
          "1" -> "strz",
          "_" -> "strz",
        )) should be (DynamicSized)
      }
    }

    describe("repeat") {
      it("no repeat does not change calculated size") {
        repeat(FixedSized(42), NoRepeat) should be (FixedSized(42))
        repeat(DynamicSized,   NoRepeat) should be (DynamicSized)
      }

      it("`repeat-expr: <negative count>` produces compilation error") {
        intercept[CompilationProblemException] {
          repeat(FixedSized(0), RepeatExpr(Ast.expr.IntNum(-1)))
        }.getMessage() should be ("(main): /:\n\terror: negative count of repetitions: -1\n")

        intercept[CompilationProblemException] {
          repeat(FixedSized(42), RepeatExpr(Ast.expr.IntNum(-1)))
        }.getMessage() should be ("(main): /:\n\terror: negative count of repetitions: -1\n")

        intercept[CompilationProblemException] {
          repeat(DynamicSized, RepeatExpr(Ast.expr.IntNum(-1)))
        }.getMessage() should be ("(main): /:\n\terror: negative count of repetitions: -1\n")
      }

      it("`repeat-expr: <zero count>` produces FixedSized(0)") {
        repeat(FixedSized(42), RepeatExpr(Ast.expr.IntNum(0))) should be (FixedSized(0))
        repeat(DynamicSized,   RepeatExpr(Ast.expr.IntNum(0))) should be (FixedSized(0))
      }

      it("`repeat-expr: <positive count>` multiplies size") {
        repeat(FixedSized(42), RepeatExpr(Ast.expr.IntNum(1))) should be (FixedSized(42))
        repeat(DynamicSized,   RepeatExpr(Ast.expr.IntNum(1))) should be (DynamicSized)

        repeat(FixedSized(42), RepeatExpr(Ast.expr.IntNum(2))) should be (FixedSized(84))
        repeat(DynamicSized,   RepeatExpr(Ast.expr.IntNum(2))) should be (DynamicSized)
      }

      it("`repeat-until: <falsy expression>` produces compilation error") {
        intercept[CompilationProblemException] {
          repeat(FixedSized(0), RepeatUntil(Ast.expr.Bool(false)))
        }.getMessage() should be ("(main): /:\n\terror: infinity cycle: stop condition is always `false`\n")

        intercept[CompilationProblemException] {
          repeat(FixedSized(42), RepeatUntil(Ast.expr.Bool(false)))
        }.getMessage() should be ("(main): /:\n\terror: infinity cycle: stop condition is always `false`\n")

        intercept[CompilationProblemException] {
          repeat(DynamicSized, RepeatUntil(Ast.expr.Bool(false)))
        }.getMessage() should be ("(main): /:\n\terror: infinity cycle: stop condition is always `false`\n")
      }

      it("`repeat-until: <truly expression>` returns the same size") {
        repeat(FixedSized(42), RepeatUntil(Ast.expr.Bool(true))) should be (FixedSized(42))
        repeat(DynamicSized,   RepeatUntil(Ast.expr.Bool(true))) should be (DynamicSized)
      }

      it("`repeat-until: <any expression>` produces DynamicSized") {
        val x = Ast.expr.Name(Ast.identifier("x"))

        repeat(FixedSized(42), RepeatUntil(x)) should be (DynamicSized)
        repeat(DynamicSized,   RepeatUntil(x)) should be (DynamicSized)
      }

      it("FixedSize(0) always produce FixedSize(0)") {
        val x = Ast.expr.Name(Ast.identifier("x"))

        repeat(FixedSized(0), NoRepeat) should be (FixedSized(0))

        repeat(FixedSized(0), RepeatExpr(Ast.expr.IntNum(0))) should be (FixedSized(0))
        repeat(FixedSized(0), RepeatExpr(Ast.expr.IntNum(1))) should be (FixedSized(0))
        repeat(FixedSized(0), RepeatExpr(Ast.expr.IntNum(2))) should be (FixedSized(0))
        repeat(FixedSized(0), RepeatExpr(x)) should be (FixedSized(0))

        repeat(FixedSized(0), RepeatUntil(Ast.expr.Bool(true))) should be (FixedSized(0))
        repeat(FixedSized(0), RepeatUntil(x)) should be (FixedSized(0))

        repeat(FixedSized(0), RepeatEos) should be (FixedSized(0))
      }
    }
  }
}
