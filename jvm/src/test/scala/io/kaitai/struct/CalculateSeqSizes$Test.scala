package io.kaitai.struct

import io.kaitai.struct.datatype.{BigEndian, BigBitEndian}
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{DynamicSized, FixedSized, MetaSpec, Sized, YamlAttrArgs}
import io.kaitai.struct.precompile.CalculateSeqSizes

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
  }
}
