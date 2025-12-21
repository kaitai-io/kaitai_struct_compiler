package io.kaitai.struct.precompile

import io.kaitai.struct.datatype.DataType.{CalcBytesType, StrFromBytesType}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.problems._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import java.util.Locale

class CanonicalizeEncodingNames$Test extends AnyFunSpec {
  describe("CanonicalizeEncodingNames.") {
    it("validates correct encoding name 'ASCII'") {
      val (newEncoding, problem) = CanonicalizeEncodingNames.canonicalizeName("ASCII")
      newEncoding should be("ASCII")
      problem should be(None)
    }

    it("reports error on encoding 'foo'") {
      val (newEncoding, problem) = CanonicalizeEncodingNames.canonicalizeName("foo")
      newEncoding should be("foo")
      problem should be(Some(UnrecognizedEncodingError("foo")))
    }

    it("reports warning and translates alias for 'iSo8859-1'") {
      val (newEncoding, problem) = CanonicalizeEncodingNames.canonicalizeName("iSo8859-1")
      newEncoding should be("ISO-8859-1")
      problem should be(Some(EncodingNameWarning("ISO-8859-1", "iSo8859-1")))
    }

    it("reports warning and fixes bad capitalization for 'iSo-8859-1'") {
      val (newEncoding, problem) = CanonicalizeEncodingNames.canonicalizeName("iSo-8859-1")
      newEncoding should be("ISO-8859-1")
      problem should be(Some(EncodingNameWarning("ISO-8859-1", "iSo-8859-1")))
    }

    it("reports warning and fixes bad capitalization for 'iSo-8859-1' even in Turkish locale") {
      // This test only covers the case conversion in the `canonicalizeName` implementation,
      // not the case conversions used when initializing the `aliasToCanonical` map.
      val oldLocale = Locale.getDefault
      Locale.setDefault(new Locale("tr"))
      try {
        val (newEncoding, problem) = CanonicalizeEncodingNames.canonicalizeName("iSo-8859-1")
        newEncoding should be("ISO-8859-1")
        problem should be(Some(EncodingNameWarning("ISO-8859-1", "iSo-8859-1")))
      } finally {
        Locale.setDefault(oldLocale)
      }
    }
  }
}
