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

    describe("do not report warnings for members with derived encoding") {
      it("in parameters") {
        val problems = CanonicalizeEncodingNames.canonicalizeMember(ParamDefSpec(
          path = List("param"),
          id = NumberedIdentifier(1),
          dataType = StrFromBytesType(CalcBytesType, "utf-8", false),
        )).toList
        problems should be(List(EncodingNameWarning(
          "UTF-8", "utf-8",
          ProblemCoords(None, Some(List("param", "encoding")), None, None)
        )))

        val noProblems = CanonicalizeEncodingNames.canonicalizeMember(ParamDefSpec(
          path = List("param"),
          id = NumberedIdentifier(1),
          dataType = StrFromBytesType(CalcBytesType, "utf-8", true),
        )).toList
        noProblems should be(List())
      }

      it("in parse instance") {
        val problems = CanonicalizeEncodingNames.canonicalizeMember(ParseInstanceSpec(
          id = InstanceIdentifier("parse_instance"),
          path = List("parse", "instance"),
          dataType = StrFromBytesType(CalcBytesType, "utf-8", false),
        )).toList
        problems should be(List(EncodingNameWarning(
          "UTF-8", "utf-8",
          ProblemCoords(None, Some(List("parse", "instance", "encoding")), None, None)
        )))

        val noProblems = CanonicalizeEncodingNames.canonicalizeMember(ParseInstanceSpec(
          id = InstanceIdentifier("parse_instance"),
          path = List("parse", "instance"),
          dataType = StrFromBytesType(CalcBytesType, "utf-8", true),
        )).toList
        noProblems should be(List())
      }

      it("in value instance") {
        val problems = CanonicalizeEncodingNames.canonicalizeMember(ValueInstanceSpec(
          id = InstanceIdentifier("value_instance"),
          path = List("value", "instance"),
          value = Ast.expr.Str("value"),
          dataTypeOpt = Some(StrFromBytesType(CalcBytesType, "utf-8", false)),
        )).toList
        problems should be(List(EncodingNameWarning(
          "UTF-8", "utf-8",
          ProblemCoords(None, Some(List("value", "instance", "encoding")), None, None)
        )))

        val noProblems = CanonicalizeEncodingNames.canonicalizeMember(ValueInstanceSpec(
          id = InstanceIdentifier("value_instance"),
          path = List("value", "instance"),
          value = Ast.expr.Str("value"),
          dataTypeOpt = Some(StrFromBytesType(CalcBytesType, "utf-8", true)),
        )).toList
        noProblems should be(List())
      }
    }
  }
}
