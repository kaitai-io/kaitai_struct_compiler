package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class TypeDetector$Test extends AnyFunSpec {
  describe("TypeDetector") {
    it("combines ints properly") {
      val ut1 = CalcUserType(List("foo"), None)
      val ut2 = CalcUserType(List("bar"), None)

      TypeDetector.combineTypes(ut1, ut2) should be(CalcKaitaiStructType(false))
    }
  }
}
