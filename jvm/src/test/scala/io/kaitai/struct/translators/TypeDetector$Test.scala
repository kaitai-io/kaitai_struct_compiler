package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType._
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class TypeDetector$Test extends FunSpec {
  describe("TypeDetector") {
    it("combines ints properly") {
      val ut1 = CalcUserType(List("foo"), None)
      val ut2 = CalcUserType(List("bar"), None)

      TypeDetector.combineTypes(ut1, ut2) should be(CalcKaitaiStructType)
    }
  }
}
