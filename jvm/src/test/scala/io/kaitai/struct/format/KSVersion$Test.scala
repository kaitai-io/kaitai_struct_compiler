package io.kaitai.struct.format

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class KSVersion$Test extends FunSpec {
  describe("KSVersion.compareTo") {
    it("returns 0 for equal versions") {
      val v1 = KSVersion.fromStr("1.2.3")
      v1.compareTo(v1) should be(0)
    }

    it("properly compares 1.2.3 and 1.2.4") {
      val v1 = KSVersion.fromStr("1.2.3")
      val v2 = KSVersion.fromStr("1.2.4")
      v1.compareTo(v2) should be(-1)
      v2.compareTo(v1) should be(1)

      (v1 < v2) should be(true)
      (v2 > v1) should be(true)
    }

    it("properly compares 0.1 and 0.1.2") {
      val v1 = KSVersion.fromStr("0.1")
      val v2 = KSVersion.fromStr("0.1.2")
      v1.compareTo(v2) should be(-1)
      v2.compareTo(v1) should be(1)

      (v1 < v2) should be(true)
      (v2 > v1) should be(true)
    }
  }
}
