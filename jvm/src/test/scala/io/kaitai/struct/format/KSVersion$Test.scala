package io.kaitai.struct.format

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class KSVersion$Test extends AnyFunSpec {
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

  describe("KSVersion.toInt") {
    it("properly dumps 1.2.3") {
      val v = KSVersion.fromStr("1.2.3")
      v.toInt should be(1002003)
    }

    it("properly dumps 1.2") {
      val v = KSVersion.fromStr("1.2")
      v.toInt should be(1002000)
    }

    it("properly dumps 1") {
      val v = KSVersion.fromStr("1")
      v.toInt should be(1000000)
    }

    it("properly dumps 0.6") {
      val v = KSVersion.fromStr("0.6")
      v.toInt should be(6000)
    }

    it("fails on too long version 1.2.3.4") {
      val v = KSVersion.fromStr("1.2.3.4")
      an [RuntimeException] should be thrownBy v.toInt
    }

    it("fails on version with bogus component 1.78373") {
      val v = KSVersion.fromStr("1.78373")
      an [RuntimeException] should be thrownBy v.toInt
    }
  }
}
