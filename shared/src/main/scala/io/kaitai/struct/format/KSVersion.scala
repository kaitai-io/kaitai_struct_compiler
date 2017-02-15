package io.kaitai.struct.format

case class KSVersion(nums: List[Int]) extends Ordered[KSVersion] {
  override def compare(that: KSVersion): Int = {
    nums.zip(that.nums).foreach { case (thisNum, otherNum) =>
      if (thisNum < otherNum) {
        return -1
      } else if (thisNum > otherNum) {
        return 1
      }
    }

    nums.size.compareTo(that.nums.size)
  }

  override def toString: String = nums.mkString(".")

  /**
    * Dumps a version in C-style integer, that is 1.2.3 becomes 001_002_003 = 1002003.
    * These versions can be compared to each other like regular numbers. Values for
    * each individual version component span from 0 to 999 inclusive, thus maximal ever
    * possible value is 999_999_999, which fits into 32-bit signed integer.
    * @return version as an integer, allowing easy numeric comparison
    */
  def toInt: Int = {
    if (nums.size > 3)
      throw new RuntimeException(s"C-style version int can have max 3 components, but $this is given")
    val nums2 = nums ++ List.fill(3 - nums.size)(0)
    nums2.foldLeft(0) { (sum, comp) =>
      if (comp < 0 || comp > 999) {
        throw new RuntimeException(s"C-style version int only allows components [0..999], but $comp was used")
      } else {
        sum * 1000 + comp
      }
    }
  }
}

object KSVersion {
  private var _current: Option[KSVersion] = None

  def current_=(str: String) {
    _current = Some(KSVersion.fromStr(str))
  }

  def current: KSVersion = _current.get

  def fromStr(str: String): KSVersion =
    KSVersion(str.replace("-SNAPSHOT", "").split('.').map(_.toInt).toList)

  /**
    * Hardcoded minimal version of runtime API that this particular
    * version of compiler is compatible with. Anything less that this
    * is not compatible and will trigger an error (compile-time, if
    * language supports it) when trying to use generated file together
    * with this older runtime API.
    */
  val minimalRuntime: KSVersion = KSVersion.fromStr("0.7")
}
