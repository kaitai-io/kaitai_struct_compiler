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

  /**
    * Dumps a version in Perl-style string, that is 1.2.3 becomes "1.002_003".
    * The limitation are the same as for `toInt`: 3 components max, each is
    * 0 to 999 inclusive.
    * @return version as a Perl-style string
    */
  def toPerlVersion: String = {
    if (nums.size > 3)
      throw new RuntimeException(s"C-style version int can have max 3 components, but $this is given")
    nums.foreach((comp) =>
      if (comp < 0 || comp > 999) {
        throw new RuntimeException(s"C-style version int only allows components [0..999], but $comp was used")
      }
    )

    val v1 :: v2 :: v3 :: _ = nums ++ List.fill(3 - nums.size)(0)
    "%d.%03d_%03d".format(v1, v2, v3)
  }

  /**
    * Dumps a version as a tuple of ints in Python syntax,
    * that is 1.2.3 becomes "(1, 2, 3)".
    * There are no limitations on the number of version components or the range
    * of values for each component.
    * @return version as a tuple of ints in Python syntax
    */
  def toPythonTuple: String = nums.mkString("(", ", ", ")")
}

object KSVersion {
  /**
    * This is abomination, the sole purpose of which is to get away from atrocious
    * SBT "we can generate managed source files (=Version.scala) only in platform-
    * dependent projects". As "shared" is not a such project, we can't just directly
    * generate a file that will be used by both projects. Probably something can be
    * done about it, but I've already spent like 4-5 hours on it and I'd rather spend
    * more on something more productive.
    */
  private var _current: Option[KSVersion] = None

  def current_=(str: String): Unit = {
    _current = Some(KSVersion.fromStr(str))
  }

  def current: KSVersion = _current.get

  def fromStr(str: String): KSVersion =
    KSVersion(str.replaceAll("-SNAPSHOT.*$", "").split('.').map(_.toInt).toList)

  /**
    * Hardcoded minimal version of runtime API that this particular
    * version of compiler is compatible with. Anything less that this
    * is not compatible and will trigger an error (compile-time, if
    * language supports it) when trying to use generated file together
    * with this older runtime API.
    */
  val minimalRuntime: KSVersion = KSVersion.fromStr("0.11")
}
