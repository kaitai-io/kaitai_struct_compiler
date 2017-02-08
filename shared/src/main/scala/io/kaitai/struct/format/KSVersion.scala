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
  val minimalRuntime: KSVersion = KSVersion.fromStr("0.6")
}
