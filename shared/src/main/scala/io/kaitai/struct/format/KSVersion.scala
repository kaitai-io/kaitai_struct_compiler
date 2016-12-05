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
}
