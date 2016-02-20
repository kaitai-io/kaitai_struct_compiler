package io.kaitai.struct

object Utils {
  private val RDecimal = "^(-?[0-9]+)$".r
  private val RHex = "^0x([0-9a-fA-F]+)$".r

  def strToOptInt(s: String): Option[Int] = {
    s match {
      case null => None
      case RDecimal(_) => Some(s.toInt)
      case RHex(hex) => Some(Integer.parseInt(hex, 16))
    }
  }
}
