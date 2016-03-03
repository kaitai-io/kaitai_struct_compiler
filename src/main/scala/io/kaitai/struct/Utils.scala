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

  def upperCamelCase(s: String) = s.split("_").map(x => x.charAt(0).toUpper + x.substring(1)).mkString

  def lowerCamelCase(s: String): String = {
    val firstWord :: restWords = s.split("_").toList
    (firstWord :: restWords.map(capitalize)).mkString
  }

  def capitalize(s: String): String = {
    s.charAt(0).toUpper + s.substring(1)
  }
}
