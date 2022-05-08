package io.kaitai.struct

import java.nio.charset.Charset

import scala.collection.mutable.ListBuffer

object Utils {
  /**
    * BigInt-typed max value of unsigned 32-bit integer.
    */
  final val MAX_UINT32 = BigInt("4294967295")

  /**
    * BigInt-typed max value of unsigned 64-bit integer.
    */
  final val MAX_UINT64 = BigInt("18446744073709551615")

  private val RDecimal = "^(-?[0-9]+)$".r
  private val RHex = "^0x([0-9a-fA-F]+)$".r

  def strToOptInt(s: String): Option[Int] = {
    s match {
      case null => None
      case RDecimal(_) => Some(s.toInt)
      case RHex(hex) => Some(Integer.parseInt(hex, 16))
    }
  }

  def strToLong(s: String): Long = {
    s match {
      case RDecimal(_) => s.toLong
      case RHex(hex) => java.lang.Long.parseLong(hex, 16)
    }
  }

  def strToBytes(s: String): Array[Byte] = {
    s match {
      case RDecimal(_) => Array(clampIntToByte(s.toInt))
      case RHex(hex) => Array(clampIntToByte(java.lang.Integer.parseInt(hex, 16)))
      case _ => s.getBytes(Charset.forName("UTF-8"))
    }
  }

  def clampIntToByte(i: Int): Byte = {
    if (i >= -128 && i < 256) {
      i.toByte
    } else {
      throw new RuntimeException(s"value $i outside of byte range")
    }
  }

  /**
    * Converts string to `UPPER_UNDER_SCORE` case.
    * @param s original string in `lower_under_score` case.
    * @return same string in `UPPER_UNDER_SCORE` case.
    */
  def upperUnderscoreCase(s: String): String =
    Platform.toUpperLocaleInsensitive(s)

  /**
    * Converts string to `lower_under_score` case. Given that currently
    * original string is using the same case, it is essentially a no-op.
    * @param s original string in `lower_under_score` case.
    * @return same string in `lower_under_score` case.
    */
  def lowerUnderscoreCase(s: String): String = s

  /**
    * Converts string to `UpperCamelCase`.
    * @param s original string in `lower_under_score` case.
    * @return same string in `UpperCamelCase`.
    */
  def upperCamelCase(s: String): String = {
    if (s.startsWith("_")) {
      "_" + upperCamelCase(s.substring(1))
    } else {
      s.split("_").map(capitalize).mkString
    }
  }

  /**
    * Converts string to `lowerCamelCase`.
    * @param s original string in `lower_under_score` case.
    * @return same string in `lowerCamelCase`.
    */
  def lowerCamelCase(s: String): String = {
    if (s.startsWith("_")) {
      "_" + lowerCamelCase(s.substring(1))
    } else {
      val firstWord :: restWords = s.split("_").toList
      (firstWord :: restWords.map(capitalize)).mkString
    }
  }

  def capitalize(s: String): String = {
    if (s.isEmpty) {
      s
    } else {
      s.charAt(0).toUpper + s.substring(1)
    }
  }

  /**
    * Joins collection together to make a single string. Makes extra exception for empty
    * collections (not like [[TraversableOnce]] `mkString`).
    * @param start the starting string.
    * @param sep   the separator string.
    * @param end   the ending string.
    * @return If the collection is empty, returns empty string, otherwise returns `start`,
    *         then elements of the collection, every pair separated with `sep`, then `end`.
    */
  def join[T](coll: TraversableOnce[T], start: String, sep: String, end: String): String =
    if (coll.isEmpty) "" else coll.mkString(start, sep, end)

  /**
    * Converts byte array (Seq[Byte]) into hex-escaped C-style literal characters
    * (i.e. like \xFF).
    * @param arr byte array to escape
    * @return array contents hex-escaped as string
    */
  def hexEscapeByteArray(arr: Seq[Byte]): String = {
    arr.map((x) =>
      // Note that we'll have to use "x & 0xff" trick to get byte as unsigned integer.
      // This code works differently in Scala JVM and JS, and this is by design.
      // For the details, see https://github.com/scala-js/scala-js/issues/2206
      "\\x%02X".format(x & 0xff)
    ).mkString
  }

  def addUniqueAttr[T](list: ListBuffer[T], element: T): Unit = {
    if (!list.contains(element))
      list += element
  }

  /**
    * Derives relative name to fullPath, given that we're currently located curPath.
    * @param fullPath
    * @param curPath
    * @return
    */
  def relClass(fullPath: List[String], curPath: List[String]): List[String] =
    if (fullPath.startsWith(curPath)) {
      fullPath.slice(curPath.length, fullPath.length)
    } else {
      fullPath
    }

  /**
    * Performs safe lookup for up to `len` character in a given
    * string `src`, starting at `from`.
    * @param src string to work on
    * @param from starting character index
    * @param len max length of substring
    * @return substring of `src`, starting at `from`, up to `len` chars max
    */
  def safeLookup(src: String, from: Int, len: Int): String = {
    val maxLen = src.length
    if (from >= maxLen) {
      ""
    } else {
      val to = from + len
      val safeTo = if (to > maxLen) maxLen else to
      src.substring(from, safeTo)
    }
  }
}
