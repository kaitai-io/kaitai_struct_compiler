package io.kaitai.struct

import java.nio.charset.Charset

import scala.collection.mutable.ListBuffer

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

  def upperCamelCase(s: String): String = {
    if (s.startsWith("_")) {
      "_" + upperCamelCase(s.substring(1))
    } else {
      s.split("_").map(capitalize).mkString
    }
  }

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
    * Converts byte array (Seq[Byte]) into hex-escaped C-style literal characters
    * (i.e. like \xFF).
    * @param arr byte array to escape
    * @return array contents hex-escaped as string
    */
  def hexEscapeByteArray(arr: Seq[Byte]): String = {
    arr.map((x) => "\\x%02X".format(x)).mkString
  }

  def addUniqueAttr[T](list: ListBuffer[T], element: T): Unit = {
    if (!list.contains(element))
      list += element
  }
}
