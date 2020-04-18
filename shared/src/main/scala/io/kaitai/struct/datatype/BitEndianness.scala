package io.kaitai.struct.datatype

import io.kaitai.struct.datatype.DataType.SwitchType
import io.kaitai.struct.exprlang.{Ast, Expressions}
import io.kaitai.struct.format.{ParseUtils, YAMLParseException}

sealed abstract class BitEndianness {
  def toSuffix: String
}
case object LittleBitEndian extends BitEndianness {
  override def toSuffix = "le"
}
case object BigBitEndian extends BitEndianness {
  override def toSuffix = "be"
}

object BitEndianness {
  def fromYaml(src: Option[Any], path: List[String]): Option[BitEndianness] = {
    src match {
      case None => None
      case Some("le") => Some(LittleBitEndian)
      case Some("be") => Some(BigBitEndian)
      case Some(s) =>
        throw new YAMLParseException(
          s"unable to parse bit endianness: expected `le` or `be`, found $s",
          path ++ List("bit-endian")
        )
    }
  }

  def fromString(s: Option[String], defBitEndian: Option[BitEndianness], dt: String, path: List[String]): BitEndianness = s match {
    case Some("le") => LittleBitEndian
    case Some("be") => BigBitEndian
    case None =>
      defBitEndian match {
        case Some(e) => e
        case None =>
          BigBitEndian
          // TODO: take `endian` as `bit-endian` default instead of assuming `be`
      }
  }
}
