package io.kaitai.struct.datatype

import io.kaitai.struct.format.YAMLParseException

sealed trait Endianness
case object LittleEndian extends Endianness {
  override def toString = "le"
}
case object BigEndian extends Endianness {
  override def toString = "be"
}

object Endianness {
  def defaultFromString(s: Option[String], path: List[String]) = s match {
    case None => None
    case Some("be") => Some(BigEndian)
    case Some("le") => Some(LittleEndian)
    case unknown => throw new YAMLParseException(
      s"must be `be` or `le`, but `$unknown` found",
      path ++ List("endian")
    )
  }

  def fromString(s: Option[String], defaultEndian: Option[Endianness], dt: String, path: List[String]) = s match {
    case Some("le") => LittleEndian
    case Some("be") => BigEndian
    case None =>
      defaultEndian match {
        case Some(e) => e
        case None => throw new YAMLParseException(s"unable to use type '$dt' without default endianness", path ++ List("type"))
      }
  }
}
