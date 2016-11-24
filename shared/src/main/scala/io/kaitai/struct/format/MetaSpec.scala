package io.kaitai.struct.format

import io.kaitai.struct.exprlang.DataType.{BigEndian, Endianness, LittleEndian}

case class MetaSpec(
  isOpaque: Boolean,
  id: Option[String],
  endian: Option[Endianness]
)

object MetaSpec {
  val LEGAL_KEYS = Set(
    "id",
    "endian",
    "file-extension",
    "application"
  )

  def fromYaml(src: Any, path: List[String]): MetaSpec = {
    val srcMap = ParseUtils.asMapStr(src, path)
    ParseUtils.ensureLegalKeys(srcMap, LEGAL_KEYS, path)

    val id = ParseUtils.getOptValueStr(srcMap, "id", path)
    val endian: Option[Endianness] = srcMap.get("endian") match {
      case None => None
      case Some("be") => Some(BigEndian)
      case Some("le") => Some(LittleEndian)
      case unknown => throw new YAMLParseException(
        s"must be `be` or `le`, but `$unknown` found",
        path ++ List("endian")
      )
    }

    MetaSpec(isOpaque = false, id, endian)
  }
}
