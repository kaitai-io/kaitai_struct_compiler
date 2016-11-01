package io.kaitai.struct.format

import io.kaitai.struct.exprlang.DataType.{BigEndian, Endianness, LittleEndian}

case class MetaSpec(id: String, endian: Option[Endianness])

object MetaSpec {
  def fromYaml(src: AnyRef, path: List[String]): MetaSpec = {
    src match {
      case srcMap: Map[String, AnyRef] =>
        var id: Option[String] = None
        var endian: Option[Endianness] = None

        srcMap.foreach { case (key, value) =>
          key match {
            case "id" =>
              id = Some(value.asInstanceOf[String])
            case "endian" =>
              endian = srcMap.get("endian") match {
                case None => None
                case Some("be") => Some(BigEndian)
                case Some("le") => Some(LittleEndian)
                case unknown => throw new YAMLParseException(
                  s"must be `be` or `le`, but `$unknown` found",
                  path ++ List("endian")
                )
              }
            case "file-extension" | "application" => // ignore
            case unknown =>
              throw new YAMLParseException(s"unknown key: `$unknown`", path)
          }
        }

        if (id.isEmpty)
          throw new YAMLParseException(s"`id` is required, but none found", path)

        val meta = MetaSpec(id.get, endian)
        // TODO: remove hack
        globalMeta = Some(meta)
        meta
      case unknown =>
        throw new YAMLParseException(s"expected map, found $unknown", path)
    }
  }

  var globalMeta: Option[MetaSpec] = None
}
