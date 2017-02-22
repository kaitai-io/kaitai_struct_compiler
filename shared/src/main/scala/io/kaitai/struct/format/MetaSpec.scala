package io.kaitai.struct.format

import io.kaitai.struct.datatype.Endianness

case class MetaSpec(
  isOpaque: Boolean,
  id: Option[String],
  endian: Option[Endianness],
  encoding: Option[String],
  forceDebug: Boolean
)

object MetaSpec {
  val OPAQUE = MetaSpec(
    isOpaque = true, None, None, None, forceDebug = false)

  val LEGAL_KEYS = Set(
    "id",
    "endian",
    "encoding",
    "title",
    "ks-version",
    "ks-debug",
    "license",
    "file-extension",
    "application"
  )

  def fromYaml(src: Any, path: List[String]): MetaSpec = {
    val srcMap = ParseUtils.asMapStr(src, path)

    ParseUtils.getOptValueStr(srcMap, "ks-version", path).foreach { (verStr) =>
      val ver = KSVersion.fromStr(verStr)
      if (ver > KSVersion.current)
        throw YAMLParseException.incompatibleVersion(ver, KSVersion.current, path)
    }

    ParseUtils.ensureLegalKeys(srcMap, LEGAL_KEYS, path)

    val id = ParseUtils.getOptValueStr(srcMap, "id", path)
    val endian: Option[Endianness] = Endianness.defaultFromString(
      ParseUtils.getOptValueStr(srcMap, "endian", path),
      path
    )
    val encoding = ParseUtils.getOptValueStr(srcMap, "encoding", path)

    val forceDebug = ParseUtils.getOptValueBool(srcMap, "ks-debug", path).getOrElse(false)

    MetaSpec(isOpaque = false, id, endian, encoding, forceDebug)
  }
}
