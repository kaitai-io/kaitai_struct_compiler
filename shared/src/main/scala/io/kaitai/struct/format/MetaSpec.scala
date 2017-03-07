package io.kaitai.struct.format

import io.kaitai.struct.datatype.Endianness

case class MetaSpec(
  isOpaque: Boolean,
  id: Option[String],
  endian: Option[Endianness],
  encoding: Option[String],
  forceDebug: Boolean,
  opaqueTypes: Option[Boolean],
  imports: List[String]
)

object MetaSpec {
  val OPAQUE = MetaSpec(
    isOpaque = true, None, None, None, forceDebug = false, None, List()
  )

  val LEGAL_KEYS = Set(
    "id",
    "imports",
    "endian",
    "encoding",
    "title",
    "ks-version",
    "ks-debug",
    "ks-opaque-types",
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
    id.foreach((idStr) =>
      Identifier.checkIdentifierSource(idStr, "meta", path ++ List("id"))
    )

    val endian: Option[Endianness] = Endianness.defaultFromString(
      ParseUtils.getOptValueStr(srcMap, "endian", path),
      path
    )
    val encoding = ParseUtils.getOptValueStr(srcMap, "encoding", path)

    val forceDebug = ParseUtils.getOptValueBool(srcMap, "ks-debug", path).getOrElse(false)
    val opaqueTypes = ParseUtils.getOptValueBool(srcMap, "ks-opaque-types", path)

    val imports = ParseUtils.getListStr(srcMap, "imports", path)

    MetaSpec(isOpaque = false, id, endian, encoding, forceDebug, opaqueTypes, imports)
  }
}
