package io.kaitai.struct.format

import io.kaitai.struct.datatype.{CalcEndian, Endianness, InheritedEndian}

case class MetaSpec(
  path: List[String],
  isOpaque: Boolean,
  id: Option[String],
  endian: Option[Endianness],
  encoding: Option[String],
  forceDebug: Boolean,
  opaqueTypes: Option[Boolean],
  imports: List[String]
) extends YAMLPath {
  def fillInDefaults(defSpec: MetaSpec): MetaSpec = {
    fillInEncoding(defSpec.encoding).
      fillInEndian(defSpec.endian)
  }

  private
  def fillInEncoding(defEncoding: Option[String]): MetaSpec = {
    (defEncoding, encoding) match {
      case (None, _) => this
      case (_, Some(_)) => this
      case (Some(_), None) =>
        this.copy(encoding = defEncoding)
    }
  }

  def fillInEndian(defEndian: Option[Endianness]): MetaSpec = {
    (defEndian, endian) match {
      case (None, _) => this
      case (_, Some(_)) => this
      case (Some(_: CalcEndian), None) =>
        this.copy(endian = Some(InheritedEndian))
      case (Some(_), None) =>
        this.copy(endian = defEndian)
    }
  }
}

object MetaSpec {
  def emptyWithPath(path: List[String]) = OPAQUE.copy(isOpaque = false, path = path)

  val OPAQUE = MetaSpec(
    path = List(),
    isOpaque = true,
    id = None,
    endian = None,
    encoding = None,
    forceDebug = false,
    opaqueTypes = None,
    imports = List()
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
    "xref",
    "tags",
    "application"
  )

  def fromYaml(src: Any, path: List[String]): MetaSpec = {
    val srcMap = ParseUtils.asMapStr(src, path)

    ParseUtils.getOptValueStr(srcMap, "ks-version", path).foreach { (verStr) =>
      val ver = KSVersion.fromStr(verStr)
      if (ver > KSVersion.current)
        throw YAMLParseException.incompatibleVersion(ver, KSVersion.current, path)
    }

    val endian: Option[Endianness] = Endianness.fromYaml(srcMap.get("endian"), path)

    ParseUtils.ensureLegalKeys(srcMap, LEGAL_KEYS, path)

    val id = ParseUtils.getOptValueStr(srcMap, "id", path)
    id.foreach((idStr) =>
      Identifier.checkIdentifierSource(idStr, "meta", path ++ List("id"))
    )

    val encoding = ParseUtils.getOptValueStr(srcMap, "encoding", path)

    val forceDebug = ParseUtils.getOptValueBool(srcMap, "ks-debug", path).getOrElse(false)
    val opaqueTypes = ParseUtils.getOptValueBool(srcMap, "ks-opaque-types", path)

    val imports = ParseUtils.getListStr(srcMap, "imports", path)

    MetaSpec(path, isOpaque = false, id, endian, encoding, forceDebug, opaqueTypes, imports)
  }
}
