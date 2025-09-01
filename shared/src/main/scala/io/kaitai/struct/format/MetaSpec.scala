package io.kaitai.struct.format

import io.kaitai.struct.datatype.{BitEndianness, CalcEndian, Endianness, InheritedEndian}
import io.kaitai.struct.problems.KSYParseError

case class MetaSpec(
  path: List[String],
  isOpaque: Boolean,
  id: Option[String],
  endian: Option[Endianness],
  bitEndian: Option[BitEndianness],
  var encoding: Option[String],
  forceDebug: Boolean,
  opaqueTypes: Option[Boolean],
  zeroCopySubstream: Option[Boolean],
  imports: List[String]
) extends YAMLPath {
  def fillInDefaults(defSpec: MetaSpec): MetaSpec = {
    fillInEncoding(defSpec.encoding)
      .fillInEndian(defSpec.endian)
      .fillInBitEndian(defSpec.bitEndian)
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

  def fillInBitEndian(defBitEndian: Option[BitEndianness]): MetaSpec = {
    (defBitEndian, bitEndian) match {
      case (None, _) => this
      case (_, Some(_)) => this
      case (Some(_), None) =>
        this.copy(bitEndian = defBitEndian)
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
    bitEndian = None,
    encoding = None,
    forceDebug = false,
    opaqueTypes = None,
    zeroCopySubstream = None,
    imports = List()
  )

  val LEGAL_KEYS = Set(
    "id",
    "imports",
    "endian",
    "bit-endian",
    "encoding",
    "title",
    "ks-version",
    "ks-debug",
    "ks-opaque-types",
    "ks-zero-copy-substream",
    "license",
    "file-extension",
    "xref",
    "tags",
    "application"
  )

  private final val ReKsVersion = """(?:0|[1-9][0-9]*)\.(?:0|[1-9][0-9]*)(?:\.(?:0|[1-9][0-9]*))?""".r
  private final val MIN_ALLOWED_KS_VERSION_0_6 = KSVersion.fromStr("0.6")

  def fromYaml(src: Any, path: List[String]): MetaSpec = {
    val srcMap = ParseUtils.asMapStr(src, path)

    ParseUtils.getOptValueStr(srcMap, "ks-version", path).foreach { (verStr) =>
      if (!ReKsVersion.matches(verStr)) {
        throw KSYParseError.withText(
          s"invalid compiler version '$verStr', expected 'X.Y' or 'X.Y.Z', " +
            "where X, Y, Z are non-negative integers without leading zeros",
          path :+ "ks-version"
        )
      }
      val ver = KSVersion.fromStr(verStr)
      if (ver < MIN_ALLOWED_KS_VERSION_0_6) {
        val extraHelp =
          if (ver.nums == List(0, 1)) {
            " (if you meant 0.10, use ks-version: '0.10' to prevent YAML from interpreting it as a float)"
          } else {
            ""
          }
        throw KSYParseError.withText(
          s"minimum allowed version is 0.6, but got $verStr$extraHelp",
          path :+ "ks-version"
        )
      }
      if (ver > KSVersion.current)
        throw KSYParseError.incompatibleVersion(ver, KSVersion.current, path :+ "ks-version")
    }

    val endian: Option[Endianness] = Endianness.fromYaml(srcMap.get("endian"), path)

    val bitEndian: Option[BitEndianness] = BitEndianness.fromYaml(srcMap.get("bit-endian"), path)

    ParseUtils.ensureLegalKeys(srcMap, LEGAL_KEYS, path)

    val id = ParseUtils.getOptValueStr(srcMap, "id", path)
    id.foreach((idStr) =>
      Identifier.checkIdentifierSource(idStr, "meta", path ++ List("id"))
    )

    val encoding = ParseUtils.getOptValueStr(srcMap, "encoding", path)

    val forceDebug = ParseUtils.getOptValueBool(srcMap, "ks-debug", path).getOrElse(false)
    val opaqueTypes = ParseUtils.getOptValueBool(srcMap, "ks-opaque-types", path)
    val zeroCopySubstream = ParseUtils.getOptValueBool(srcMap, "ks-zero-copy-substream", path)

    val imports = ParseUtils.getListStr(srcMap, "imports", path)

    MetaSpec(
      path,
      isOpaque = false,
      id,
      endian,
      bitEndian,
      encoding,
      forceDebug,
      opaqueTypes,
      zeroCopySubstream,
      imports
    )
  }
}
