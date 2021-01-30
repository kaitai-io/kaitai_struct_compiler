package io.kaitai.struct.format

import io.kaitai.struct.problems.KSYParseError

case class EnumValueSpec(name: String, doc: DocSpec)

object EnumValueSpec {
  def fromYaml(src: Any, path: List[String]): EnumValueSpec = {
    src match {
      case name: String =>
        fromSimpleName(name, path)
      case x: Boolean =>
        fromSimpleName(x.toString, path)
      case srcMap: Map[Any, Any] =>
        fromMap(ParseUtils.anyMapToStrMap(srcMap, path), path)
      case _ =>
        throw KSYParseError.badType("string or map", src, path)
    }
  }

  def fromSimpleName(name: String, path: List[String]): EnumValueSpec = {
    Identifier.checkIdentifierSource(name, "enum member", path)
    EnumValueSpec(name, DocSpec.EMPTY)
  }

  val LEGAL_KEYS = Set(
    "id",
    "doc",
    "doc-ref"
  )

  def fromMap(srcMap: Map[String, Any], path: List[String]): EnumValueSpec = {
    ParseUtils.ensureLegalKeys(srcMap, LEGAL_KEYS, path, Some("enum value spec"))

    val name = ParseUtils.getValueStr(srcMap, "id", path)
    Identifier.checkIdentifierSource(name, "enum value spec id", path)

    val doc = DocSpec.fromYaml(srcMap, path)

    EnumValueSpec(name, doc)
  }
}
