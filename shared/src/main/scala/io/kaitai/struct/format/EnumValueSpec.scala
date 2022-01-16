package io.kaitai.struct.format

import io.kaitai.struct.problems.KSYParseError

case class EnumValueSpec(name: String, doc: DocSpec)

object EnumValueSpec {
  def fromYaml(src: yamlesque.Node, path: List[String]): EnumValueSpec = {
    src match {
      case yamlesque.Str(name) =>
        fromSimpleName(name, path)
      case yamlesque.Bool(x) =>
        fromSimpleName(x.toString, path)
      case obj: yamlesque.Obj =>
        fromMap(obj, path)
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

  def fromMap(srcMap: yamlesque.Obj, path: List[String]): EnumValueSpec = {
    ParseUtils.ensureLegalKeys(srcMap, LEGAL_KEYS, path, Some("enum value spec"))

    val name = ParseUtils.getValueStr(srcMap, "id", path)
    Identifier.checkIdentifierSource(name, "enum value spec id", path)

    val doc = DocSpec.fromYaml(srcMap, path)

    EnumValueSpec(name, doc)
  }
}
