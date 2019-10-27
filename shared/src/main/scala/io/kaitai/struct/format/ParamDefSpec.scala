package io.kaitai.struct.format

import io.kaitai.struct.datatype.DataType

case class ParamDefSpec(
  path: List[String],
  id: Identifier,
  dataType: DataType,
  doc: DocSpec = DocSpec.EMPTY
) extends MemberSpec {
  override def isNullable: Boolean = false
  override def isNullableSwitchRaw: Boolean = false
}

object ParamDefSpec {
  def fromYaml(src: Any, path: List[String], idx: Int): ParamDefSpec = {
    val srcMap = ParseUtils.asMapStr(src, path)
    val id = ParseUtils.getValueIdentifier(srcMap, idx, "parameter", path)
    fromYaml(srcMap, path, id)
  }

  val LEGAL_KEYS = Set(
    "id",
    "type",
    "enum",
    "doc",
    "doc-ref"
  )

  def fromYaml(srcMap: Map[String, Any], path: List[String], id: Identifier): ParamDefSpec = {
    val doc = DocSpec.fromYaml(srcMap, path)
    val typeStr = ParseUtils.getOptValueStr(srcMap, "type", path)
    val enumRef = ParseUtils.getOptValueStr(srcMap, "enum", path)

    val dataType = DataType.pureFromString(typeStr, enumRef, path)

    ParseUtils.ensureLegalKeys(srcMap, LEGAL_KEYS, path, Some("parameter definition"))

    ParamDefSpec(path, id, dataType, doc)
  }
}
