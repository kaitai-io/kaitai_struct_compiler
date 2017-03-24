package io.kaitai.struct.format

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.exprlang.{Ast, Expressions}

sealed abstract class InstanceSpec(val doc: DocSpec) {
  def dataTypeComposite: DataType
}
case class ValueInstanceSpec(
  path: List[String],
  private val _doc: DocSpec,
  value: Ast.expr,
  ifExpr: Option[Ast.expr],
  var dataType: Option[DataType]
) extends InstanceSpec(_doc) with YAMLPath {
  override def dataTypeComposite = dataType.get
}
case class ParseInstanceSpec(
  path: List[String],
  private val _doc: DocSpec,
  dataType: DataType,
  cond: ConditionalSpec,
  pos: Option[Ast.expr],
  io: Option[Ast.expr]
) extends InstanceSpec(_doc) with AttrLikeSpec with YAMLPath

object InstanceSpec {
  val LEGAL_KEYS_VALUE_INST = Set(
    "value",
    "doc",
    "doc-ref",
    "enum",
    "if"
  )

  def fromYaml(src: Any, path: List[String], metaDef: MetaDefaults, id: InstanceIdentifier): InstanceSpec = {
    val srcMap = ParseUtils.asMapStr(src, path)

    ParseUtils.getOptValueStr(srcMap, "value", path).map(Expressions.parse) match {
      case Some(value) =>
        // value instance
        ParseUtils.ensureLegalKeys(srcMap, LEGAL_KEYS_VALUE_INST, path, Some("value instance"))

        // Wrap everything in EnumById if "enum" is used
        val value2 = ParseUtils.getOptValueStr(srcMap, "enum", path) match {
          case None =>
            value
          case Some(enumName) =>
            Ast.expr.EnumById(Ast.identifier(enumName), value)
        }

        val ifExpr = ParseUtils.getOptValueStr(srcMap, "if", path).map(Expressions.parse)

        ValueInstanceSpec(
          path,
          DocSpec.fromYaml(srcMap, path),
          value2,
          ifExpr,
          None
        )
      case None =>
        // normal positional instance
        val pos = ParseUtils.getOptValueStr(srcMap, "pos", path).map(Expressions.parse)
        val io = ParseUtils.getOptValueStr(srcMap, "io", path).map(Expressions.parse)

        val fakeAttrMap = srcMap.filterKeys((key) => key != "pos" && key != "io")
        val a = AttrSpec.fromYaml(fakeAttrMap, path, metaDef, id)
        ParseInstanceSpec(path, a.doc, a.dataType, a.cond, pos, io)
    }
  }
}
