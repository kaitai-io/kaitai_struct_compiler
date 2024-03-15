package io.kaitai.struct.format

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.exprlang.{Ast, Expressions}
import io.kaitai.struct.precompile.TypeUndecidedError

sealed abstract class InstanceSpec(val doc: DocSpec) extends MemberSpec {
  def dataTypeComposite: DataType
  def isNullable: Boolean
}
case class ValueInstanceSpec(
  id: InstanceIdentifier,
  path: List[String],
  value: Ast.expr,
  ifExpr: Option[Ast.expr] = None,
  var dataTypeOpt: Option[DataType] = None,
  val _doc: DocSpec = DocSpec.EMPTY,
) extends InstanceSpec(_doc) {
  override def dataType: DataType = {
    dataTypeOpt match {
      case Some(t) => t
      case None => throw new TypeUndecidedError(id.name)
    }
  }
  override def dataTypeComposite = dataType
  override def isNullable: Boolean = ifExpr.isDefined
  override def isNullableSwitchRaw: Boolean = isNullable
}
case class ParseInstanceSpec(
  id: InstanceIdentifier,
  path: List[String],
  dataType: DataType,
  cond: ConditionalSpec = ConditionalSpec(None, NoRepeat),
  pos: Option[Ast.expr] = None,
  io: Option[Ast.expr] = None,
  valid: Option[ValidationSpec] = None,
  val _doc: DocSpec = DocSpec.EMPTY,
) extends InstanceSpec(_doc) with AttrLikeSpec {
  override def isLazy = true
}

object InstanceSpec {
  val LEGAL_KEYS_VALUE_INST = Set(
    "value",
    "doc",
    "doc-ref",
    "enum",
    "if"
  )

  def fromYaml(src: Any, path: List[String], metaDef: MetaSpec, id: InstanceIdentifier): InstanceSpec = {
    val srcMap = ParseUtils.asMapStr(src, path)

    ParseUtils.getOptValueExpression(srcMap, "value", path) match {
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

        val ifExpr = ParseUtils.getOptValueExpression(srcMap, "if", path)

        ValueInstanceSpec(
          id,
          path,
          value2,
          ifExpr,
          None,
          DocSpec.fromYaml(srcMap, path),
        )
      case None =>
        // normal parse instance
        // TODO: perform proper validation of parse instance keys
        // ParseUtils.ensureLegalKeys(srcMap, LEGAL_KEYS_PARSE_INST, path, Some("parse instance"))

        val pos = ParseUtils.getOptValueExpression(srcMap, "pos", path)
        val io = ParseUtils.getOptValueExpression(srcMap, "io", path)

        val fakeAttrMap = srcMap.view.filterKeys((key) => key != "pos" && key != "io").toMap
        val a = AttrSpec.fromYaml(fakeAttrMap, path, metaDef, id)
        val valid = srcMap.get("valid").map(ValidationSpec.fromYaml(_, path ++ List("valid")))

        ParseInstanceSpec(id, path, a.dataType, a.cond, pos, io, valid, a.doc)
    }
  }
}
