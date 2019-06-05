package io.kaitai.struct.format

import java.nio.charset.Charset

import io.kaitai.struct.Utils
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.{Ast, Expressions}

import scala.collection.JavaConversions._

case class ConditionalSpec(ifExpr: Option[Ast.expr], repeat: RepeatSpec)

trait AttrLikeSpec extends MemberSpec {
  def dataType: DataType
  def cond: ConditionalSpec
  def doc: DocSpec

  def isArray: Boolean = cond.repeat != NoRepeat

  override def dataTypeComposite: DataType = {
    if (isArray) {
      ArrayType(dataType)
    } else {
      dataType
    }
  }

  override def isNullable: Boolean = {
    if (cond.ifExpr.isDefined) {
      true
    } else {
      dataType match {
        case st: SwitchType =>
          st.isNullable
        case _ =>
          false
      }
    }
  }

  def isNullableSwitchRaw: Boolean = {
    if (cond.ifExpr.isDefined) {
      true
    } else {
      dataType match {
        case st: SwitchType =>
          st.isNullableSwitchRaw
        case _ =>
          false
      }
    }
  }

  /**
    * Determines if this attribute is to be parsed lazily (i.e. on first use),
    * or eagerly (during object construction, usually in a `_read` method)
    * @return True if this attribute is lazy, false if it's eager
    */
  def isLazy: Boolean
}

case class AttrSpec(
  path: List[String],
  id: Identifier,
  dataType: DataType,
  cond: ConditionalSpec = ConditionalSpec(None, NoRepeat),
  doc: DocSpec = DocSpec.EMPTY
) extends AttrLikeSpec with MemberSpec {
  override def isLazy = false
}

case class YamlAttrArgs(
  size: Option[Ast.expr],
  sizeEos: Boolean,
  encoding: Option[String],
  terminator: Option[Int],
  include: Boolean,
  consume: Boolean,
  eosError: Boolean,
  padRight: Option[Int],
  contents: Option[Array[Byte]],
  enumRef: Option[String],
  parent: Option[Ast.expr],
  process: Option[ProcessExpr],
  scanEnd: Option[ScanExpr]
) {
  def getByteArrayType(path: List[String]) = {
    (size, sizeEos) match {
      case (Some(bs: expr), false) =>
        BytesLimitType(bs, terminator, include, padRight, process)
      case (None, true) =>
        BytesEosType(terminator, include, padRight, process)
      case (None, false) =>
        terminator match {
          case Some(term) =>
            BytesTerminatedType(term, include, consume, eosError, process)
          case None =>
            BytesScanEndType(process, scanEnd)
            // throw new YAMLParseException("'size', 'size-eos' or 'terminator' must be specified", path)
        }
      case (Some(_), true) =>
        throw new YAMLParseException("only one of 'size' or 'size-eos' must be specified", path)
    }
  }
}

object AttrSpec {
  val LEGAL_KEYS = Set(
    "id",
    "doc",
    "doc-ref",
    "type",
    "if",
    "terminator",
    "consume",
    "include",
    "eos-error",
    "repeat",
    "scan-end"
  )

  val LEGAL_KEYS_BYTES = Set(
    "contents",
    "size",
    "size-eos",
    "pad-right",
    "parent",
    "process"
  )

  val LEGAL_KEYS_STR = Set(
    "size",
    "size-eos",
    "pad-right",
    "encoding"
  )

  val LEGAL_KEYS_ENUM = Set(
    "enum"
  )

  def fromYaml(src: Any, path: List[String], metaDef: MetaSpec, idx: Int): AttrSpec = {
    val srcMap = ParseUtils.asMapStr(src, path)
    val id = ParseUtils.getOptValueStr(srcMap, "id", path) match {
      case Some(idStr) =>
        try {
          NamedIdentifier(idStr)
        } catch {
          case _: InvalidIdentifier =>
            throw YAMLParseException.invalidId(idStr, "attribute", path ++ List("id"))
        }
      case None => NumberedIdentifier(idx)
    }
    fromYaml(srcMap, path, metaDef, id)
  }

  def fromYaml(srcMap: Map[String, Any], path: List[String], metaDef: MetaSpec, id: Identifier): AttrSpec = {
    try {
      fromYaml2(srcMap, path, metaDef, id)
    } catch {
      case (epe: Expressions.ParseException) =>
        throw YAMLParseException.expression(epe, path)
    }
  }

  def fromYaml2(srcMap: Map[String, Any], path: List[String], metaDef: MetaSpec, id: Identifier): AttrSpec = {
    val doc = DocSpec.fromYaml(srcMap, path)
    val process = ProcessExpr.fromStr(ParseUtils.getOptValueStr(srcMap, "process", path), path)
    val scanEnd = ScanExpr.fromStr(ParseUtils.getOptValueStr(srcMap, "scan-end", path), path)

    // TODO: add proper path propagation
    val contents = srcMap.get("contents").map(parseContentSpec(_, path ++ List("contents")))
    val size = ParseUtils.getOptValueExpression(srcMap, "size", path)
    val sizeEos = ParseUtils.getOptValueBool(srcMap, "size-eos", path).getOrElse(false)
    val ifExpr = ParseUtils.getOptValueExpression(srcMap, "if", path)
    val encoding = ParseUtils.getOptValueStr(srcMap, "encoding", path)
    val terminator = ParseUtils.getOptValueInt(srcMap, "terminator", path)
    val consume = ParseUtils.getOptValueBool(srcMap, "consume", path).getOrElse(true)
    val include = ParseUtils.getOptValueBool(srcMap, "include", path).getOrElse(false)
    val eosError = ParseUtils.getOptValueBool(srcMap, "eos-error", path).getOrElse(true)
    val padRight = ParseUtils.getOptValueInt(srcMap, "pad-right", path)
    val enum = ParseUtils.getOptValueStr(srcMap, "enum", path)
    val parent = ParseUtils.getOptValueExpression(srcMap, "parent", path)

    val typObj = srcMap.get("type")

    val yamlAttrArgs = YamlAttrArgs(
      size, sizeEos,
      encoding, terminator, include, consume, eosError, padRight,
      contents, enum, parent, process, scanEnd
    )

    // Unfortunately, this monstrous match can't rewritten in simpler way due to Java type erasure
    val dataType: DataType = typObj match {
      case None =>
        DataType.fromYaml(
          None, path, metaDef, yamlAttrArgs
        )
      case Some(x) =>
        x match {
          case simpleType: String =>
            DataType.fromYaml(
              Some(simpleType), path, metaDef, yamlAttrArgs
            )
          case switchMap: Map[Any, Any] =>
            val switchMapStr = ParseUtils.anyMapToStrMap(switchMap, path)
            parseSwitch(switchMapStr, path, metaDef, yamlAttrArgs)
          case unknown =>
            throw new YAMLParseException(s"expected map or string, found $unknown", path ++ List("type"))
        }
    }

    val (repeatSpec, legalRepeatKeys) = RepeatSpec.fromYaml(srcMap, path)

    val legalKeys = LEGAL_KEYS ++ legalRepeatKeys ++ (dataType match {
      case _: BytesType => LEGAL_KEYS_BYTES
      case _: StrFromBytesType => LEGAL_KEYS_STR
      case _: UserType => LEGAL_KEYS_BYTES
      case EnumType(_, _) => LEGAL_KEYS_ENUM
      case _: SwitchType => LEGAL_KEYS_BYTES
      case _ => Set()
    })

    ParseUtils.ensureLegalKeys(srcMap, legalKeys, path)

    AttrSpec(path, id, dataType, ConditionalSpec(ifExpr, repeatSpec), doc)
  }

  def parseContentSpec(c: Any, path: List[String]): Array[Byte] = {
    c match {
      case s: String =>
        s.getBytes(Charset.forName("UTF-8"))
      case objects: List[_] =>
        val bb = new scala.collection.mutable.ArrayBuffer[Byte]
        objects.zipWithIndex.foreach { case (value, idx) =>
          value match {
            case s: String =>
              bb.appendAll(Utils.strToBytes(s))
            case integer: Integer =>
              bb.append(Utils.clampIntToByte(integer))
            case el =>
              throw new YAMLParseException(s"unable to parse fixed content in array: $el", path ++ List(idx.toString))
          }
        }
        bb.toArray
      case _ =>
        throw new YAMLParseException(s"unable to parse fixed content: $c", path)
    }
  }

  val LEGAL_KEYS_SWITCH = Set(
    "switch-on",
    "cases"
  )

  private def parseSwitch(
    switchSpec: Map[String, Any],
    path: List[String],
    metaDef: MetaSpec,
    arg: YamlAttrArgs
  ): DataType = {
    val on = ParseUtils.getValueExpression(switchSpec, "switch-on", path)
    val _cases = ParseUtils.getValueMapStrStr(switchSpec, "cases", path)

    ParseUtils.ensureLegalKeys(switchSpec, LEGAL_KEYS_SWITCH, path)

    val cases = _cases.map { case (condition, typeName) =>
      val casePath = path ++ List("cases", condition)
      val condType = DataType.fromYaml(
        Some(typeName), casePath, metaDef,
        arg
      )
      try {
        Expressions.parse(condition) -> condType
      } catch {
        case epe: Expressions.ParseException =>
          throw YAMLParseException.expression(epe, casePath)
      }
    }

    // If we have size defined, and we don't have any "else" case already, add
    // an implicit "else" case that will at least catch everything else as
    // "untyped" byte array of given size
    val addCases: Map[Ast.expr, DataType] = if (cases.containsKey(SwitchType.ELSE_CONST)) {
      Map()
    } else {
      (arg.size, arg.sizeEos) match {
        case (Some(sizeValue), false) =>
          Map(SwitchType.ELSE_CONST -> BytesLimitType(sizeValue, None, false, None, arg.process))
        case (None, true) =>
          Map(SwitchType.ELSE_CONST -> BytesEosType(None, false, None, arg.process))
        case (None, false) =>
          Map()
        case (Some(_), true) =>
          throw new YAMLParseException("can't have both `size` and `size-eos` defined", path)
      }
    }

    SwitchType(on, cases ++ addCases)
  }
}
