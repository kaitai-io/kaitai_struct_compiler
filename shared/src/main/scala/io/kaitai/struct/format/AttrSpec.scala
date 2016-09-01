package io.kaitai.struct.format

import java.nio.charset.Charset
import java.util
import java.util.{List => JList, Map => JMap}

import com.fasterxml.jackson.annotation.{JsonCreator, JsonProperty}
import io.kaitai.struct.Utils
import io.kaitai.struct.exprlang.{Ast, DataType, Expressions}
import io.kaitai.struct.exprlang.DataType._

import scala.collection.JavaConversions._

sealed trait RepeatSpec
case class RepeatExpr(expr: Ast.expr) extends RepeatSpec
case class RepeatUntil(expr: Ast.expr) extends RepeatSpec
case object RepeatEos extends RepeatSpec
case object NoRepeat extends RepeatSpec

case class ConditionalSpec(ifExpr: Option[Ast.expr], repeat: RepeatSpec)

trait AttrLikeSpec {
  def dataType: BaseType
  def cond: ConditionalSpec

  def isArray: Boolean = cond.repeat != NoRepeat

  def dataTypeComposite: BaseType = {
    if (isArray) {
      ArrayType(dataType)
    } else {
      dataType
    }
  }
}

case class AttrSpec(
  id: Identifier,
  dataType: BaseType,
  cond: ConditionalSpec = ConditionalSpec(None, NoRepeat)
) extends AttrLikeSpec

object AttrSpec {
  def parseContentSpec(c: Object): Array[Byte] = {
    c match {
      case s: String =>
        s.getBytes(Charset.forName("UTF-8"))
      case objects: util.ArrayList[_] =>
        val bb = new scala.collection.mutable.ArrayBuffer[Byte]
        objects.foreach {
          case s: String =>
            bb.appendAll(Utils.strToBytes(s))
          case integer: Integer =>
            bb.append(Utils.clampIntToByte(integer))
          case el =>
            throw new RuntimeException(s"Unable to parse fixed content in array: $el")
        }
        bb.toArray
      case _ =>
        throw new RuntimeException(s"Unable to parse fixed content: ${c.getClass}")
    }
  }

  @JsonCreator
  def create(
              @JsonProperty("id") id: String,
              @JsonProperty("type") _dataType: Object,
              @JsonProperty("process") _process: String,
              @JsonProperty("contents") _contents: Object,
              @JsonProperty("size") _size: String,
              @JsonProperty("size-eos") sizeEos: Boolean,
              @JsonProperty("if") _ifExpr: String,
              @JsonProperty("encoding") _encoding: String,
              @JsonProperty("repeat") _repeat: String,
              @JsonProperty("repeat-expr") _repeatExpr: String,
              @JsonProperty("repeat-until") _repeatUntil: String,
              @JsonProperty("terminator") _terminator: String,
              @JsonProperty("consume") _consume: String,
              @JsonProperty("include") _include: String,
              @JsonProperty("eos-error") _eosError: String,
              @JsonProperty("enum") _enum: String
            ): AttrSpec = {
    if (id == null)
      throw new RuntimeException("id is mandatory for an attribute")

    val ifExpr = Option(_ifExpr).map(Expressions.parse)
    val repeat = Option(_repeat)
    val repeatExpr = Option(_repeatExpr).map(Expressions.parse)
    val repeatUntil = Option(_repeatUntil).map(Expressions.parse)

    val contents = if (_contents != null) {
      Some(AttrSpec.parseContentSpec(_contents))
    } else {
      None
    }
    val size = Option(_size).map(Expressions.parse)
    val encoding = Option(_encoding)
    val terminator = Utils.strToOptInt(_terminator).getOrElse(0)
    val consume = boolFromStr(_consume, true)
    val include = boolFromStr(_include, false)
    val eosError = boolFromStr(_eosError, true)
    val process = ProcessExpr.fromStr(_process)

    val dto = Option(_dataType)

    val dataType: BaseType = dto match {
      case None =>
        DataType.fromYaml(
          None, MetaSpec.globalMeta.get.endian,
          size, sizeEos,
          encoding, terminator, include, consume, eosError,
          contents, Option(_enum), process
        )
      case Some(simpleType: String) =>
        DataType.fromYaml(
          Some(simpleType), MetaSpec.globalMeta.get.endian,
          size, sizeEos,
          encoding, terminator, include, consume, eosError,
          contents, Option(_enum), process
        )
      case _ =>
        parseSwitch(_dataType,
          size, sizeEos,
          encoding, terminator, include, consume, eosError,
          contents, Option(_enum), process
        )
    }

    val repeatSpec = repeat match {
      case Some("until") => RepeatUntil(repeatUntil.get)
      case Some("expr") => RepeatExpr(repeatExpr.get)
      case Some("eos") => RepeatEos
      case None => NoRepeat
    }

    AttrSpec(NamedIdentifier(id), dataType, ConditionalSpec(ifExpr, repeatSpec))
  }

  private def parseSwitch(
    _dataType: Object,
    size: Option[Ast.expr],
    sizeEos: Boolean,
    encoding: Option[String],
    terminator: Int,
    include: Boolean,
    consume: Boolean,
    eosError: Boolean,
    contents: Option[Array[Byte]],
    enumRef: Option[String],
    process: Option[ProcessExpr]
  ): BaseType = {
    val switchSpec = _dataType.asInstanceOf[JMap[String, Object]].toMap
    val _on = switchSpec("switch-on").asInstanceOf[String]
    val _cases: Map[String, String] = switchSpec.get("cases") match {
      case None => Map()
      case Some(x) => x.asInstanceOf[JMap[String, String]].toMap
    }

    val on = Expressions.parse(_on)
    val cases = _cases.map { case (condition, typeName) =>
      Expressions.parse(condition) -> DataType.fromYaml(
        Some(typeName), MetaSpec.globalMeta.get.endian,
        size, sizeEos,
        encoding, terminator, include, consume, eosError,
        contents, enumRef, process
      )
    }

    SwitchType(on, cases)
  }

  private def boolFromStr(s: String, byDef: Boolean): Boolean = {
    s match {
      case "true" | "yes" | "1" => true
      case "false" | "no" | "0" | "" => false
      case null => byDef
    }
  }
}
