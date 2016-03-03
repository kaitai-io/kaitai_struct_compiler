package io.kaitai.struct.format

import java.util.{List => JList, Map => JMap}

import io.kaitai.struct.Utils
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.exprlang.Expressions

import collection.JavaConversions._
import com.fasterxml.jackson.annotation.JsonProperty

class AttrSpec(
  @JsonProperty("id") val id: String,
  @JsonProperty("type") val dataType: String,
  @JsonProperty("process") _process: String,
  @JsonProperty("contents") val contents: Object,
  @JsonProperty("size") _size: String,
  @JsonProperty("size-eos") val sizeEos: Boolean,
  @JsonProperty("if") _ifExpr: String,
  @JsonProperty("encoding") _encoding: String,
  @JsonProperty("repeat") _repeat: String,
  @JsonProperty("repeat-expr") _repeatExpr: String,
  @JsonProperty("terminator") _terminator: String,
  @JsonProperty("consume") _consume: String,
  @JsonProperty("include") _include: String,
  @JsonProperty("eos-error") _eosError: String
) {
  val size = Option(_size).map(Expressions.parse)
  val ifExpr = Option(_ifExpr).map(Expressions.parse)
  val encoding = Option(_encoding)
  val repeat = Option(_repeat)
  val repeatExpr = Option(_repeatExpr).map(Expressions.parse)
  val terminator = Utils.strToOptInt(_terminator).getOrElse(0)

  val consume = boolFromStr(_consume, true)
  val include = boolFromStr(_include, false)
  val eosError = boolFromStr(_eosError, true)

  def isArray: Boolean = repeat.isDefined

  val process = ProcessExpr.fromStr(_process)

  private def boolFromStr(s: String, byDef: Boolean): Boolean = {
    s match {
      case "true" | "yes" | "1" => true
      case "false" | "no" | "0" | "" => false
      case null => byDef
    }
  }

  def dataTypeAsBaseType: BaseType = AttrSpec.dataTypeToBaseType(dataType, isArray)
}

object AttrSpec {
  def create(
              id: String = null,
              dataType: String = null,
              process: String = null,
              contents: Object = null,
              size: String = null,
              sizeEos: Boolean = false,
              ifExpr: String = null,
              encoding: String = null,
              repeat: String = null,
              repeatExpr: String = null,
              terminator: String = null,
              consume: String = null,
              include: String = null,
              eosError: String = null
            ): AttrSpec = {
    new AttrSpec(
      id,
      dataType,
      process,
      contents,
      size,
      sizeEos,
      ifExpr,
      encoding,
      repeat,
      repeatExpr,
      terminator,
      consume,
      include,
      eosError
    )
  }

  def dataTypeToBaseType(dt: String, isArray: Boolean): BaseType = {
    val t = dt match {
      case "u1" | "s1" |
           "u2le" | "u2be" | "u4le" | "u4be" | "u8le" | "u8be" |
           "s2le" | "s2be" | "s4le" | "s4be" | "s8le" | "s8be" |
           "u2" | "u4" | "u8" | "s2" | "s4" | "s8" =>
        IntType
      case "str" | "strz" =>
        StrType
      case null =>
        BytesType
      case _ =>
        UserType(dt)
    }

    if (isArray) {
      ArrayType(t)
    } else {
      t
    }
  }
}
