package io.kaitai.struct.format

import java.nio.charset.Charset
import java.util
import java.util.{List => JList, Map => JMap}
import collection.JavaConversions._

import com.fasterxml.jackson.annotation.JsonProperty
import io.kaitai.struct.Utils
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.exprlang.{DataType, Expressions}

class AttrSpec(
  @JsonProperty("id") val id: String,
  @JsonProperty("type") _dataType: String,
  @JsonProperty("process") _process: String,
  @JsonProperty("contents") _contents: Object,
  @JsonProperty("size") _size: String,
  @JsonProperty("size-eos") val sizeEos: Boolean,
  @JsonProperty("if") _ifExpr: String,
  @JsonProperty("encoding") _encoding: String,
  @JsonProperty("repeat") _repeat: String,
  @JsonProperty("repeat-expr") _repeatExpr: String,
  @JsonProperty("terminator") _terminator: String,
  @JsonProperty("consume") _consume: String,
  @JsonProperty("include") _include: String,
  @JsonProperty("eos-error") _eosError: String,
  @JsonProperty("enum") _enum: String
) {
  val ifExpr = Option(_ifExpr).map(Expressions.parse)
  val repeat = Option(_repeat)
  val repeatExpr = Option(_repeatExpr).map(Expressions.parse)

  private val contents = if (_contents != null) {
    Some(AttrSpec.parseContentSpec(_contents))
  } else {
    None
  }
  private val size = Option(_size).map(Expressions.parse)
  private val encoding = Option(_encoding)
  private val terminator = Utils.strToOptInt(_terminator).getOrElse(0)
  private val consume = boolFromStr(_consume, true)
  private val include = boolFromStr(_include, false)
  private val eosError = boolFromStr(_eosError, true)

  lazy val dataType = DataType.yamlToDataType(_dataType, "le", size, sizeEos, encoding, terminator, include, consume, eosError, contents, Option(_enum))

  def isArray: Boolean = repeat.isDefined

  val process = ProcessExpr.fromStr(_process)

  private def boolFromStr(s: String, byDef: Boolean): Boolean = {
    s match {
      case "true" | "yes" | "1" => true
      case "false" | "no" | "0" | "" => false
      case null => byDef
    }
  }

  def dataTypeComposite: BaseType = {
    if (isArray) {
      ArrayType(dataType)
    } else {
      dataType
    }
  }
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
              eosError: String = null,
              _enum: String = null
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
      eosError,
      _enum
    )
  }

  def parseContentSpec(c: Object): Array[Byte] = {
    if (c.isInstanceOf[String]) {
      c.asInstanceOf[String].getBytes(Charset.forName("UTF-8"))
    } else if (c.isInstanceOf[util.ArrayList[Object]]) {
      val arr = c.asInstanceOf[util.ArrayList[Object]].toList
      val bb = new scala.collection.mutable.ArrayBuffer[Byte]
      arr.foreach((el) =>
        if (el.isInstanceOf[String]) {
          val strBytes = el.asInstanceOf[String].getBytes(Charset.forName("UTF-8"))
          bb.appendAll(strBytes)
        } else if (el.isInstanceOf[Integer]) {
          bb.append(el.asInstanceOf[Integer].toByte)
        } else {
          throw new RuntimeException(s"Unable to parse fixed content in array: ${el}")
        }
      )
      bb.toArray
    } else {
      throw new RuntimeException(s"Unable to parse fixed content: ${c.getClass}")
    }
  }
}
