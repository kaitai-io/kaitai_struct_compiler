package io.kaitai.struct.format

import java.util.{List => JList, Map => JMap}

import io.kaitai.struct.exprlang.Expressions

import collection.JavaConversions._
import com.fasterxml.jackson.annotation.JsonProperty

class InstanceSpec(
  @JsonProperty("id") id: String,
  @JsonProperty("type") dataType: String,
  @JsonProperty("process") process: String,
  @JsonProperty("contents") contents: Object,
  @JsonProperty("byte_size") _byteSize: String,
  @JsonProperty("size") _size: String,
  @JsonProperty("size_eos") sizeEos: Boolean,
  @JsonProperty("if") _ifExpr: String,
  @JsonProperty("encoding") _encoding: String,
  @JsonProperty("repeat") _repeat: String,
  @JsonProperty("repeat-expr") _repeatExpr: String,
  @JsonProperty("terminator") _terminator: String,
  @JsonProperty("consume") _consume: String,
  @JsonProperty("include") _include: String,
  @JsonProperty("eos_error") _eosError: String,

  @JsonProperty("position_abs") _positionAbs: String,
  @JsonProperty("value") _value: String
) extends AttrSpec(id, dataType, process, contents, _byteSize, _size, sizeEos, _ifExpr, _encoding, _repeat, _repeatExpr, _terminator, _consume, _include, _eosError) {
  val positionAbs = Option(_positionAbs).map(Expressions.parse)
  val value = Option(_value).map(e =>
    if (dataType != null) {
      throw new RuntimeException(s"instance '${id}': can't specify both 'value' and 'type'")
    } else if (process != null) {
      throw new RuntimeException(s"instance '${id}': can't specify both 'value' and 'process'")
    } else if (contents != null) {
      throw new RuntimeException(s"instance '${id}': can't specify both 'value' and 'contents'")
    } else if (_size != null) {
      throw new RuntimeException(s"instance '${id}': can't specify both 'value' and 'size'")
    } else if (_repeat != null) {
      throw new RuntimeException(s"instance '${id}': can't specify both 'value' and 'repeat'")
    } else if (_repeatExpr != null) {
      throw new RuntimeException(s"instance '${id}': can't specify both 'value' and 'repeat-expr'")
    } else if (positionAbs.isDefined) {
      throw new RuntimeException(s"instance '${id}': can't specify both 'value' and 'position_abs'")
    } else {
      Expressions.parse(e)
    }
  )
}
