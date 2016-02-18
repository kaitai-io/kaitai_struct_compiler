package io.kaitai.structures.format

import java.util.{List => JList, Map => JMap}

import collection.JavaConversions._
import com.fasterxml.jackson.annotation.JsonProperty

case class AttrSpec(@JsonProperty("id") id: String,
                    @JsonProperty("type") dataType: String,
                    @JsonProperty("process") process: String,
                    @JsonProperty("contents") contents: Object,
                    @JsonProperty("byte_size") byteSize: String,
                    @JsonProperty("size") _size: String,
                    @JsonProperty("size_eos") sizeEos: Boolean,
                    @JsonProperty("if") _ifExpr: String,
                    @JsonProperty("encoding") _encoding: String,
                    @JsonProperty("repeat") _repeat: String,
                    @JsonProperty("repeat-expr") _repeatExpr: String,
                    @JsonProperty("terminator") terminator: String,
                    @JsonProperty("consume") consume: Boolean,
                    @JsonProperty("include") include: Boolean
                  ) {
  val size = Option(_size)
  val ifExpr = Option(_ifExpr)
  val encoding = Option(_encoding)
  val repeat = Option(_repeat)
  val repeatExpr = Option(_repeatExpr)

  def isArray: Boolean = repeat.isDefined
}
