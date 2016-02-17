package io.kaitai.structures.format

import java.util.{List => JList, Map => JMap}

import collection.JavaConversions._
import com.fasterxml.jackson.annotation.JsonProperty

case class AttrSpec(@JsonProperty("id") id: String,
                    @JsonProperty("type") dataType: String,
                    @JsonProperty("process") process: String,
                    @JsonProperty("contents") contents: Object,
                    @JsonProperty("byte_size") byteSize: String,
                    @JsonProperty("size") size: String,
                    @JsonProperty("size_eos") sizeEos: Boolean,
                    @JsonProperty("if") ifExpr: String,
                    @JsonProperty("encoding") encoding: String,
                    @JsonProperty("repeat") repeat: String,
                    @JsonProperty("repeat-expr") repeatExpr: String,
                    @JsonProperty("terminator") terminator: String,
                    @JsonProperty("consume") consume: Boolean,
                    @JsonProperty("include") include: Boolean
                  ) {
}
