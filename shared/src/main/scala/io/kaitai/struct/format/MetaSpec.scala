package io.kaitai.struct.format

import com.fasterxml.jackson.annotation.{JsonIgnore, JsonProperty, JsonCreator}
import io.kaitai.struct.exprlang.DataType.{LittleEndian, BigEndian, Endianness}

case class MetaSpec(id: String, endian: Option[Endianness])

object MetaSpec {
  @JsonCreator
  def create(
              @JsonProperty("id") _id: String,
              @JsonProperty("endian") _endian: String,
              @JsonProperty("file-extension") fileExtension: String,
              @JsonProperty("application") application: String
            ): MetaSpec = {
    if (_id == null) {
      throw new RuntimeException("meta: id is required, but not found");
    }

    val endian = _endian match {
      case null => None
      case "be" => Some(BigEndian)
      case "le" => Some(LittleEndian)
      case _ => throw new RuntimeException(s"meta: endian - must be 'be' or 'le', but '${_endian}' found")
    }

    val m = MetaSpec(_id, endian)
    globalMeta = Some(m)
    m
  }

  var globalMeta: Option[MetaSpec] = None
}
