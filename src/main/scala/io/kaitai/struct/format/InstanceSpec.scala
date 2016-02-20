package io.kaitai.struct.format

import java.util.{List => JList, Map => JMap}

import collection.JavaConversions._
import com.fasterxml.jackson.annotation.JsonProperty

case class InstanceSpec(
  @JsonProperty("type") dataType: String,
  @JsonProperty("position_abs") positionAbs: String,
  @JsonProperty("size") size: String
) {
  // TODO: implement repeats for instances
  def isArray: Boolean = false
}
