package io.kaitai.struct.format

import java.util.{List => JList, Map => JMap}

import collection.JavaConversions._
import com.fasterxml.jackson.annotation.JsonProperty

class ClassSpec(@JsonProperty("meta") _meta: JMap[String, String],
                @JsonProperty("seq") _seq: JList[AttrSpec],
                @JsonProperty("types") _types: JMap[String, ClassSpec],
                @JsonProperty("instances") _instances: JMap[String, InstanceSpec],
                @JsonProperty("maps") _maps: JMap[String, Object]) {
  val meta: Map[String, String] = if (_meta == null) {
    Map()
  } else {
    _meta.toMap
  }
  val seq: List[AttrSpec] = if (_seq == null) {
    List()
  } else {
    _seq.toList
  }
  val types: Option[Map[String, ClassSpec]] = if (_types == null) {
    None
  } else {
    Some(_types.toMap)
  }
  val instances: Option[Map[String, InstanceSpec]] = if (_instances == null) {
    None
  } else {
    Some(_instances.toMap)
  }
  val maps: Option[Map[String, Object]] = if (_maps == null) {
    None
  } else {
    Some(_maps.toMap)
  }
}
