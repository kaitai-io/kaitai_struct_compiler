package io.kaitai.struct.format

import java.util.{List => JList, Map => JMap}

import collection.JavaConversions._
import com.fasterxml.jackson.annotation.JsonProperty

class ClassSpec(@JsonProperty("meta") _meta: JMap[String, String],
                @JsonProperty("seq") _seq: JList[AttrSpec],
                @JsonProperty("types") _types: JMap[String, ClassSpec],
                @JsonProperty("instances") _instances: JMap[String, InstanceSpec],
                @JsonProperty("enums") _enums: JMap[String, JMap[String, String]]) {
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
  val enums: Map[String, Map[Long, String]] = if (_enums == null) {
    Map()
  } else {
    _enums.toMap.map { case(k, v) => (k, v.toMap.map { case (enumId, enumLabel) => (enumId.toLong, enumLabel) }) }
  }

  var _parentType: Option[(String, ClassSpec)] = None

  def parentTypeName: String = _parentType match {
    case Some((name: String, _)) => name
    case None => "kaitai_struct"
  }
  def parentType: ClassSpec = _parentType.get._2
}
