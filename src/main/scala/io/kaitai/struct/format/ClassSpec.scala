package io.kaitai.struct.format

import java.util.{List => JList, Map => JMap}

import io.kaitai.struct.Utils

import collection.JavaConversions._
import com.fasterxml.jackson.annotation.{JsonCreator, JsonProperty}

case class ClassSpec(
                      meta: Option[MetaSpec],
                      seq: List[AttrSpec],
                      types: Map[String, ClassSpec],
                      instances: Map[String, InstanceSpec],
                      enums: Map[String, Map[Long, String]]
                    ) {
  var _parentType: Option[(String, ClassSpec)] = None

  def parentTypeName: String = _parentType match {
    case Some((name: String, _)) => name
    case None => "kaitai_struct"
  }
  def parentType: ClassSpec = _parentType.get._2
}

object ClassSpec {
  @JsonCreator
  def create(@JsonProperty("meta") _meta: MetaSpec,
             @JsonProperty("seq") _seq: JList[AttrSpec],
             @JsonProperty("types") _types: JMap[String, ClassSpec],
             @JsonProperty("instances") _instances: JMap[String, InstanceSpec],
             @JsonProperty("enums") _enums: JMap[String, JMap[String, String]]
            ): ClassSpec = {
    val meta = Option(_meta)

    val seq: List[AttrSpec] = if (_seq == null) {
      List()
    } else {
      _seq.toList
    }
    val types: Map[String, ClassSpec] = if (_types == null) {
      Map()
    } else {
      _types.toMap
    }
    val instances: Map[String, InstanceSpec] = if (_instances == null) {
      Map()
    } else {
      _instances.toMap
    }
    val enums: Map[String, Map[Long, String]] = if (_enums == null) {
      Map()
    } else {
      _enums.toMap.map { case(k, v) => (k, v.toMap.map { case (enumId, enumLabel) => (Utils.strToLong(enumId), enumLabel) }) }
    }

    ClassSpec(meta, seq, types, instances, enums)
  }
}
