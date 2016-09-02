package io.kaitai.struct.format

import java.util.{List => JList, Map => JMap}

import io.kaitai.struct.Utils

import collection.JavaConversions._
import com.fasterxml.jackson.annotation.{JsonCreator, JsonProperty}

sealed trait NamedClassSpec

case object UnknownNamedClass extends NamedClassSpec
case class NamedClass(name: List[String], spec: ClassSpec) extends NamedClassSpec
case object GenericStructClass extends NamedClassSpec

case class ClassSpec(
                      meta: Option[MetaSpec],
                      seq: List[AttrSpec],
                      types: Map[String, ClassSpec],
                      instances: Map[InstanceIdentifier, InstanceSpec],
                      enums: Map[String, Map[Long, String]]
                    ) {
  var _parentType: NamedClassSpec = UnknownNamedClass
  var name = List[String]()
  var upClass: Option[ClassSpec] = None

  def parentTypeName: List[String] = _parentType match {
    case UnknownNamedClass | GenericStructClass => List("kaitai_struct")
    case NamedClass(name: List[String], _) => name
  }
  def parentType: ClassSpec = _parentType match {
    case NamedClass(_, spec) => spec
  }
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
    val instances: Map[InstanceIdentifier, InstanceSpec] = if (_instances == null) {
      Map()
    } else {
      _instances.toMap.map { case (k, v) => InstanceIdentifier(k) -> v }
    }
    val enums: Map[String, Map[Long, String]] = if (_enums == null) {
      Map()
    } else {
      _enums.toMap.map { case(k, v) =>
        k -> v.toMap.map { case (enumId, enumLabel) => (Utils.strToLong(enumId), enumLabel) }
      }
    }

    ClassSpec(meta, seq, types, instances, enums)
  }
}
