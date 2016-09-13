package io.kaitai.struct.format

import java.util.{List => JList, Map => JMap}

import io.kaitai.struct.Utils

import collection.JavaConversions._
import com.fasterxml.jackson.annotation.{JsonCreator, JsonProperty}

sealed trait ClassSpecLike
case object UnknownClassSpec extends ClassSpecLike
case object GenericStructClassSpec extends ClassSpecLike

case class ClassSpec(
                      meta: Option[MetaSpec],
                      seq: List[AttrSpec],
                      types: Map[String, ClassSpec],
                      instances: Map[InstanceIdentifier, InstanceSpec],
                      enums: Map[String, Map[Long, String]]
                    ) extends ClassSpecLike {
  var parentClass: ClassSpecLike = UnknownClassSpec
  var name = List[String]()
  var upClass: Option[ClassSpec] = None

  def parentTypeName: List[String] = parentClass match {
    case UnknownClassSpec | GenericStructClassSpec => List("kaitai_struct")
    case t: ClassSpec => t.name
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
