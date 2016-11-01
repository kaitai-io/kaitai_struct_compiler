package io.kaitai.struct.format

import java.util.{List => JList, Map => JMap}

import com.fasterxml.jackson.annotation.{JsonCreator, JsonProperty}
import io.kaitai.struct.Utils

import scala.collection.JavaConversions._

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
  val LEGAL_KEYS = Set(
    "meta",
    "seq",
    "types",
    "instances",
    "enums"
  )

  def fromYaml(src: Any, path: List[String]) = {
    src match {
      case srcMap: Map[String, AnyRef] =>
        ParseUtils.ensureLegalKeys(srcMap, LEGAL_KEYS, path)

        val meta = srcMap.get("meta").map(MetaSpec.fromYaml(_, path ++ List("meta")))
        val seq: List[AttrSpec] = srcMap.get("seq") match {
          case Some(value) => seqFromYaml(value, path ++ List("seq"))
          case None => List()
        }
        val types: Map[String, ClassSpec] = srcMap.get("types") match {
          case Some(value) => typesFromYaml(value, path ++ List("types"))
          case None => Map()
        }
        val instances = Map[InstanceIdentifier, InstanceSpec]()
        val enums = Map[String, Map[Long, String]]()

        if (path.isEmpty && meta.isEmpty)
          throw new YAMLParseException("no `meta` encountered in top-level class spec", path)

        ClassSpec(meta, seq, types, instances, enums)
      case unknown =>
        throw new YAMLParseException(s"expected map, found $unknown", path)
    }
  }

  def seqFromYaml(src: AnyRef, path: List[String]): List[AttrSpec] = {
    src match {
      case srcList: List[AnyRef] =>
        srcList.zipWithIndex.map { case (attrSrc, idx) =>
          AttrSpec.fromYaml(attrSrc, path ++ List(idx.toString))
        }
      case unknown =>
        throw new YAMLParseException(s"expected array, found $unknown", path)
    }
  }

  def typesFromYaml(src: AnyRef, path: List[String]): Map[String, ClassSpec] = {
    src match {
      case srcMap: Map[String, AnyRef] =>
        srcMap.map { case (typeName, body) =>
          typeName -> ClassSpec.fromYaml(body, path ++ List(typeName))
        }
      case unknown =>
        throw new YAMLParseException(s"expected map, found $unknown", path)
    }
  }

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
