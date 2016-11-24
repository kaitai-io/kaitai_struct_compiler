package io.kaitai.struct.format

import io.kaitai.struct.exprlang.DataType.Endianness

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

  def fromYaml(src: Any, path: List[String], defEndian: Option[Endianness]) = {
    val srcMap = ParseUtils.asMapStr(src, path)
    ParseUtils.ensureLegalKeys(srcMap, LEGAL_KEYS, path)

    val meta = srcMap.get("meta").map(MetaSpec.fromYaml(_, path ++ List("meta")))

    val curDefEndian: Option[Endianness] = meta.flatMap(_.endian) match {
      case Some(endianness) => Some(endianness)
      case None => defEndian
    }

    val seq: List[AttrSpec] = srcMap.get("seq") match {
      case Some(value) => seqFromYaml(value, path ++ List("seq"), curDefEndian)
      case None => List()
    }
    val types: Map[String, ClassSpec] = srcMap.get("types") match {
      case Some(value) => typesFromYaml(value, path ++ List("types"), curDefEndian)
      case None => Map()
    }
    val instances: Map[InstanceIdentifier, InstanceSpec] = srcMap.get("instances") match {
      case Some(value) => instancesFromYaml(value, path ++ List("instances"), curDefEndian)
      case None => Map()
    }
    val enums: Map[String, Map[Long, String]] = srcMap.get("enums") match {
      case Some(value) => enumsFromYaml(value, path ++ List("enums"))
      case None => Map()
    }

    if (path.isEmpty) {
      if (meta.isEmpty)
        throw new YAMLParseException("no `meta` encountered in top-level class spec", path)
      if (meta.get.id.isEmpty)
        throw new YAMLParseException("no `meta/id` encountered in top-level class spec", path ++ List("meta", "id"))
    }

    ClassSpec(meta, seq, types, instances, enums)
  }

  def seqFromYaml(src: Any, path: List[String], defEndian: Option[Endianness]): List[AttrSpec] = {
    src match {
      case srcList: List[Any] =>
        srcList.zipWithIndex.map { case (attrSrc, idx) =>
          AttrSpec.fromYaml(attrSrc, path ++ List(idx.toString), defEndian)
        }
      case unknown =>
        throw new YAMLParseException(s"expected array, found $unknown", path)
    }
  }

  def typesFromYaml(src: Any, path: List[String], defEndian: Option[Endianness]): Map[String, ClassSpec] = {
    val srcMap = ParseUtils.asMapStr(src, path)
    srcMap.map { case (typeName, body) =>
      typeName -> ClassSpec.fromYaml(body, path ++ List(typeName), defEndian)
    }
  }

  def instancesFromYaml(src: Any, path: List[String], defEndian: Option[Endianness]): Map[InstanceIdentifier, InstanceSpec] = {
    val srcMap = ParseUtils.asMap(src, path)
    srcMap.map { case (key, body) =>
      val instName = ParseUtils.asStr(key, path)
      val id = InstanceIdentifier(instName)
      // TODO: check this conversion
      id -> InstanceSpec.fromYaml(body, path ++ List(instName), defEndian)
    }
  }

  def enumsFromYaml(src: Any, path: List[String]): Map[String, Map[Long, String]] = {
    val srcMap = ParseUtils.asMap(src, path)
    srcMap.map { case (key, body) =>
      val enumName = ParseUtils.asStr(key, path)
      enumName -> enumFromYaml(body, path ++ List(enumName))
    }
  }

  def enumFromYaml(src: Any, path: List[String]): Map[Long, String] = {
    val srcMap = ParseUtils.asMap(src, path)
    srcMap.map { case (id, name) =>
      val idLong = ParseUtils.asLong(id, path)
      idLong -> ParseUtils.asStr(name, path ++ List(idLong.toString))
    }
  }

  def fromYaml(src: Any): ClassSpec = fromYaml(src, List(), None)
}
