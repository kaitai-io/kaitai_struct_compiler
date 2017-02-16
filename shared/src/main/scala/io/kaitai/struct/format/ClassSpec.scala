package io.kaitai.struct.format

/**
  * Type that we use when we want to refer to a class specification or something
  * close, but not (yet) that well defined.
  */
sealed trait ClassSpecLike
case object UnknownClassSpec extends ClassSpecLike
case object GenericStructClassSpec extends ClassSpecLike

case class ClassSpec(
                      meta: Option[MetaSpec],
                      doc: Option[String],
                      seq: List[AttrSpec],
                      types: Map[String, ClassSpec],
                      instances: Map[InstanceIdentifier, InstanceSpec],
                      enums: Map[String, EnumSpec]
                    ) extends ClassSpecLike {
  var parentClass: ClassSpecLike = UnknownClassSpec

  /**
    * Full absolute name of the class (including all names of classes that
    * it's nested into, as a namespace). Derived either from `meta`/`id`
    * (for top-level classes), or from keys in `types` (for nested classes).
    */
  var name = List[String]()

  /**
    * @return Absolute name of class as string, components separated by
    *         double colon operator `::`
    */
  def nameAsStr = name.mkString("::")

  /**
    * The class specification that this class is nested into, if it exists.
    * For top-level classes, it's None.
    */
  var upClass: Option[ClassSpec] = None

  def parentTypeName: List[String] = parentClass match {
    case UnknownClassSpec | GenericStructClassSpec => List("kaitai_struct")
    case t: ClassSpec => t.name
  }
}

object ClassSpec {
  val LEGAL_KEYS = Set(
    "meta",
    "doc",
    "seq",
    "types",
    "instances",
    "enums"
  )

  def fromYaml(src: Any, path: List[String], metaDef: MetaDefaults): ClassSpec = {
    val srcMap = ParseUtils.asMapStr(src, path)
    ParseUtils.ensureLegalKeys(srcMap, LEGAL_KEYS, path)

    val meta = srcMap.get("meta").map(MetaSpec.fromYaml(_, path ++ List("meta")))
    val curMetaDef = metaDef.updateWith(meta)

    val doc = ParseUtils.getOptValueStr(srcMap, "doc", path)

    val seq: List[AttrSpec] = srcMap.get("seq") match {
      case Some(value) => seqFromYaml(value, path ++ List("seq"), curMetaDef)
      case None => List()
    }
    val types: Map[String, ClassSpec] = srcMap.get("types") match {
      case Some(value) => typesFromYaml(value, path ++ List("types"), curMetaDef)
      case None => Map()
    }
    val instances: Map[InstanceIdentifier, InstanceSpec] = srcMap.get("instances") match {
      case Some(value) => instancesFromYaml(value, path ++ List("instances"), curMetaDef)
      case None => Map()
    }
    val enums: Map[String, EnumSpec] = srcMap.get("enums") match {
      case Some(value) => enumsFromYaml(value, path ++ List("enums"))
      case None => Map()
    }

    if (path.isEmpty) {
      if (meta.isEmpty)
        throw new YAMLParseException("no `meta` encountered in top-level class spec", path)
      if (meta.get.id.isEmpty)
        throw new YAMLParseException("no `meta/id` encountered in top-level class spec", path ++ List("meta", "id"))
    }

    ClassSpec(meta, doc, seq, types, instances, enums)
  }

  def seqFromYaml(src: Any, path: List[String], metaDef: MetaDefaults): List[AttrSpec] = {
    src match {
      case srcList: List[Any] =>
        srcList.zipWithIndex.map { case (attrSrc, idx) =>
          AttrSpec.fromYaml(attrSrc, path ++ List(idx.toString), metaDef, idx)
        }
      case unknown =>
        throw new YAMLParseException(s"expected array, found $unknown", path)
    }
  }

  def typesFromYaml(src: Any, path: List[String], metaDef: MetaDefaults): Map[String, ClassSpec] = {
    val srcMap = ParseUtils.asMapStr(src, path)
    srcMap.map { case (typeName, body) =>
      typeName -> ClassSpec.fromYaml(body, path ++ List(typeName), metaDef)
    }
  }

  def instancesFromYaml(src: Any, path: List[String], metaDef: MetaDefaults): Map[InstanceIdentifier, InstanceSpec] = {
    val srcMap = ParseUtils.asMap(src, path)
    srcMap.map { case (key, body) =>
      val instName = ParseUtils.asStr(key, path)
      val id = InstanceIdentifier(instName)
      // TODO: check this conversion
      id -> InstanceSpec.fromYaml(body, path ++ List(instName), metaDef, id)
    }
  }

  def enumsFromYaml(src: Any, path: List[String]): Map[String, EnumSpec] = {
    val srcMap = ParseUtils.asMap(src, path)
    srcMap.map { case (key, body) =>
      val enumName = ParseUtils.asStr(key, path)
      enumName -> EnumSpec.fromYaml(body, path ++ List(enumName))
    }
  }

  def fromYaml(src: Any): ClassSpec = fromYaml(src, List(), MetaDefaults(None, None))

  def opaquePlaceholder(typeName: List[String]): ClassSpec = {
    val placeholder = ClassSpec(
      meta = Some(MetaSpec.OPAQUE),
      doc = None,
      seq = List(),
      types = Map(),
      instances = Map(),
      enums = Map()
    )
    placeholder.name = typeName
    placeholder
  }
}
