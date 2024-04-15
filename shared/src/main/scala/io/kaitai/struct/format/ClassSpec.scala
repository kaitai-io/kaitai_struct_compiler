package io.kaitai.struct.format

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.problems.KSYParseError

import scala.collection.immutable.SortedMap
import scala.collection.mutable

/**
  * Type that we use when we want to refer to a class specification or something
  * close, but not (yet) that well defined.
  */
sealed trait ClassSpecLike {
  def toDataType: DataType
}
/**
  * Type was not yet calculated. If that type returned during calculation, then
  * cyclic reference is present and it is impossible to calculate actual type.
  *
  * Parent type of each KSY-defined type initialized to that value and refined
  * later based on type usage.
  */
case object UnknownClassSpec extends ClassSpecLike {
  override def toDataType: DataType = CalcKaitaiStructType()
}
/**
  * Type is calculated as a type that able to store any KSY-defined type.
  * Usually it have a name `KaitaiStruct` in corresponding language runtime library.
  */
case object GenericStructClassSpec extends ClassSpecLike {
  override def toDataType: DataType = CalcKaitaiStructType()
}

sealed trait Sized
case object DynamicSized extends Sized
case object NotCalculatedSized extends Sized
case object StartedCalculationSized extends Sized
case class FixedSized(n: Int) extends Sized

case class ClassSpec(
  fileName: Option[String],
  path: List[String],
  isTopLevel: Boolean,
  meta: MetaSpec,
  doc: DocSpec,
  toStringExpr: Option[Ast.expr],
  params: List[ParamDefSpec],
  seq: List[AttrSpec],
  types: SortedMap[String, ClassSpec],
  instances: SortedMap[InstanceIdentifier, InstanceSpec],
  enums: SortedMap[String, EnumSpec]
) extends ClassSpecLike with YAMLPath {
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
  def nameAsStr: String = name.mkString("::")

  /**
    * @return Name of the file this type originates from, or, worst case,
    *         if filename is unknown, name of the type.
    */
  def fileNameAsStr: String = fileName.getOrElse(nameAsStr)

  /**
    * The class specification that this class is nested into, if it exists.
    * For top-level classes, it's None.
    */
  var upClass: Option[ClassSpec] = None

  var seqSize: Sized = NotCalculatedSized

  def toDataType: DataType = {
    val cut = CalcUserType(name, None)
    cut.classSpec = Some(this)
    cut
  }

  def parentType: DataType = parentClass.toDataType

  /**
    * Determines whether this `ClassSpec` represents a type that is external
    * (i.e. not defined in the same .ksy file) from the perspective of the given
    * `ClassSpec`.
    * @param curClass class spec from which the local/external relationship
    * should be evaluated
    */
  def isExternal(curClass: ClassSpec): Boolean =
    name.head != curClass.name.head

  /**
    * Recursively traverses tree of types starting from this type, calling
    * certain function for every type, starting from this one.
    *
    * @param proc function to execute on every encountered type.
    */
  def forEachRec(proc: (ClassSpec) => Unit): Unit = {
    proc.apply(this)
    types.foreach { case (_, typeSpec) =>
      typeSpec.forEachRec(proc)
    }
  }

  /**
    * Recursively traverses tree of types starting from this type, calling
    * certain function for every type, starting from this one.
    *
    * @param proc function to execute on every encountered type.
    * @tparam R mandates that function must return a list of this type.
    */
  def mapRec[R](proc: (ClassSpec) => Iterable[R]): Iterable[R] = {
    val r1 = proc.apply(this)
    val r2 = types.flatMap { case (_, typeSpec) =>
      typeSpec.mapRec(proc)
    }
    r1 ++ r2
  }

  override def equals(obj: Any): Boolean = obj match {
    case other: ClassSpec =>
      path == other.path &&
      isTopLevel == other.isTopLevel &&
      meta == other.meta &&
      doc == other.doc &&
      params == other.params &&
      seq == other.seq &&
      types == other.types &&
      instances == other.instances &&
      enums == other.enums &&
      name == other.name
    case _ => false
  }
}

object ClassSpec {
  val LEGAL_KEYS = Set(
    "meta",
    "doc",
    "doc-ref",
    "to-string",
    "params",
    "seq",
    "types",
    "instances",
    "enums"
  )

  def fromYaml(src: Any, fileName: Option[String], path: List[String], metaDef: MetaSpec): ClassSpec = {
    val srcMap = ParseUtils.asMapStr(src, path)
    ParseUtils.ensureLegalKeys(srcMap, LEGAL_KEYS, path)

    val metaPath = path ++ List("meta")
    val explicitMeta = srcMap.get("meta").map(MetaSpec.fromYaml(_, metaPath)).getOrElse(MetaSpec.emptyWithPath(metaPath))
    val meta = explicitMeta.fillInDefaults(metaDef)

    val doc = DocSpec.fromYaml(srcMap, path)

    val toStringExpr = ParseUtils.getOptValueExpression(srcMap, "to-string", path)

    val params: List[ParamDefSpec] = srcMap.get("params") match {
      case Some(value) => paramDefFromYaml(value, path ++ List("params"))
      case None => List()
    }
    val seq: List[AttrSpec] = srcMap.get("seq") match {
      case Some(value) => seqFromYaml(value, path ++ List("seq"), meta)
      case None => List()
    }
    val instances: SortedMap[InstanceIdentifier, InstanceSpec] = srcMap.get("instances") match {
      case Some(value) => instancesFromYaml(value, path ++ List("instances"), meta)
      case None => SortedMap()
    }

    checkDupMemberIds(params ++ seq ++ instances.values)

    val types: SortedMap[String, ClassSpec] = srcMap.get("types") match {
      case Some(value) => typesFromYaml(value, fileName, path ++ List("types"), meta)
      case None => SortedMap()
    }
    val enums: SortedMap[String, EnumSpec] = srcMap.get("enums") match {
      case Some(value) => enumsFromYaml(value, path ++ List("enums"))
      case None => SortedMap()
    }

    val cs = ClassSpec(
      fileName, path, path.isEmpty,
      meta, doc, toStringExpr,
      params, seq, types, instances, enums
    )

    // If that's a top-level class, set its name from meta/id
    if (path.isEmpty) {
      explicitMeta.id match {
        case None =>
          throw KSYParseError.withText("no `meta/id` encountered in top-level class spec", path ++ List("meta"))
        case Some(id) =>
          cs.name = List(id)
      }
    }

    cs
  }

  def paramDefFromYaml(src: Any, path: List[String]): List[ParamDefSpec] = {
    src match {
      case srcList: List[Any] =>
        val params = srcList.zipWithIndex.map { case (attrSrc, idx) =>
          ParamDefSpec.fromYaml(attrSrc, path ++ List(idx.toString), idx)
        }
        params
      case unknown =>
        throw KSYParseError.withText(s"expected array, found $unknown", path)
    }
  }

  def seqFromYaml(src: Any, path: List[String], metaDef: MetaSpec): List[AttrSpec] = {
    src match {
      case srcList: List[Any] =>
        val seq = srcList.zipWithIndex.map { case (attrSrc, idx) =>
          AttrSpec.fromYaml(attrSrc, path ++ List(idx.toString), metaDef, idx)
        }
        seq
      case unknown =>
        throw KSYParseError.withText(s"expected array, found $unknown", path)
    }
  }

  def checkDupMemberIds(attrs: List[MemberSpec]): Unit = {
    val attrIds = mutable.Map[String, MemberSpec]()
    attrs.foreach { (attr) =>
      val idOpt: Option[String] = attr.id match {
        case NamedIdentifier(name) => Some(name)
        case InstanceIdentifier(name) => Some(name)
        case _ => None // do nothing with non-named IDs
      }
      idOpt.foreach { (id) =>
        checkDupId(attrIds.get(id), id, attr)
        attrIds.put(id, attr)
      }
    }
  }

  private def checkDupId(prevAttrOpt: Option[MemberSpec], id: String, nowAttr: YAMLPath): Unit = {
    prevAttrOpt match {
      case Some(prevAttr) =>
        throw KSYParseError.withText(
          s"duplicate attribute ID '$id', previously defined at /${prevAttr.pathStr}",
          nowAttr.path
        )
      case None =>
        // no dups, ok
    }
  }

  def typesFromYaml(src: Any, fileName: Option[String], path: List[String], metaDef: MetaSpec): SortedMap[String, ClassSpec] = {
    val srcMap = ParseUtils.asMapStr(src, path)
    SortedMap.from(
      srcMap.map { case (typeName, body) =>
        Identifier.checkIdentifierSource(typeName, "type", path ++ List(typeName))
        typeName -> ClassSpec.fromYaml(body, fileName, path ++ List(typeName), metaDef)
      }
    )
  }

  def instancesFromYaml(src: Any, path: List[String], metaDef: MetaSpec): SortedMap[InstanceIdentifier, InstanceSpec] = {
    val srcMap = ParseUtils.asMap(src, path)
    SortedMap.from(
      srcMap.map { case (key, body) =>
        val instName = ParseUtils.asStr(key, path)
        Identifier.checkIdentifierSource(instName, "instance", path ++ List(instName))
        val id = InstanceIdentifier(instName)
        id -> InstanceSpec.fromYaml(body, path ++ List(instName), metaDef, id)
      }
    )
  }

  def enumsFromYaml(src: Any, path: List[String]): SortedMap[String, EnumSpec] = {
    val srcMap = ParseUtils.asMap(src, path)
    SortedMap.from(
      srcMap.map { case (key, body) =>
        val enumName = ParseUtils.asStr(key, path)
        Identifier.checkIdentifierSource(enumName, "enum", path ++ List(enumName))
        enumName -> EnumSpec.fromYaml(body, path ++ List(enumName))
      }
    )
  }

  def fromYaml(src: Any, fileName: Option[String]): ClassSpec = fromYaml(src, fileName, List(), MetaSpec.OPAQUE)

  def opaquePlaceholder(typeName: List[String]): ClassSpec = {
    val placeholder = ClassSpec(
      fileName = None,
      path = List(),
      isTopLevel = true,
      meta = MetaSpec.OPAQUE,
      doc = DocSpec.EMPTY,
      toStringExpr = None,
      params = List(),
      seq = List(),
      types = SortedMap(),
      instances = SortedMap(),
      enums = SortedMap()
    )
    placeholder.name = typeName
    placeholder
  }
}
