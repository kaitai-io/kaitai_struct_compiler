package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.format._

/**
  * Trait to be implemented by all [[LanguageCompiler]] compilers: supplies extra attributes
  * when we'll be allocating new IOs.
  */
trait ExtraAttrs {
  /**
    * Provides a collection of extra attributes which will be necessary to store in a class for
    * handling of a single "normal" attribute. Primarily
    * @param id ID of attribute
    * @param dataType data type of attribute
    * @param condSpec conditional spec of attribute
    * @return a collection of extra attributes
    */
  def extraAttrsForAttribute(id: Identifier, dataType: DataType, condSpec: ConditionalSpec): Iterable[AttrSpec] = {
    dataType match {
      case bt: BytesType =>
        // Byte array: only need extra attrs if `process` is used
        bt.process match {
          case None => List()
          case Some(_) =>
            val rawId = RawIdentifier(id)
            List(AttrSpec(List(), rawId, bt, condSpec))
        }
      case utb: UserTypeFromBytes =>
        // User type in a substream
        val rawId = RawIdentifier(id)
        (extraRawAttrForUserTypeFromBytes(id, utb, condSpec) ++
          extraAttrForIO(rawId, condSpec.repeat) ++
          extraAttrsForAttribute(rawId, utb.bytes, condSpec)).toList.distinct
      case st: SwitchType =>
        st.cases.flatMap { case (_, caseType) =>
          extraAttrsForAttribute(id, caseType, condSpec)
        }.toList.distinct
      case _ =>
        List()
    }
  }

  /**
    * Provides a collection of extra attributes that will be necessary to track raw byte
    * arrays associated with handling of a specific attribute.
    *
    * Default implementation provides one byte array unconditionally. Per-language implementations
    * might skip storage of byte arrays in certain situations (e.g. substreams which are not storing
    * any byte arrays).
    *
    * @param id original ID of the attribute (not raw ID!)
    * @param ut user type from bytes
    * @param condSpec conditional spec of the attribute
    * @return collection of extra attributes necessary to track raw byte arrays
    */
  def extraRawAttrForUserTypeFromBytes(id: Identifier, ut: UserTypeFromBytes, condSpec: ConditionalSpec): List[AttrSpec] =
    List(AttrSpec(List(), RawIdentifier(id), ut.bytes, condSpec))

  def extraAttrForIO(id: Identifier, rep: RepeatSpec): List[AttrSpec]
}

/**
  * Generates list of extra attributes required to store intermediate /
  * virtual stuff for every attribute like:
  *
  * * buffered raw value byte arrays
  * * IO objects (?)
  * * unprocessed / postprocessed byte arrays
  */
object ExtraAttrs {
  def forClassSpec(curClass: ClassSpec, compiler: ExtraAttrs): List[AttrSpec] = {
    // We want only values of ParseInstances, which are AttrSpecLike.
    // ValueInstances are ignored, as they can't currently generate
    // any extra attributes (i.e. no `size`, no `process`, etc)
    val parseInstances = curClass.instances.values.collect {
      case inst: AttrLikeSpec => inst
    }

    (curClass.seq ++ parseInstances).foldLeft(List[AttrSpec]())(
      (attrs, attr) => attrs ++ ExtraAttrs.forAttr(attr, compiler)
    )
  }

  def forAttr(attr: AttrLikeSpec, compiler: ExtraAttrs): Iterable[AttrSpec] = {
    compiler.extraAttrsForAttribute(attr.id, attr.dataType, attr.cond)
  }
}
