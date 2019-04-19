package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.format._

/**
  * Trait to be implemented by all [[LanguageCompiler]] compilers: supplies extra attributes
  * when we'll be allocating new IOs.
  */
trait ExtraAttrs {
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

  def forAttr(attr: AttrLikeSpec, compiler: ExtraAttrs): Iterable[AttrSpec] =
    forAttr(attr.id, attr.dataType, attr.cond, compiler)

  private
  def forAttr(id: Identifier, dataType: DataType, condSpec: ConditionalSpec, compiler: ExtraAttrs): Iterable[AttrSpec] = {
    dataType match {
      case bt: BytesType =>
        // Byte array: only need extra attrs if `process` is used
        bt.process match {
          case None => List()
          case Some(_) =>
            val rawId = RawIdentifier(id)
            List(AttrSpec(List(), rawId, bt, condSpec)) ++
              compiler.extraAttrForIO(id, condSpec.repeat)
        }
      case utb: UserTypeFromBytes =>
        // User type in a substream
        val rawId = RawIdentifier(id)
        (List(AttrSpec(List(), rawId, utb.bytes, condSpec)) ++
          compiler.extraAttrForIO(rawId, condSpec.repeat) ++
          forAttr(rawId, utb.bytes, condSpec, compiler)).toList.distinct
      case st: SwitchType =>
        st.cases.flatMap { case (_, caseType) =>
          forAttr(id, caseType, condSpec, compiler)
        }.toList.distinct
      case _ =>
        List()
    }
  }
}
