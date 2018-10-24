package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.format._

/**
  * Generates list of extra attributes required to store intermediate /
  * virtual stuff for every attribute like:
  *
  * * buffered raw value byte arrays
  * * IO objects (?)
  * * unprocessed / postprocessed byte arrays
  */
object ExtraAttrs {
  def forClassSpec(curClass: ClassSpec): List[AttrSpec] = {
    // We want only values of ParseInstances, which are AttrSpecLike.
    // ValueInstances are ignored, as they can't currently generate
    // any extra attributes (i.e. no `size`, no `process`, etc)
    val parseInstances = curClass.instances.values.collect {
      case inst: AttrLikeSpec => inst
    }

    (curClass.seq ++ parseInstances).foldLeft(List[AttrSpec]())(
      (attrs, attr) => attrs ++ ExtraAttrs.forAttr(attr)
    )
  }

  def forAttr(attr: AttrLikeSpec): List[AttrSpec] =
    forAttr(attr.id, attr.dataType, attr.cond)

  private
  def forAttr(id: Identifier, dataType: DataType, condSpec: ConditionalSpec): List[AttrSpec] = {
    dataType match {
      case bt: BytesType =>
        bt.process match {
          case None => List()
          case Some(_) =>
            val rawId = RawIdentifier(id)
            List(AttrSpec(List(), rawId, bt, condSpec))
        }
      case utb: UserTypeFromBytes =>
        val rawId = RawIdentifier(id)
        List(AttrSpec(List(), rawId, utb.bytes, condSpec)) ++ forAttr(rawId, utb.bytes, condSpec)
      case _ =>
        List()
    }
  }
}
