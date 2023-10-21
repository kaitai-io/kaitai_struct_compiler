package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype.DataType.{ArrayTypeInStream, OwnedKaitaiStreamType}
import io.kaitai.struct.format._

/**
  * Allocates new IO and returns attribute identifier that it will be stored
  * at. This is used for languages without garbage collection that need to
  * keep track of allocated IOs.
  */
trait AllocateAndStoreIO extends ExtraAttrs {
  def allocateIO(id: Identifier, rep: RepeatSpec): String

  override def extraAttrForIO(id: Identifier, rep: RepeatSpec): List[AttrSpec] =
    List(AttrSpec(List(), IoStorageIdentifier(id), OwnedKaitaiStreamType, ConditionalSpec(None, rep)))
}
