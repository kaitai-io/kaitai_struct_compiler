package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype.DataType.{ArrayTypeInStream, OwnedKaitaiStreamType}
import io.kaitai.struct.format._

/**
  * Allocates new IO and returns attribute identifier that it will be stored
  * at. This is used for languages without garbage collection that need to
  * keep track of allocated IOs.
  */
trait AllocateAndStoreIO extends ExtraAttrs {
  /**
   * Generates code that create new input stream for read specified field content
   *
   * @param id Name of variable that used to store field for which stream is created
   * @param rep Repeat specification for the [[id]] field
   * @param currentIo Name of current stream variable, that used to read data currently
   */
  def allocateIO(id: Identifier, rep: RepeatSpec, currentIo: String): String

  override def extraAttrForIO(id: Identifier, rep: RepeatSpec): List[AttrSpec] =
    List(AttrSpec(List(), IoStorageIdentifier(id), OwnedKaitaiStreamType, ConditionalSpec(None, rep)))
}
