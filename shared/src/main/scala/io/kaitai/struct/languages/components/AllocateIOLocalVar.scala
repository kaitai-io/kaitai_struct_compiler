package io.kaitai.struct.languages.components

import io.kaitai.struct.format.{AttrSpec, Identifier, RepeatSpec}

/**
  * Allocates new auxiliary IOs as local vars - no references saved and thus
  * probably garbage collector will deal with them.
  */
trait AllocateIOLocalVar extends ExtraAttrs {
  /**
   * Generates code that create new input stream for read specified field content
   *
   * @param id Name of variable that used to store field for which stream is created
   * @param rep Repeat specification for the [[id]] field
   * @param currentIo Name of current stream variable, that used to read data currently
   */
  def allocateIO(varName: Identifier, rep: RepeatSpec, currentIo: String): String

  override def extraAttrForIO(id: Identifier, rep: RepeatSpec): List[AttrSpec] = List()
}
