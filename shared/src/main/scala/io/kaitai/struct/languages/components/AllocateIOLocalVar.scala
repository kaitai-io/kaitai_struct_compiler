package io.kaitai.struct.languages.components

import io.kaitai.struct.format.{AttrSpec, Identifier, RepeatSpec}

/**
  * Allocates new auxiliary IOs as local vars - no references saved and thus
  * probably garbage collector will deal with them.
  */
trait AllocateIOLocalVar extends ExtraAttrs {
  def allocateIO(varName: Identifier, rep: RepeatSpec): String

  override def extraAttrForIO(id: Identifier, rep: RepeatSpec): List[AttrSpec] = List()
}
