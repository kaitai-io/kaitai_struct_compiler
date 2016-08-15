package io.kaitai.struct.languages

import io.kaitai.struct.format.{Identifier, RepeatSpec}

/**
  * Allocates new auxiliary IOs as local vars - no references saved and thus
  * probably garbage collector will deal with them.
  */
trait AllocateIOLocalVar {
  def allocateIO(varName: Identifier, rep: RepeatSpec): String
}
