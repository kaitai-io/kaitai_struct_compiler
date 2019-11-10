package io.kaitai.struct.languages.components

import io.kaitai.struct.format.{AttrSpec, Identifier, RepeatSpec}

/**
  * Allocates new auxiliary IOs as local vars - no references saved and thus
  * probably garbage collector will deal with them.
  */
trait AllocateIOLocalVar extends ExtraAttrs {
  def allocateIO(varName: Identifier, rep: RepeatSpec): String

  /**
    * Allocates a fixed-size KaitaiStream IO object for writing into it.
    * @param varName variable name to use to generate IO name
    * @param size size expression to use
    * @return name of generated IO local variable as string
    */
  def allocateIOFixed(varName: Identifier, size: String): String = ???

  /**
    * Allocates a growing KaitaiStream IO object for writing into it.
    * This one is expected to grow indefinitely as one writes more data
    * into it, never raising a buffer overflow exception
    * @param varName variable name to use to generate IO name
    * @return name of generated IO local variable as string
    */
  def allocateIOGrowing(varName: Identifier): String = ???
  override def extraAttrForIO(id: Identifier, rep: RepeatSpec): List[AttrSpec] = List()
}
