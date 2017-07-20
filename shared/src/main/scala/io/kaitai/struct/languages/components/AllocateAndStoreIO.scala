package io.kaitai.struct.languages.components

import io.kaitai.struct.format.{AttrSpec, Identifier, RepeatSpec}

import scala.collection.mutable.ListBuffer

/**
  * Allocates new IO and returns attribute identifier that it will be stored
  * at. This is used for languages without garbage collection that need to
  * keep track of allocated IOs.
  */
trait AllocateAndStoreIO {
  def allocateIO(id: Identifier, rep: RepeatSpec, extraAttrs: ListBuffer[AttrSpec]): String
}
