package io.kaitai.struct.languages

import io.kaitai.struct.format.{Identifier, IoStorageIdentifier, RepeatSpec}

/**
  * Allocates new IO and returns attribute identifier that it will be stored
  * at. This is used for languages without garbage collection that need to
  * keep track of allocated IOs.
  */
trait AllocateAndStoreIO {
  def allocateIO(varName: Identifier, rep: RepeatSpec): IoStorageIdentifier
}
