package io.kaitai.struct.languages.components

/**
  * Ensure that there are single source of names of classes that act as
  * KaitaiStruct and KaitaiStream in a compiler.
  */
trait StreamStructNames {
  def kstreamName: String
  def kstructName: String
}
