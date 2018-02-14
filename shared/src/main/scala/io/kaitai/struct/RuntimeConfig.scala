package io.kaitai.struct

case class RuntimeConfig(
  debug: Boolean = false,
  opaqueTypes: Boolean = false,
  goPackage: String = "",
  javaPackage: String = "",
  javaFromFileClass: String = "io.kaitai.struct.ByteBufferKaitaiStream",
  dotNetNamespace: String = "Kaitai",
  phpNamespace: String = "",
  pythonPackage: String = ""
)
