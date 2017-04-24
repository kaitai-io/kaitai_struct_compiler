package io.kaitai.struct

case class RuntimeConfig(
  debug: Boolean = false,
  opaqueTypes: Boolean = false,
  goPackage: String = "",
  javaPackage: String = "",
  dotNetNamespace: String = "Kaitai",
  phpNamespace: String = ""
)
