package io.kaitai.struct

case class RuntimeConfig(
  debug: Boolean = false,
  opaqueTypes: Boolean = false,
  javaPackage: String = "",
  dotNetNamespace: String = "Kaitai",
  phpNamespace: String = ""
)
