package io.kaitai.struct

case class RuntimeConfig(
  debug: Boolean = false,
  opaqueTypes: Boolean = false,
  readWrite: Boolean = false,
  javaPackage: String = "",
  dotNetNamespace: String = "Kaitai",
  phpNamespace: String = ""
)
