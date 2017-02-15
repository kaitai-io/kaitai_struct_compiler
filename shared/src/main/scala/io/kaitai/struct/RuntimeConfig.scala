package io.kaitai.struct

case class RuntimeConfig(
  debug: Boolean = false,
  javaPackage: String = "",
  dotNetNamespace: String = "Kaitai",
  phpNamespace: String = ""
)
