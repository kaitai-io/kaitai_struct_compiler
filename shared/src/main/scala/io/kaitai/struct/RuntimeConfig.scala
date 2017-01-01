package io.kaitai.struct

case class RuntimeConfig(
  verbose: Boolean = false,
  debug: Boolean = false,
  javaPackage: String = "",
  dotNetNamespace: String = "Kaitai",
  phpNamespace: String = ""
)
