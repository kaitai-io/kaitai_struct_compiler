package io.kaitai.struct

import io.kaitai.struct.format._
import io.kaitai.struct.languages.CCompiler

class CClassCompiler(
  classSpecs: ClassSpecs,
  override val topClass: ClassSpec,
  config: RuntimeConfig
) extends ClassCompiler(classSpecs, topClass, config, CCompiler) {}
