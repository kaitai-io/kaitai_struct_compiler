package io.kaitai.struct

import io.kaitai.struct.translators.TypeProvider

trait AbstractCompiler extends TypeProvider {
  def compile: Unit
}
