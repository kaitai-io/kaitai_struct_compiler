package io.kaitai.struct

trait AbstractCompiler {
  def compile: CompileLog.SpecSuccess
}
