package io.kaitai.struct.languages.components

import io.kaitai.struct.StringLanguageOutputWriter
import io.kaitai.struct.format.ClassSpec

/**
  * Common trait for languages that have one output file per ClassSpec.
  */
trait SingleOutputFile extends LanguageCompiler {
  val out = new StringLanguageOutputWriter(indent)

  override def results(topClass: ClassSpec) =
    Map(outFileName(topClass.nameAsStr) -> out.result)
}
