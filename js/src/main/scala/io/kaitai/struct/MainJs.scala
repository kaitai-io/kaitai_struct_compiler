package io.kaitai.struct

import io.kaitai.struct.format.JSClassSpec
import io.kaitai.struct.languages._

import scala.scalajs.js.annotation.JSExport

@JSExport
object MainJs {
  @JSExport
  def compile(lang: String, yaml: JSClassSpec): String = {
    val config = new RuntimeConfig(verbose = true)

    val (out, cc) = ClassCompiler.fromClassSpecToString(yaml.toScala, LanguageCompilerStatic.byString(lang), config)
    cc.compile
    out.result
  }
}
