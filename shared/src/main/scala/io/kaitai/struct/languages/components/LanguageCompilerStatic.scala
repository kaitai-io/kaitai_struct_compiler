package io.kaitai.struct.languages.components

import io.kaitai.struct.RuntimeConfig
import io.kaitai.struct.languages._
import io.kaitai.struct.translators.{BaseTranslator, TypeProvider}

trait LanguageCompilerStatic {
  def indent: String
  def outFileName(topClassName: String): String
  def outFilePath(config: RuntimeConfig, outDir: String, topClassName: String) = s"$outDir/${outFileName(topClassName)}"
  def getTranslator(tp: TypeProvider): BaseTranslator
}

object LanguageCompilerStatic {
  val NAME_TO_CLASS = Map(
    "cpp_stl" -> CppCompiler,
    "csharp" -> CSharpCompiler,
    "graphviz" -> GraphvizCompiler,
    "java" -> JavaCompiler,
    "javascript" -> JavaScriptCompiler,
    "php" -> PHPCompiler,
    "python" -> PythonCompiler,
    "ruby" -> RubyCompiler
  )

  def byString(langName: String): LanguageCompilerStatic = NAME_TO_CLASS(langName)
}
