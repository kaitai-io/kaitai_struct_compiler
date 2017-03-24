package io.kaitai.struct.languages.components

import io.kaitai.struct._
import io.kaitai.struct.languages._
import io.kaitai.struct.translators.{BaseTranslator, TypeProvider}

trait LanguageCompilerStatic {
  def getCompiler(tp: ClassTypeProvider, config: RuntimeConfig): LanguageCompiler
  def getTranslator(tp: TypeProvider, config: RuntimeConfig): BaseTranslator
}

object LanguageCompilerStatic {
  val NAME_TO_CLASS: Map[String, LanguageCompilerStatic] = Map(
    "cpp_stl" -> CppCompiler,
    "csharp" -> CSharpCompiler,
    "graphviz" -> GraphvizClassCompiler,
    "java" -> JavaCompiler,
    "javascript" -> JavaScriptCompiler,
    "perl" -> PerlCompiler,
    "php" -> PHPCompiler,
    "python" -> PythonCompiler,
    "ruby" -> RubyCompiler
  )

  def byString(langName: String): LanguageCompilerStatic = NAME_TO_CLASS(langName)
}
