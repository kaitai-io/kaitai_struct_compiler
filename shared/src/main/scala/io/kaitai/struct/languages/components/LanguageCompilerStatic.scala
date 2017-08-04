package io.kaitai.struct.languages.components

import io.kaitai.struct._
import io.kaitai.struct.languages._
import io.kaitai.struct.translators.{BaseTranslator, TypeProvider}

trait LanguageCompilerStatic {
  def getCompiler(tp: ClassTypeProvider, config: RuntimeConfig): LanguageCompiler
}

object LanguageCompilerStatic {
  val NAME_TO_CLASS: Map[String, LanguageCompilerStatic] = Map(
    "cpp_stl" -> CppCompiler,
    "csharp" -> CSharpCompiler,
    "graphviz" -> GraphvizClassCompiler,
    "go" -> GoCompiler,
    "java" -> JavaCompiler,
    "javascript" -> JavaScriptCompiler,
    "lua" -> LuaCompiler,
    "perl" -> PerlCompiler,
    "php" -> PHPCompiler,
    "python" -> PythonCompiler,
    "ruby" -> RubyCompiler
  )

  val CLASS_TO_NAME: Map[LanguageCompilerStatic, String] = NAME_TO_CLASS.map(_.swap)

  def byString(langName: String): LanguageCompilerStatic = NAME_TO_CLASS(langName)
}
