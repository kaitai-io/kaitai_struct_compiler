package io.kaitai.struct.languages.components

import io.kaitai.struct._
import io.kaitai.struct.languages._

trait LanguageCompilerStatic {
  def getCompiler(tp: ClassTypeProvider, config: RuntimeConfig): LanguageCompiler
}

object LanguageCompilerStatic {
  val NAME_TO_CLASS: Map[String, LanguageCompilerStatic] = Map(
    "construct" -> ConstructClassCompiler,
    "cpp_stl" -> CppCompiler,
    "csharp" -> CSharpCompiler,
    "graphviz" -> GraphvizClassCompiler,
    "go" -> GoCompiler,
    "html" -> HtmlClassCompiler,
    "java" -> JavaCompiler,
    "javascript" -> JavaScriptCompiler,
    "lua" -> LuaCompiler,
    "nim" -> NimCompiler,
    "perl" -> PerlCompiler,
    "php" -> PHPCompiler,
    "python" -> PythonCompiler,
    "ruby" -> RubyCompiler,
    "rust" -> RustCompiler
  )

  val CLASS_TO_NAME: Map[LanguageCompilerStatic, String] = NAME_TO_CLASS.map(_.swap)

  def byString(langName: String): LanguageCompilerStatic = NAME_TO_CLASS(langName)
}
