package io.kaitai.struct

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components.{LanguageCompiler, LanguageCompilerStatic}
import io.kaitai.struct.translators.NimTranslator

class NimClassCompiler(classSpecs: ClassSpecs, topClass: ClassSpec) extends AbstractCompiler {
  import NimClassCompiler._

  val out = new StringLanguageOutputWriter(indent)
  val provider = new ClassTypeProvider(classSpecs, topClass)
  val importList = new ImportList
  val translator = new NimTranslator(provider, importList)

  override def compile: CompileLog.SpecSuccess = {
    out.puts("TEST")

    CompileLog.SpecSuccess(
      "",
      List(CompileLog.FileSuccess(
        outFileName(topClass.nameAsStr),
        outImports(topClass) + out.result
      ))
    )
  }

  def indent: String = "  "
  def outFileName(topClassName: String): String = s"$topClassName.nim"
  def outImports(topClass: ClassSpec) =
    "\n" + importList.toList.map((x) => s"import $x").mkString("\n") + "\n"
}

object NimClassCompiler extends LanguageCompilerStatic {
  // FIXME: Unused, should be probably separated from LanguageCompilerStatic
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = ???
}