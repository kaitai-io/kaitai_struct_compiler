package io.kaitai.struct

import io.kaitai.struct.CompileLog.FileSuccess
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{AttrSpec, _}
import io.kaitai.struct.languages.AwkwardCompiler
import io.kaitai.struct.languages.components.{ExtraAttrs, LanguageCompiler, LanguageCompilerStatic}

class AwkwardClassCompiler(
  classSpecs: ClassSpecs,
  override val topClass: ClassSpec,
  config: RuntimeConfig
) extends ClassCompiler(classSpecs, topClass, config, AwkwardCompiler) {
  private def awkwardLang = lang.asInstanceOf[AwkwardCompiler]

  override def compile: CompileLog.SpecSuccess = {
    awkwardLang.createBuilderMap(topClass, topClass)
    super.compile
  }
}
