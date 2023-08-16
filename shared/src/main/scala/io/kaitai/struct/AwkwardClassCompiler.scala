package io.kaitai.struct

import io.kaitai.struct.CompileLog.FileSuccess
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{AttrSpec, _}
import io.kaitai.struct.languages.AwkwardCompiler
import io.kaitai.struct.languages.components.{LanguageCompiler, LanguageCompilerStatic}

class AwkwardClassCompiler(
  classSpecs: ClassSpecs,
  override val topClass: ClassSpec,
  config: RuntimeConfig
) extends ClassCompiler(classSpecs, topClass, config, AwkwardCompiler) {

  val awk = new AwkwardCompiler(provider, config)

  override def compile: CompileLog.SpecSuccess = {
    lang.fileHeader(topClassName.head)
    compileOpaqueClasses(topClass)
    compileClass(topClass)
    awk.builderStructure(topClass)
    lang.fileFooter(topClassName.head)

    CompileLog.SpecSuccess(
      lang.type2class(topClassName.head),
      lang.results(topClass).map { case (fileName, contents) => FileSuccess(fileName, contents) }.toList
    )
  }

  override def compileConstructor(curClass: ClassSpec) = {
    lang.classConstructorHeader(
      curClass.name,
      curClass.parentType,
      topClassName,
      curClass.meta.endian.contains(InheritedEndian),
      curClass.params
    )
    awk.createBuilderMap(curClass)
    compileInit(curClass)
    curClass.instances.foreach { case (instName, _) => lang.instanceClear(instName) }
    if (lang.config.autoRead)
      lang.runRead(curClass.name)
    lang.classConstructorFooter
  }
}