package io.kaitai.struct

import io.kaitai.struct.CompileLog.FileSuccess
import io.kaitai.struct.datatype.DataType.{KaitaiStreamType, UserTypeInstream, CalcUserType}
import io.kaitai.struct.datatype.{Endianness, FixedEndian, InheritedEndian}
import io.kaitai.struct.format._
import io.kaitai.struct.languages.NimCompiler
import io.kaitai.struct.languages.components.ExtraAttrs

import scala.collection.mutable.ListBuffer

class NimClassCompiler(
  classSpecs: ClassSpecs,
  override val topClass: ClassSpec,
  config: RuntimeConfig
) extends ClassCompiler(classSpecs, topClass, config, NimCompiler) {

  val nimlang = lang.asInstanceOf[NimCompiler]

  def classNameFlattened(theClass: ClassSpec): String = nimlang.namespaced(theClass.name)

  override def compile: CompileLog.SpecSuccess = {
    lang.fileHeader(classNameFlattened(topClass))
    nimlang.typeSectionHeader
    compileTypes(topClass)
    nimlang.typeSectionFooter
    compileProcs(topClass)
    CompileLog.SpecSuccess(
      classNameFlattened(topClass),
      lang.results(topClass).map { case (fileName, contents) => FileSuccess(fileName, contents) }.toList
    )
  }

  def compileTypes(curClass: ClassSpec): Unit = {
    compileTypesRec(curClass)
    nimlang.typeHeader(classNameFlattened(curClass))

    val allAttrs: List[MemberSpec] =
      curClass.seq ++
      curClass.params ++
      List(
        AttrSpec(List(), IoIdentifier, KaitaiStreamType),
        AttrSpec(List(), RootIdentifier, CalcUserType(topClassName, None)),
        AttrSpec(List(), ParentIdentifier, curClass.parentType)
      ) ++
      ExtraAttrs.forClassSpec(curClass, lang)
    compileAttrDeclarations(allAttrs)

    nimlang.universalFooter
  }

  def compileTypesRec(curClass: ClassSpec): Unit = {
    curClass.types.foreach { case (_, subClass) => compileTypes(subClass) }
  }

  def compileProcs(curClass: ClassSpec): Unit = {
    compileProcsRec(curClass)
    nimlang.sectionHeader(curClass.name)

    // proc read
    lang.classConstructorHeader(
      curClass.name,
      curClass.parentType,
      topClassName,
      curClass.meta.endian.contains(InheritedEndian),
      curClass.params
    )
    compileEagerRead(curClass.seq, curClass.meta.endian)
    nimlang.readProcFooter

    // proc fromFile
    nimlang.fromFile(curClass.name)

    // proc `=destroy`
    nimlang.destructor(curClass.name)
  }

  def compileProcsRec(curClass: ClassSpec): Unit = {
    curClass.types.foreach { case (_, subClass) => compileProcs(subClass) }
  }

}

