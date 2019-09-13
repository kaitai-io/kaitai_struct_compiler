package io.kaitai.struct

import io.kaitai.struct.CompileLog.FileSuccess
import io.kaitai.struct.datatype.DataType.UserTypeInstream
import io.kaitai.struct.datatype.{InheritedEndian, FixedEndian}
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components.ExtraAttrs
import io.kaitai.struct.languages.NimCompiler

class NimClassCompiler(
  classSpecs: ClassSpecs,
  override val topClass: ClassSpec,
  config: RuntimeConfig
) extends ClassCompiler(classSpecs, topClass, config, NimCompiler) {
  override def compile: CompileLog.SpecSuccess = {
    lang.fileHeader(topClassName.head)
    compileType(topClass)
    compileProcs(topClass)

    CompileLog.SpecSuccess(
      lang.type2class(topClassName.head),
      lang.results(topClass).map { case (fileName, contents) => FileSuccess(fileName, contents) }.toList
    )
  }

  def compileType(curClass: ClassSpec): Unit = {
    lang.classHeader(curClass.name)

    val extraAttrs = List(
      AttrSpec(List(), RootIdentifier, UserTypeInstream(topClassName, None)),
      AttrSpec(List(), ParentIdentifier, curClass.parentType)
    ) ++ ExtraAttrs.forClassSpec(curClass, lang)

    compileAttrDeclarations(curClass.seq ++ extraAttrs)
    lang.classFooter(curClass.name)
    compileSubtypes(curClass)
  }

  def compileSubtypes(curClass: ClassSpec): Unit = {
    curClass.types.foreach { case (_, subClass) => compileType(subClass) }
  }

  def compileProcs(curClass: ClassSpec): Unit = {
    lang.classConstructorHeader(
      curClass.name,
      curClass.parentType,
      topClassName,
      curClass.meta.endian.contains(InheritedEndian),
      curClass.params
    )

    // FIXME
    val defEndian = curClass.meta.endian match {
      case Some(fe: FixedEndian) => Some(fe)
      case _ => None
    }

    compileReads(curClass)

    lang.classConstructorFooter

    lang.asInstanceOf[NimCompiler].fromFileProc(curClass.name)
    compileSubprocs(curClass)
  }

  def compileSubprocs(curClass: ClassSpec): Unit = {
    curClass.types.foreach { case (_, subClass) => compileSubprocs(subClass) }
  }

  def compileReads(curClass: ClassSpec): Unit = {
    val defEndian = curClass.meta.endian match {
      case Some(fe: FixedEndian) => Some(fe)
      case _ => None
    }
    curClass.seq.foreach { (attr) => lang.asInstanceOf[NimCompiler].readInstance(attr.id, attr.dataType, defEndian) }
  }
}
