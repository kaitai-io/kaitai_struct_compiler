package io.kaitai.struct

import io.kaitai.struct.CompileLog.FileSuccess
import io.kaitai.struct.datatype.DataType.UserTypeInstream
import io.kaitai.struct.datatype.{InheritedEndian, FixedEndian, LittleEndian}
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
    compileTypes(topClass)
    lang.classFooter(null)
    compileProcs(topClass)

    CompileLog.SpecSuccess(
      lang.type2class(topClassName.head),
      lang.results(topClass).map { case (fileName, contents) => FileSuccess(fileName, contents) }.toList
    )
  }

  def compileTypes(curClass: ClassSpec): Unit = {
    lang.classHeader(curClass.name)

    val extraAttrs = List(
      AttrSpec(List(), RootIdentifier, UserTypeInstream(topClassName, None)),
      AttrSpec(List(), ParentIdentifier, curClass.parentType)
    ) ++ ExtraAttrs.forClassSpec(curClass, lang)

    compileAttrDeclarations(curClass.seq ++ extraAttrs)
    compileInstanceDeclarations(curClass)
    lang.classFooter(curClass.name)
    compileSubtypes(curClass)
  }

  def compileSubtypes(curClass: ClassSpec): Unit = {
    curClass.types.foreach { case (_, subClass) => compileTypes(subClass) }
  }

  def compileInstanceDeclarations(curClass: ClassSpec) = {
    curClass.instances.foreach { case (instName, instSpec) =>
      lang.instanceDeclaration(instName, instSpec.dataTypeComposite, instSpec.isNullable)
    }
  }

  def compileProcs(curClass: ClassSpec): Unit = {
    compileSubprocs(curClass)

    lang.classConstructorHeader(
      curClass.name,
      curClass.parentType,
      topClassName,
      curClass.meta.endian.contains(InheritedEndian),
      curClass.params
    )

    compileReads(curClass)

    lang.classConstructorFooter

    lang.asInstanceOf[NimCompiler].fromFileProc(curClass.name)
  }

  def compileSubprocs(curClass: ClassSpec): Unit = {
    curClass.types.foreach { case (_, subClass) => compileProcs(subClass) }
  }

  def compileReads(curClass: ClassSpec): Unit = {
    val defEndian = curClass.meta.endian match {
      case Some(fe: FixedEndian) => Some(fe)
      case _ => Some(LittleEndian)
    }
    curClass.seq.foreach { (attr) => lang.asInstanceOf[NimCompiler].readType(attr.id, attr.dataType, attr.isArray, defEndian) }
    lang.asInstanceOf[NimCompiler].savePosition()
    curClass.instances.foreach { case (id, pis: ParseInstanceSpec) =>
      lang.asInstanceOf[NimCompiler].readInstance(id, pis.dataType, pis.pos.get, defEndian)
    }
  }
}
