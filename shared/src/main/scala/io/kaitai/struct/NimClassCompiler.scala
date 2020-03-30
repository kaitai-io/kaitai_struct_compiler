package io.kaitai.struct

import io.kaitai.struct.CompileLog.FileSuccess
import io.kaitai.struct.datatype.DataType.{KaitaiStreamType, UserTypeInstream, CalcUserType}
import io.kaitai.struct.datatype.{Endianness, FixedEndian, InheritedEndian, LittleEndian, BigEndian, CalcEndian}
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

  override def compileEagerRead(seq: List[AttrSpec], endian: Option[Endianness]): Unit = {
    endian match {
      case None | Some(_: FixedEndian) =>
        compileSeqProc(seq, None)
      case Some(ce: CalcEndian) =>
        compileSeqProc(seq, Some(LittleEndian))
        compileSeqProc(seq, Some(BigEndian))
        lang.readHeader(None, false)
        //compileCalcEndian(ce)
        lang.runReadCalc()
        lang.readFooter()
      case Some(InheritedEndian) =>
        compileSeqProc(seq, Some(LittleEndian))
        compileSeqProc(seq, Some(BigEndian))
        lang.readHeader(None, false)
        lang.runReadCalc()
        lang.readFooter()
    }
  }

  override def compileInstances(curClass: ClassSpec) = {
    curClass.instances.foreach { case (instName, instSpec) =>
      nimlang.instanceForwardDeclaration(curClass.name, instName, instSpec.dataTypeComposite)
    }
    curClass.instances.foreach { case (instName, instSpec) =>
      compileInstance(curClass.name, instName, instSpec, curClass.meta.endian)
    }
  }

  override def compileInstance(className: List[String], instName: InstanceIdentifier, instSpec: InstanceSpec, endian: Option[Endianness]): Unit = {
    // Determine datatype
    val dataType = instSpec.dataTypeComposite

    if (!lang.innerDocstrings)
      compileInstanceDoc(instName, instSpec)
    lang.instanceHeader(className, instName, dataType, instSpec.isNullable)
    if (lang.innerDocstrings)
      compileInstanceDoc(instName, instSpec)
    lang.instanceCheckCacheAndReturn(instName, dataType)

    instSpec match {
      case vi: ValueInstanceSpec =>
        lang.attrParseIfHeader(instName, vi.ifExpr)
        lang.instanceCalculate(instName, dataType, vi.value)
        lang.attrParseIfFooter(vi.ifExpr)
        lang.instanceSetCalculated(instName)
      case pi: ParseInstanceSpec =>
        lang.attrParse(pi, instName, endian)
    }

    lang.instanceReturn(instName, dataType)
    lang.instanceFooter
  }

  def compileTypes(curClass: ClassSpec): Unit = {
    compileTypesRec(curClass)
    provider.nowClass = curClass
    nimlang.classHeader(curClass.name)

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

    curClass.instances.foreach { case (instName, instSpec) =>
      compileInstanceDeclaration(instName, instSpec)
    }

    nimlang.classFooter(curClass.name)

    compileEnums(curClass)
  }

  def compileTypesRec(curClass: ClassSpec): Unit = {
    curClass.types.foreach { case (_, subClass) => compileTypes(subClass) }
  }

  def compileProcs(curClass: ClassSpec): Unit = {
    compileProcsRec(curClass)
    provider.nowClass = curClass
    nimlang.sectionHeader(curClass.name)

    // instances' procs
    compileInstances(curClass)

    // read, readLe, readBe
    compileEagerRead(curClass.seq, curClass.meta.endian)

    // proc fromFile
    nimlang.fromFile(curClass.name)

    // proc `=destroy`
    nimlang.destructor(curClass.name)
  }

  def compileProcsRec(curClass: ClassSpec): Unit = {
    curClass.types.foreach { case (_, subClass) => compileProcs(subClass) }
  }

}

