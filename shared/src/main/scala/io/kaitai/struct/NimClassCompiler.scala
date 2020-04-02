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
    compileOpaqueClasses(topClass)
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
        compileCalcEndian(ce)
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

  // Must override just to add attribute docstrings
  override def compileSeq(seq: List[AttrSpec], defEndian: Option[FixedEndian]) = {
    var wasUnaligned = false
    seq.foreach { (attr) =>
      val nowUnaligned = isUnalignedBits(attr.dataType)
      if (wasUnaligned && !nowUnaligned)
        lang.alignToByte(lang.normalIO)
      lang.attributeDoc(attr.id, attr.doc)
      lang.attrParse(attr, attr.id, defEndian)
      wasUnaligned = nowUnaligned
    }
  }

  override def compileInstances(curClass: ClassSpec) = {
    curClass.instances.foreach { case (instName, instSpec) =>
      compileInstance(curClass.name, instName, instSpec, curClass.meta.endian)
    }
  }

  def compileInstancesForwardDeclaration(curClass: ClassSpec) = {
    curClass.instances.foreach { case (instName, instSpec) =>
      nimlang.instanceForwardDeclaration(curClass.name, instName, instSpec.dataTypeComposite)
    }
  }

  override def compileInstance(className: List[String], instName: InstanceIdentifier, instSpec: InstanceSpec, endian: Option[Endianness]): Unit = {
    // Determine datatype
    val dataType = instSpec.dataTypeComposite

    lang.instanceHeader(className, instName, dataType, instSpec.isNullable)
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
    provider.nowClass = curClass
    nimlang.classHeader(curClass.name)

    val allAttrs: List[MemberSpec] =
      curClass.seq ++
      curClass.params ++
      List(AttrSpec(List(), ParentIdentifier, curClass.parentType)) ++
      ExtraAttrs.forClassSpec(curClass, lang)
    compileAttrDeclarations(allAttrs)

    curClass.instances.foreach { case (instName, instSpec) =>
      compileInstanceDeclaration(instName, instSpec)
    }

    nimlang.classFooter(curClass.name)

    compileEnums(curClass)
    compileTypesRec(curClass)
  }

  def compileTypesRec(curClass: ClassSpec): Unit = {
    curClass.types.foreach { case (_, subClass) => compileTypes(subClass) }
  }

  def compileProcs(curClass: ClassSpec): Unit = {
    compileProcsRec(curClass)
    provider.nowClass = curClass
    compileClassDoc(curClass)
    compileInstancesForwardDeclaration(curClass)
    compileEagerRead(curClass.seq, curClass.meta.endian)
    compileInstances(curClass)
    nimlang.fromFile(curClass.name)
  }

  def compileProcsRec(curClass: ClassSpec): Unit = {
    curClass.types.foreach { case (_, subClass) => compileProcs(subClass) }
  }

}

