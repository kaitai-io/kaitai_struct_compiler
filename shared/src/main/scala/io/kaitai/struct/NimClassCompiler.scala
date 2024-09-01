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
    compileExternalTypes(topClass)

    // if there are any enums at all maybe we can detect it and not generate this template
//    nimlang.enumTemplate
//    nimlang.enumTemplateFooter

//    compileEnumsForAllTypes(topClass)
//    compileEnumConstants(topClass)

    nimlang.typeSectionHeader
    compileTypes(topClass)
    nimlang.typeSectionFooter

    compileReadsForward(topClass)
    nimlang.blankLine

    compileInstancesForward(topClass)
    nimlang.blankLine

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
      if (!attr.doc.isEmpty)
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

  override def compileInstance(className: List[String], instName: InstanceIdentifier, instSpec: InstanceSpec, endian: Option[Endianness]): Unit = {
    // Determine datatype
    val dataType = instSpec.dataTypeComposite

    lang.instanceHeader(className, instName, dataType, instSpec.isNullable)
    compileInstanceDoc(instName, instSpec)
    lang.instanceCheckCacheAndReturn(instName, dataType)

    lang.instanceSetCalculated(instName)
    instSpec match {
      case vi: ValueInstanceSpec =>
        lang.attrParseIfHeader(instName, vi.ifExpr)
        lang.instanceCalculate(instName, dataType, vi.value)
        lang.attrParseIfFooter(vi.ifExpr)
      case pi: ParseInstanceSpec =>
        lang.attrParse(pi, instName, endian)
    }

    lang.instanceReturn(instName, dataType)
    lang.instanceFooter
  }

//  def compileEnumsForAllTypes(curClass: ClassSpec) {
//    provider.nowClass = curClass
//    compileEnums(curClass)
//    compileEnumsForAllTypesRec(curClass)
//  }
//
//  def compileEnumsForAllTypesRec(curClass: ClassSpec) {
//    curClass.types.foreach { case (_, subClass) => compileEnumsForAllTypes(subClass) }
//  }

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

//  def compileEnumConstants(curClass: ClassSpec): Unit = {
//    provider.nowClass = curClass
//    curClass.enums.foreach { case(_, enumColl) => {
//      nimlang.enumHeader
//      nimlang.enumConstants(curClass.name, enumColl.name.last, enumColl.sortedSeq) }
//      nimlang.enumFooter
//    }
//    compileEnumConstantsRec(curClass)
//  }
//
//  def compileEnumConstantsRec(curClass: ClassSpec): Unit = {
//    curClass.types.foreach { case (_, subClass) => compileEnumConstants(subClass) }
//  }

  def compileReadsForward(curClass: ClassSpec): Unit = {
    provider.nowClass = curClass
    nimlang.classForwardDeclaration(curClass.name)
    compileReadsForwardRec(curClass)
  }

  def compileReadsForwardRec(curClass: ClassSpec): Unit = {
    curClass.types.foreach { case (_, subClass) => compileReadsForward(subClass) }
  }

  def compileInstancesForward(curClass: ClassSpec): Unit = {
    provider.nowClass = curClass
    curClass.instances.foreach { case (instName, instSpec) =>
      nimlang.instanceForwardDeclaration(curClass.name, instName, instSpec.dataTypeComposite) }
    compileInstancesForwardRec(curClass)
  }

  def compileInstancesForwardRec(curClass: ClassSpec): Unit = {
    curClass.types.foreach { case (_, subClass) => compileInstancesForward(subClass) }
  }

  def compileProcs(curClass: ClassSpec): Unit = {
    provider.nowClass = curClass
    compileClassDoc(curClass)
    compileEagerRead(curClass.seq, curClass.meta.endian)
    compileInstances(curClass)
    nimlang.fromFile(curClass.name)
    compileProcsRec(curClass)
  }

  def compileProcsRec(curClass: ClassSpec): Unit = {
    curClass.types.foreach { case (_, subClass) => compileProcs(subClass) }
  }

}
