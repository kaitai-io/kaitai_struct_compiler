package io.kaitai.struct

import io.kaitai.struct.CompileLog.FileSuccess
import io.kaitai.struct.datatype._
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components.{LanguageCompiler, LanguageCompilerStatic}

import scala.collection.mutable.ListBuffer

class ClassCompiler(
  val topClass: ClassSpec,
  config: RuntimeConfig,
  langObj: LanguageCompilerStatic
) extends AbstractCompiler {
  val provider = new ClassTypeProvider(topClass)
  val topClassName = topClass.name
  val lang: LanguageCompiler = langObj.getCompiler(provider, config)

  override def compile: CompileLog.SpecSuccess = {
    lang.fileHeader(topClassName.head)
    compileOpaqueClasses(topClass)
    compileClass(topClass)
    lang.fileFooter(topClassName.head)

    CompileLog.SpecSuccess(
      lang.type2class(topClassName.head),
      lang.results(topClass).map { case (fileName, contents) => FileSuccess(fileName, contents) }.toList
    )
  }

  def compileOpaqueClasses(topClass: ClassSpec) = {
    TypeProcessor.getOpaqueClasses(topClass).foreach((classSpec) =>
      if (classSpec != topClass)
        lang.opaqueClassDeclaration(classSpec)
    )
  }

  def compileClass(curClass: ClassSpec): Unit = {
    provider.nowClass = curClass

    if (!curClass.doc.isEmpty)
      lang.classDoc(curClass.name, curClass.doc)
    lang.classHeader(curClass.name)

    val extraAttrs = ListBuffer[AttrSpec]()
    extraAttrs += AttrSpec(List(), RootIdentifier, UserTypeInstream(topClassName, None))
    extraAttrs += AttrSpec(List(), ParentIdentifier, curClass.parentType)

    // Forward declarations for recursive types
    curClass.types.foreach { case (typeName, _) => lang.classForwardDeclaration(List(typeName)) }

    if (lang.innerEnums)
      compileEnums(curClass)

    if (lang.debug)
      lang.debugClassSequence(curClass.seq)

    lang.classConstructorHeader(curClass.name, curClass.parentType, topClassName)
    curClass.instances.foreach { case (instName, _) => lang.instanceClear(instName) }
    compileEagerReadCall(curClass.meta.endian)
    lang.classConstructorFooter

    compileEagerRead(curClass.seq, extraAttrs, curClass.meta.endian)

    compileDestructor(curClass)

    // Recursive types
    if (lang.innerClasses) {
      compileSubclasses(curClass)

      provider.nowClass = curClass
    }

    compileInstances(curClass, extraAttrs)

    // Attributes declarations and readers
    compileAttrDeclarations(curClass.seq ++ extraAttrs)

    (curClass.seq ++ extraAttrs).foreach { (attr) =>
      if (!attr.doc.isEmpty)
        lang.attributeDoc(attr.id, attr.doc)
      lang.attributeReader(attr.id, attr.dataTypeComposite, attr.cond)
    }

    lang.classFooter(curClass.name)

    if (!lang.innerClasses)
      compileSubclasses(curClass)

    if (!lang.innerEnums)
      compileEnums(curClass)
  }

  def compileDestructor(curClass: ClassSpec) = {
    lang.classDestructorHeader(curClass.name, curClass.parentType, topClassName)
    curClass.seq.foreach((attr) => lang.attrDestructor(attr, attr.id))
    curClass.instances.foreach { case (id, instSpec) =>
      instSpec match {
        case pis: ParseInstanceSpec => lang.attrDestructor(pis, id)
        case _: ValueInstanceSpec => // ignore for now
      }
    }
    lang.classDestructorFooter
  }

  def compileAttrDeclarations(attrs: List[AttrSpec]): Unit =
    attrs.foreach((attr) => lang.attributeDeclaration(attr.id, attr.dataTypeComposite, attr.cond))

  def compileEagerReadCall(endian: Option[Endianness]): Unit = {
    endian match {
      case None | Some(_: FixedEndian) =>
        lang.runRead()
      case Some(ce: CalcEndian) =>
        ce match {
          case CalcEndianOne(isLittle) =>
            lang.runReadCalcOne(isLittle)
          case CalcEndianTwo(isLittle, isBig) =>
            lang.runReadCalcTwo(isLittle, isBig)
        }
    }
  }

  def compileEagerRead(seq: List[AttrSpec], extraAttrs: ListBuffer[AttrSpec], endian: Option[Endianness]): Unit = {
    endian match {
      case None | Some(_: FixedEndian) =>
        compileSeqProc(seq, extraAttrs, None)
      case Some(_: CalcEndian) =>
        compileSeqProc(seq, extraAttrs, Some(LittleEndian))
        compileSeqProc(seq, extraAttrs, Some(BigEndian))
    }
  }

  def compileSeqProc(seq: List[AttrSpec], extraAttrs: ListBuffer[AttrSpec], defEndian: Option[FixedEndian]) = {
    lang.readHeader(defEndian)
    compileSeq(seq, extraAttrs, defEndian)
    lang.readFooter()
  }

  def compileSeq(seq: List[AttrSpec], extraAttrs: ListBuffer[AttrSpec], defEndian: Option[FixedEndian]) = {
    var wasUnaligned = false
    seq.foreach { (attr) =>
      val nowUnaligned = isUnalignedBits(attr.dataType)
      if (wasUnaligned && !nowUnaligned)
        lang.alignToByte(lang.normalIO)
      lang.attrParse(attr, attr.id, extraAttrs, defEndian)
      wasUnaligned = nowUnaligned
    }
  }

  def compileEnums(curClass: ClassSpec): Unit =
    curClass.enums.foreach { case(_, enumColl) => compileEnum(curClass, enumColl) }

  def compileSubclasses(curClass: ClassSpec): Unit =
    curClass.types.foreach { case (_, intClass) => compileClass(intClass) }

  def compileInstances(curClass: ClassSpec, extraAttrs: ListBuffer[AttrSpec]) = {
    val hybridEndian = curClass.meta.endian match {
      case Some(_: CalcEndian) => true
      case _ => false
    }
    curClass.instances.foreach { case (instName, instSpec) =>
      compileInstance(curClass.name, instName, instSpec, extraAttrs, hybridEndian)
    }
  }

  def compileInstance(className: List[String], instName: InstanceIdentifier, instSpec: InstanceSpec, extraAttrs: ListBuffer[AttrSpec], hybridEndian: Boolean): Unit = {
    // Determine datatype
    val dataType = instSpec.dataTypeComposite

    compileInstanceDeclaration(instName, instSpec)

    if (!instSpec.doc.isEmpty)
      lang.attributeDoc(instName, instSpec.doc)
    lang.instanceHeader(className, instName, dataType)
    lang.instanceCheckCacheAndReturn(instName)

    instSpec match {
      case vi: ValueInstanceSpec =>
        lang.attrParseIfHeader(instName, vi.ifExpr)
        lang.instanceCalculate(instName, dataType, vi.value)
        lang.attrParseIfFooter(vi.ifExpr)
      case i: ParseInstanceSpec =>
        lang.attrParse(i, instName, extraAttrs, None) // FIXME
    }

    lang.instanceSetCalculated(instName)
    lang.instanceReturn(instName)
    lang.instanceFooter
  }

  def compileInstanceDeclaration(instName: InstanceIdentifier, instSpec: InstanceSpec): Unit = {
    // Determine datatype
    val dataType = instSpec.dataTypeComposite

    // Declare caching variable
    val condSpec = instSpec match {
      case vis: ValueInstanceSpec => ConditionalSpec(vis.ifExpr, NoRepeat)
      case pis: ParseInstanceSpec => pis.cond
    }
    lang.instanceDeclaration(instName, dataType, condSpec)
  }

  def compileEnum(curClass: ClassSpec, enumColl: EnumSpec): Unit = {
    lang.enumDeclaration(curClass.name, enumColl.name.last, enumColl.sortedSeq)
  }

  def isUnalignedBits(dt: DataType): Boolean =
    dt match {
      case _: BitsType | BitsType1 => true
      case et: EnumType => isUnalignedBits(et.basedOn)
      case _ => false
    }
}
