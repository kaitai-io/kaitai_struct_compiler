package io.kaitai.struct

import io.kaitai.struct.CompileLog.FileSuccess
import io.kaitai.struct.datatype.DataType
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
    extraAttrs += AttrSpec(List(), ParentIdentifier, UserTypeInstream(curClass.parentTypeName, None))

    // Forward declarations for recursive types
    curClass.types.foreach { case (typeName, _) => lang.classForwardDeclaration(List(typeName)) }

    if (lang.innerEnums)
      compileEnums(curClass)

    if (lang.debug)
      lang.debugClassSequence(curClass.seq)

    lang.classConstructorHeader(curClass.name, curClass.parentTypeName, topClassName)
    curClass.instances.foreach { case (instName, _) => lang.instanceClear(instName) }
    compileSeq(curClass.seq, extraAttrs)
    lang.classConstructorFooter

    lang.classDestructorHeader(curClass.name, curClass.parentTypeName, topClassName)
    curClass.seq.foreach((attr) => lang.attrDestructor(attr, attr.id))
    curClass.instances.foreach { case (id, instSpec) =>
      instSpec match {
        case pis: ParseInstanceSpec => lang.attrDestructor(pis, id)
        case _: ValueInstanceSpec => // ignore for now
      }
    }
    lang.classDestructorFooter

    // Recursive types
    if (lang.innerClasses) {
      compileSubclasses(curClass)

      provider.nowClass = curClass
    }

    curClass.instances.foreach { case (instName, instSpec) => compileInstance(curClass.name, instName, instSpec, extraAttrs) }

    // Attributes declarations and readers
    (curClass.seq ++ extraAttrs).foreach((attr) => lang.attributeDeclaration(attr.id, attr.dataTypeComposite, attr.cond))
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

  def compileSeq(seq: List[AttrSpec], extraAttrs: ListBuffer[AttrSpec]) = {
    var wasUnaligned = false
    seq.foreach { (attr) =>
      val nowUnaligned = isUnalignedBits(attr.dataType)
      if (wasUnaligned && !nowUnaligned)
        lang.alignToByte(lang.normalIO)
      lang.attrParse(attr, attr.id, extraAttrs)
      wasUnaligned = nowUnaligned
    }
  }

  def compileEnums(curClass: ClassSpec): Unit =
    curClass.enums.foreach { case(_, enumColl) => compileEnum(curClass, enumColl) }

  def compileSubclasses(curClass: ClassSpec): Unit =
    curClass.types.foreach { case (_, intClass) => compileClass(intClass) }

  def compileInstance(className: List[String], instName: InstanceIdentifier, instSpec: InstanceSpec, extraAttrs: ListBuffer[AttrSpec]): Unit = {
    // Determine datatype
    val dataType = instSpec.dataTypeComposite

    // Declare caching variable
    val condSpec = instSpec match {
      case vis: ValueInstanceSpec => ConditionalSpec(vis.ifExpr, NoRepeat)
      case pis: ParseInstanceSpec => pis.cond
    }
    lang.instanceDeclaration(instName, dataType, condSpec)

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
        lang.attrParse(i, instName, extraAttrs)
    }

    lang.instanceSetCalculated(instName)
    lang.instanceReturn(instName)
    lang.instanceFooter
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
