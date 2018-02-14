package io.kaitai.struct

import io.kaitai.struct.CompileLog.FileSuccess
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components.{LanguageCompiler, LanguageCompilerStatic}

import scala.collection.mutable.ListBuffer

class ClassCompiler(
  classSpecs: ClassSpecs,
  val topClass: ClassSpec,
  config: RuntimeConfig,
  langObj: LanguageCompilerStatic
) extends AbstractCompiler {
  val provider = new ClassTypeProvider(classSpecs, topClass)
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

  /**
    * Generates code for one full class using a given [[ClassSpec]].
    * @param curClass current class to generate code for
    */
  def compileClass(curClass: ClassSpec): Unit = {
    provider.nowClass = curClass

    if (!lang.innerDocstrings)
      compileClassDoc(curClass)
    lang.classHeader(curClass.name)
    if (lang.innerDocstrings)
      compileClassDoc(curClass)

    val extraAttrs = ListBuffer[AttrSpec]()
    extraAttrs += AttrSpec(List(), RootIdentifier, UserTypeInstream(topClassName, None))
    extraAttrs += AttrSpec(List(), ParentIdentifier, curClass.parentType)

    // Forward declarations for recursive types
    curClass.types.foreach { case (typeName, _) => lang.classForwardDeclaration(List(typeName)) }

    if (lang.innerEnums)
      compileEnums(curClass)

    if (lang.debug)
      lang.debugClassSequence(curClass.seq)

    // Constructor
    compileConstructor(curClass)

    // Read method(s)
    compileEagerRead(curClass.seq, extraAttrs, curClass.meta.endian)

    // Destructor
    compileDestructor(curClass)

    // Recursive types
    if (lang.innerClasses) {
      compileSubclasses(curClass)

      provider.nowClass = curClass
    }

    compileInstances(curClass, extraAttrs)

    // Attributes declarations and readers
    val allAttrs: List[MemberSpec] = curClass.seq ++ curClass.params ++ extraAttrs
    compileAttrDeclarations(allAttrs)
    compileAttrReaders(allAttrs)

    lang.classFooter(curClass.name)

    if (!lang.innerClasses)
      compileSubclasses(curClass)

    if (!lang.innerEnums)
      compileEnums(curClass)
  }

  /**
    * Compiles constructor for a given class. Generally, it should:
    *
    * * store passed parameters, io/root/parent/endianness if needed
    * * initialize everything
    * * invoke _read() method, if applicable
    *
    * @param curClass current class to generate code for
    */
  def compileConstructor(curClass: ClassSpec) = {
    lang.classConstructorHeader(
      curClass.name,
      curClass.parentType,
      topClassName,
      curClass.meta.endian.contains(InheritedEndian),
      curClass.params
    )
    curClass.instances.foreach { case (instName, _) => lang.instanceClear(instName) }
    if (!lang.debug)
      lang.runRead()
    lang.classConstructorFooter
  }

  /**
    * Compiles destructor for a given class. It should clean up everything
    * (i.e. every applicable allocated seq / instance attribute variables, and
    * any extra attribute variables, if they were used).
    * @param curClass current class to generate code for
    */
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

  def compileAttrDeclarations(attrs: List[MemberSpec]): Unit = {
    attrs.foreach { (attr) =>
      val isNullable = if (lang.switchBytesOnlyAsRaw) {
        attr.isNullableSwitchRaw
      } else {
        attr.isNullable
      }
      lang.attributeDeclaration(attr.id, attr.dataTypeComposite, isNullable)
    }
  }

  /**
    * Iterates over a given list of attributes and generates attribute
    * readers (AKA getters) for each of them.
    * @param attrs attribute list to traverse
    */
  def compileAttrReaders(attrs: List[MemberSpec]): Unit =
    attrs.foreach { (attr) =>
      // FIXME: Python should have some form of attribute docs too
      if (!attr.doc.isEmpty && !lang.innerDocstrings)
        lang.attributeDoc(attr.id, attr.doc)
      val isNullable = if (lang.switchBytesOnlyAsRaw) {
        attr.isNullableSwitchRaw
      } else {
        attr.isNullable
      }
      lang.attributeReader(attr.id, attr.dataTypeComposite, isNullable)
    }

  def compileEagerRead(seq: List[AttrSpec], extraAttrs: ListBuffer[AttrSpec], endian: Option[Endianness]): Unit = {
    endian match {
      case None | Some(_: FixedEndian) =>
        compileSeqProc(seq, extraAttrs, None)
      case Some(ce: CalcEndian) =>
        lang.readHeader(None, false)
        compileCalcEndian(ce)
        lang.runReadCalc()
        lang.readFooter()

        compileSeqProc(seq, extraAttrs, Some(LittleEndian))
        compileSeqProc(seq, extraAttrs, Some(BigEndian))
      case Some(InheritedEndian) =>
        lang.readHeader(None, false)
        lang.runReadCalc()
        lang.readFooter()

        compileSeqProc(seq, extraAttrs, Some(LittleEndian))
        compileSeqProc(seq, extraAttrs, Some(BigEndian))
    }
  }

  val IS_LE_ID = SpecialIdentifier("_is_le")

  def compileCalcEndian(ce: CalcEndian): Unit = {
    def renderProc(result: FixedEndian): Unit = {
      val v = Ast.expr.Bool(result == LittleEndian)
      lang.instanceCalculate(IS_LE_ID, CalcBooleanType, v)
    }

    lang.switchCases[FixedEndian](IS_LE_ID, ce.on, ce.cases, renderProc, renderProc)
  }

  /**
    * Compiles seq reading method (complete with header and footer).
    * @param seq sequence of attributes
    * @param extraAttrs extra attributes to be allocated
    * @param defEndian default endianness
    */
  def compileSeqProc(seq: List[AttrSpec], extraAttrs: ListBuffer[AttrSpec], defEndian: Option[FixedEndian]) = {
    lang.readHeader(defEndian, seq.isEmpty)
    compileSeq(seq, extraAttrs, defEndian)
    lang.readFooter()
  }

  /**
    * Compiles seq reading method body (only reading statements).
    * @param seq sequence of attributes
    * @param extraAttrs extra attributes to be allocated
    * @param defEndian default endianness
    */
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

  /**
    * Compile subclasses for a given class.
    * @param curClass current class to generate code for
    */
  def compileSubclasses(curClass: ClassSpec): Unit =
    curClass.types.foreach { case (_, intClass) => compileClass(intClass) }

  def compileInstances(curClass: ClassSpec, extraAttrs: ListBuffer[AttrSpec]) = {
    curClass.instances.foreach { case (instName, instSpec) =>
      compileInstance(curClass.name, instName, instSpec, extraAttrs, curClass.meta.endian)
    }
  }

  def compileInstance(className: List[String], instName: InstanceIdentifier, instSpec: InstanceSpec, extraAttrs: ListBuffer[AttrSpec], endian: Option[Endianness]): Unit = {
    // Determine datatype
    val dataType = instSpec.dataTypeComposite

    compileInstanceDeclaration(instName, instSpec)

    if (!lang.innerDocstrings)
      compileInstanceDoc(instName, instSpec)
    lang.instanceHeader(className, instName, dataType, instSpec.isNullable)
    if (lang.innerDocstrings)
      compileInstanceDoc(instName, instSpec)
    lang.instanceCheckCacheAndReturn(instName)

    instSpec match {
      case vi: ValueInstanceSpec =>
        lang.attrParseIfHeader(instName, vi.ifExpr)
        lang.instanceCalculate(instName, dataType, vi.value)
        lang.attrParseIfFooter(vi.ifExpr)
      case i: ParseInstanceSpec =>
        lang.attrParse(i, instName, extraAttrs, endian)
    }

    lang.instanceSetCalculated(instName)
    lang.instanceReturn(instName)
    lang.instanceFooter
  }

  def compileInstanceDeclaration(instName: InstanceIdentifier, instSpec: InstanceSpec): Unit =
    lang.instanceDeclaration(instName, instSpec.dataTypeComposite, instSpec.isNullable)

  def compileEnum(curClass: ClassSpec, enumColl: EnumSpec): Unit =
    lang.enumDeclaration(curClass.name, enumColl.name.last, enumColl.sortedSeq)

  def isUnalignedBits(dt: DataType): Boolean =
    dt match {
      case _: BitsType | BitsType1 => true
      case et: EnumType => isUnalignedBits(et.basedOn)
      case _ => false
    }

  def compileClassDoc(curClass: ClassSpec) = {
    if (!curClass.doc.isEmpty)
      lang.classDoc(curClass.name, curClass.doc)
  }

  def compileInstanceDoc(instName: Identifier, instSpec: InstanceSpec) {
    if (!instSpec.doc.isEmpty)
      lang.attributeDoc(instName, instSpec.doc)
  }
}
