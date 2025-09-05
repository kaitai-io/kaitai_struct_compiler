package io.kaitai.struct

import io.kaitai.struct.CompileLog.FileSuccess
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{AttrSpec, _}
import io.kaitai.struct.languages.components.{ExtraAttrs, LanguageCompiler, LanguageCompilerStatic}

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
    compileExternalTypes(topClass)
    compileClass(topClass)
    lang.fileFooter(topClassName.head)

    CompileLog.SpecSuccess(
      lang.type2class(topClassName.head),
      lang.results(topClass).map { case (fileName, contents) => FileSuccess(fileName, contents) }.toList
    )
  }

  def compileExternalTypes(topClass: ClassSpec) = {
    TypeProcessor.getExternalTypes(topClass).foreach((extType) =>
      lang.externalTypeDeclaration(extType)
    )
  }

  /**
    * Generates code for one full class using a given [[format.ClassSpec]].
    * @param curClass current class to generate code for
    */
  def compileClass(curClass: ClassSpec): Unit = {
    provider.nowClass = curClass

    if (!lang.innerDocstrings)
      compileClassDoc(curClass)
    lang.classHeader(curClass.name)
    if (lang.innerDocstrings)
      compileClassDoc(curClass)

    // Forward declarations for recursive types
    curClass.types.foreach { case (typeName, _) => lang.classForwardDeclaration(List(typeName)) }

    if (lang.innerEnums)
      compileEnums(curClass)

    if (lang.config.readStoresPos)
      lang.debugClassSequence(curClass.seq)

    // Constructor
    compileConstructor(curClass)

    // Read method(s)
    compileEagerRead(curClass.seq, curClass.meta.endian)

    compileFetchInstancesProc(curClass.seq ++ curClass.instances.values.collect {
      case inst: AttrLikeSpec => inst
    })

    if (config.readWrite) {
      compileWrite(curClass.seq, curClass.instances, curClass.meta.endian)
      compileCheck(curClass.seq, curClass.instances)
    }

    // Destructor
    compileDestructor(curClass)

    // Recursive types
    if (lang.innerClasses) {
      compileSubclasses(curClass)

      provider.nowClass = curClass
    }

    compileInstances(curClass)

    // Attributes declarations and readers
    val allAttrs: List[MemberSpec] =
      curClass.seq ++
      curClass.params ++
      List(
        AttrSpec(List(), RootIdentifier, CalcUserType(topClassName, None)),
        AttrSpec(List(), ParentIdentifier, curClass.parentType)
      ) ++
      ExtraAttrs.forClassSpec(curClass, lang)
    compileAttrDeclarations(allAttrs)
    compileAttrReaders(allAttrs)

    curClass.toStringExpr.foreach(expr => lang.classToString(expr))

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
    compileInit(curClass)
    curClass.instances.foreach { case (instName, _) => lang.instanceClear(instName) }
    if (config.readWrite) {
      curClass.instances.foreach { case (instName, instSpec) =>
        instSpec match {
          case _: ParseInstanceSpec =>
            lang.instanceWriteFlagInit(instName)
          case _: ValueInstanceSpec => // do nothing
        }
      }
    }
    if (lang.config.autoRead)
      lang.runRead(curClass.name)
    lang.classConstructorFooter
  }

  /**
    * Compile initialization of class members for a given type. Typically
    * this is only required for languages which both:
    *
    * * don't perform auto-initialization of object with some default
    *   values (like 0s) on object creation,
    * * require these members to be initialized because any other
    *   procedures with object (e.g. destruction) will require that
    *
    * Currently, this is only applicable to C++ without smart pointers,
    * as destructors we'll generate will rely on pointers being set to
    * null.
    * @param curClass current type to generate code for
    */
  def compileInit(curClass: ClassSpec) = {
    curClass.seq.foreach((attr) => compileAttrInit(attr))
    curClass.instances.foreach { case (_, instSpec) =>
      instSpec match {
        case pis: ParseInstanceSpec => compileAttrInit(pis)
        case _: ValueInstanceSpec => // ignore for now
      }
    }
  }

  def compileAttrInit(originalAttr: AttrLikeSpec): Unit = {
    val extraAttrs = ExtraAttrs.forAttr(originalAttr, lang)
    val allAttrs = List(originalAttr) ++ extraAttrs
    allAttrs.foreach((attr) => lang.attrInit(attr))
  }

  /**
    * Compiles destructor for a given type. It should clean up everything
    * (i.e. every applicable allocated seq / instance attribute variables, and
    * any extra attribute variables, if they were used).
    * @param curClass current type to generate code for
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

  /**
    * Iterates over a given list of attributes and generates attribute
    * declarations for each of them.
    * @param attrs attribute list to traverse
    */
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
  def compileAttrReaders(attrs: List[MemberSpec]): Unit = {
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
      if (config.readWrite) {
        lang.attributeSetter(attr.id, attr.dataTypeComposite, isNullable)
      }
    }
  }

  /**
    * Compiles everything related to "eager reading" for a given list of
    * sequence attributes and endianness. Depending on endianness:
    *
    * * For types known to have fixed endianness, we do just "_read" method.
    * * For types with ambiguous endianness, we'll do `_read` + "_read_le" +
    *   "_read_be" methods. If endianness needs to be calculated, we'll perform
    *   that calculation in "_read". If it's inherited, then we'll just make
    *   decision based on that inherited setting.
    *
    * @param seq list of sequence attributes
    * @param endian endianness setting
    */
  def compileEagerRead(seq: List[AttrSpec], endian: Option[Endianness]): Unit = {
    endian match {
      case None | Some(_: FixedEndian) =>
        compileSeqReadProc(seq, None)
      case Some(ce: CalcEndian) =>
        lang.readHeader(None, false)
        compileCalcEndian(ce)
        lang.runReadCalc()
        lang.readFooter()

        compileSeqReadProc(seq, Some(LittleEndian))
        compileSeqReadProc(seq, Some(BigEndian))
      case Some(InheritedEndian) =>
        lang.readHeader(None, false)
        lang.runReadCalc()
        lang.readFooter()

        compileSeqReadProc(seq, Some(LittleEndian))
        compileSeqReadProc(seq, Some(BigEndian))
    }
  }

  def compileWrite(seq: List[AttrSpec], instances: Map[InstanceIdentifier, InstanceSpec], endian: Option[Endianness]): Unit = {
    endian match {
      case None | Some(_: FixedEndian) =>
        compileSeqWriteProc(seq, instances, None)
      case Some(CalcEndian(_, _)) | Some(InheritedEndian) =>
        lang.writeHeader(None, false)
        lang.runWriteCalc()
        lang.writeFooter()

        compileSeqWriteProc(seq, instances, Some(LittleEndian))
        compileSeqWriteProc(seq, instances, Some(BigEndian))
    }
  }

  def compileCheck(seq: List[AttrSpec], instances: Map[InstanceIdentifier, InstanceSpec]): Unit = {
    lang.checkHeader()
    compileSeqCheck(seq)
    compileInstCheck(instances)
    lang.checkFooter()
  }

  val IS_LE_ID = SpecialIdentifier("_is_le")

  /**
    * Compiles endianness calculation procedure and stores result in a special
    * attribute [[IS_LE_ID]]. Typically occurs as part of "_read" method.
    * @param ce calculated endianness specification
    */
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
    * @param defEndian default endianness
    */
  def compileSeqReadProc(seq: List[AttrSpec], defEndian: Option[FixedEndian]) = {
    lang.readHeader(defEndian, seq.isEmpty)
    compileSeqRead(seq, defEndian)
    lang.readFooter()
  }

  def compileFetchInstancesProc(attrs: List[AttrLikeSpec]) = {
    lang.fetchInstancesHeader()
    compileFetchInstances(attrs)
    lang.fetchInstancesFooter()
  }

  def compileSeqWriteProc(seq: List[AttrSpec], instances: Map[InstanceIdentifier, InstanceSpec], defEndian: Option[FixedEndian]) = {
    lang.writeHeader(defEndian, !instances.values.exists(i => i.isInstanceOf[ParseInstanceSpec]) && seq.isEmpty)
    compileSetInstanceWriteFlags(instances)
    compileSeqWrite(seq, defEndian)
    lang.writeFooter()
  }

  /**
    * Compiles seq reading method body (only reading statements).
    * @param seq sequence of attributes
    * @param defEndian default endianness
    */
  def compileSeqRead(seq: List[AttrSpec], defEndian: Option[FixedEndian]) = {
    var wasUnaligned = false
    seq.foreach { (attr) =>
      val nowUnaligned = isUnalignedBits(attr.dataType)
      if (wasUnaligned && !nowUnaligned)
        lang.alignToByte(lang.normalIO)
      lang.attrParse(attr, attr.id, defEndian)
      wasUnaligned = nowUnaligned
    }
  }

  def compileFetchInstances(attrs: List[AttrLikeSpec]): Unit = {
    attrs.foreach { (attr) =>
      lang.attrFetchInstances(attr, attr.id)
    }
  }

  def compileSetInstanceWriteFlags(instances: Map[InstanceIdentifier, InstanceSpec]) = {
    instances.foreach { case (instName, instSpec) =>
      instSpec match {
        case _: ParseInstanceSpec =>
          lang.instanceSetWriteFlag(instName)
        case _: ValueInstanceSpec => // do nothing
      }
    }
  }

  def compileSeqWrite(seq: List[AttrSpec], defEndian: Option[FixedEndian]) = {
    seq.foreach { (attr) =>
      lang.attrWrite(attr, attr.id, defEndian)
    }
  }

  def compileSeqCheck(seq: List[AttrSpec]) = {
    seq.foreach { (attr) =>
      lang.attrCheck(attr, attr.id)
    }
  }

  def compileInstCheck(instances: Map[InstanceIdentifier, InstanceSpec]) = {
    instances.foreach { case (instName, instSpec) =>
      instSpec match {
        case pi: ParseInstanceSpec =>
          lang.checkInstanceHeader(instName)
          lang.attrCheck(pi, instName)
          lang.checkInstanceFooter
        case _: ValueInstanceSpec => // do nothing
      }
    }
  }

  /**
    * Compiles all enums specifications for a given type.
    * @param curClass current type to generate code for
    */
  def compileEnums(curClass: ClassSpec): Unit =
    curClass.enums.foreach { case(_, enumColl) => compileEnum(curClass, enumColl) }

  /**
    * Compile subclasses for a given class.
    * @param curClass current type to generate code for
    */
  def compileSubclasses(curClass: ClassSpec): Unit =
    curClass.types.foreach { case (_, intClass) => compileClass(intClass) }

  def compileInstances(curClass: ClassSpec) = {
    curClass.instances.foreach { case (instName, instSpec) =>
      compileInstance(curClass.name, instName, instSpec, curClass.meta.endian)
    }
  }

  def compileInstance(className: List[String], instName: InstanceIdentifier, instSpec: InstanceSpec, endian: Option[Endianness]): Unit = {
    // Determine datatype
    val dataType = instSpec.dataTypeComposite

    compileInstanceDeclaration(instName, instSpec)

    if (!lang.innerDocstrings)
      compileInstanceDoc(instName, instSpec)
    lang.instanceHeader(className, instName, dataType, instSpec.isNullable)
    if (lang.innerDocstrings)
      compileInstanceDoc(instName, instSpec)
    val writingParseInst = config.readWrite && (instSpec match {
      case _: ParseInstanceSpec =>
        true
      case _: ValueInstanceSpec =>
        false
    })
    if (writingParseInst) {
      lang.instanceCheckWriteFlagAndWrite(instName)
    }
    lang.instanceCheckCacheAndReturn(instName, dataType)
    if (writingParseInst) {
      lang.instanceReturnNullIfDisabled(instName)
    }

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

    if (config.readWrite)
      instSpec match {
        case pi: ParseInstanceSpec =>
          lang.attributeSetter(instName, dataType, instSpec.isNullable)
          lang.instanceEnabledSetter(instName)
          lang.writeInstanceHeader(instName)
          lang.attrWrite(pi, instName, endian)
          lang.writeInstanceFooter
        case _: ValueInstanceSpec =>
          lang.instanceInvalidate(instName)
      }
  }

  def compileInstanceDeclaration(instName: InstanceIdentifier, instSpec: InstanceSpec): Unit = {
    val isNullable = if (lang.switchBytesOnlyAsRaw) {
      instSpec match {
        case pi: ParseInstanceSpec => pi.isNullableSwitchRaw
        case i => i.isNullable
      }
    } else {
      instSpec.isNullable
    }
    lang.instanceDeclaration(instName, instSpec.dataTypeComposite, isNullable)
    if (config.readWrite)
      instSpec match {
        case _: ParseInstanceSpec =>
          lang.instanceWriteFlagDeclaration(instName)
        case _: ValueInstanceSpec => // do nothing
      }
  }

  def compileEnum(curClass: ClassSpec, enumColl: EnumSpec): Unit =
    lang.enumDeclaration(curClass.name, enumColl.name.last, enumColl.map.toSeq)

  def isUnalignedBits(dt: DataType): Boolean =
    dt match {
      case _: BitsType | _: BitsType1 => true
      case et: EnumType => isUnalignedBits(et.basedOn)
      case _ => false
    }

  def compileClassDoc(curClass: ClassSpec): Unit = {
    if (!curClass.doc.isEmpty)
      lang.classDoc(curClass.name, curClass.doc)
  }

  def compileInstanceDoc(instName: Identifier, instSpec: InstanceSpec): Unit = {
    if (!instSpec.doc.isEmpty)
      lang.attributeDoc(instName, instSpec.doc)
  }
}
