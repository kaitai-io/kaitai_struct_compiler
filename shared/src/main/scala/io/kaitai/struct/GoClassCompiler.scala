package io.kaitai.struct

import io.kaitai.struct.datatype.DataType.{CalcIntType, KaitaiStreamType, UserTypeInstream, UserType}
import io.kaitai.struct.datatype.{BigEndian, CalcEndian, Endianness, FixedEndian, InheritedEndian, LittleEndian}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.languages.GoCompiler
import io.kaitai.struct.languages.components.ExtraAttrs

class GoClassCompiler(
  classSpecs: ClassSpecs,
  override val topClass: ClassSpec,
  config: RuntimeConfig
) extends ClassCompiler(classSpecs, topClass, config, GoCompiler) {

  override def compileClass(curClass: ClassSpec): Unit = {
    provider.nowClass = curClass

    curClass.meta.imports.foreach(file => lang.importFile(file))

    if (!lang.innerDocstrings)
      compileClassDoc(curClass)
    lang.classHeader(curClass.name)
    if (lang.innerDocstrings)
      compileClassDoc(curClass)

    // Forward declarations for recursive types
    curClass.types.foreach { case (typeName, _) => lang.classForwardDeclaration(List(typeName)) }

    // Forward declarations for params which reference types external to this type
    curClass.params.foreach((paramDefSpec) =>
      paramDefSpec.dataType match {
        case ut: UserType =>
          val externalTypeName = ut.classSpec.get.name
          if (externalTypeName.head != curClass.name.head) {
            lang.classForwardDeclaration(externalTypeName)
          }
        case _ => // no forward declarations needed
      }
    )

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
      compileCheck(curClass.seq)
    }

    // Destructor
    compileDestructor(curClass)

    // Recursive types
    if (lang.innerClasses) {
      compileSubclasses(curClass)

      provider.nowClass = curClass
    }

    compileInstances(curClass)

    val extraAttrs = List(
      AttrSpec(List(), IoIdentifier, KaitaiStreamType),
      AttrSpec(List(), RootIdentifier, UserTypeInstream(topClassName, None)),
      AttrSpec(List(), ParentIdentifier, curClass.parentType)
    ) ++ ExtraAttrs.forClassSpec(curClass, lang)

    if (!curClass.doc.isEmpty)
      lang.classDoc(curClass.name, curClass.doc)

    // Enums declaration defines types, so they need to go first
    compileEnums(curClass)

    // Basic struct declaration
    lang.classHeader(curClass.name)
    compileAttrDeclarations(curClass.seq ++ curClass.params ++ extraAttrs)
    curClass.instances.foreach { case (instName, instSpec) =>
      compileInstanceDeclaration(instName, instSpec)
    }
    lang.classFooter(curClass.name)

    // Constructor = Read() function
    compileReadFunction(curClass)

    curClass.toStringExpr.foreach(expr => lang.classToString(expr))

    compileInstances(curClass)

    compileAttrReaders(curClass.seq ++ extraAttrs)

    // Recursive types
    compileSubclasses(curClass)
  }

  def compileReadFunction(curClass: ClassSpec) = {
    lang.classConstructorHeader(
      curClass.name,
      curClass.parentType,
      topClassName,
      curClass.meta.endian.contains(InheritedEndian),
      curClass.params
    )
    compileEagerRead(curClass.seq, curClass.meta.endian)
    lang.classConstructorFooter
  }

  override def compileInstance(className: List[String], instName: InstanceIdentifier, instSpec: InstanceSpec, endian: Option[Endianness]): Unit = {
    // Determine datatype
    val dataType = instSpec.dataTypeComposite

    if (!instSpec.doc.isEmpty)
      lang.attributeDoc(instName, instSpec.doc)
    lang.instanceHeader(className, instName, dataType, instSpec.isNullable)
    lang.instanceCheckCacheAndReturn(instName, dataType)

    instSpec match {
      case vi: ValueInstanceSpec =>
        lang.attrParseIfHeader(instName, vi.ifExpr)
        lang.instanceCalculate(instName, dataType, vi.value)
        lang.attrParseIfFooter(vi.ifExpr)
      case i: ParseInstanceSpec =>
        lang.attrParse(i, instName, endian)
    }

    lang.instanceSetCalculated(instName)
    lang.instanceReturn(instName, dataType)
    lang.instanceFooter
  }

  override def compileCalcEndian(ce: CalcEndian): Unit = {
    def renderProc(result: FixedEndian): Unit = {
      val v = result match {
        case LittleEndian => Ast.expr.IntNum(1)
        case BigEndian => Ast.expr.IntNum(0)
      }
      lang.instanceCalculate(IS_LE_ID, CalcIntType, v)
    }
    lang.switchCases[FixedEndian](IS_LE_ID, ce.on, ce.cases, renderProc, renderProc)
  }
}
