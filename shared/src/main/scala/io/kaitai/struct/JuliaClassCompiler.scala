package io.kaitai.struct

import io.kaitai.struct.datatype.DataType.{CalcIntType, KaitaiStreamType, UserType, UserTypeInstream}
import io.kaitai.struct.datatype.{BigEndian, CalcEndian, Endianness, FixedEndian, LittleEndian}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.languages.JuliaCompiler
import io.kaitai.struct.languages.components.ExtraAttrs

class JuliaClassCompiler(
  classSpecs: ClassSpecs,
  override val topClass: ClassSpec,
  config: RuntimeConfig
) extends ClassCompiler(classSpecs, topClass, config, JuliaCompiler) {

  private val julialang = lang.asInstanceOf[JuliaCompiler]

  override def compileClass(curClass: ClassSpec): Unit = {
    provider.nowClass = curClass

    val extraAttrs = List(
      AttrSpec(List(), IoIdentifier, KaitaiStreamType),
      AttrSpec(List(), RootIdentifier, UserTypeInstream(topClassName, None)),
      AttrSpec(List(), ParentIdentifier, curClass.parentType)
    ) ++ ExtraAttrs.forClassSpec(curClass, lang)

    curClass.types.foreach { case (typeName, _) => lang.classForwardDeclaration(curClass.name ++ List(typeName)) }

    if (!curClass.doc.isEmpty)
      lang.classDoc(curClass.name, curClass.doc)

    // Enums declaration defines types, so they need to go first
    compileEnums(curClass)

    if (lang.config.readStoresPos)
      lang.debugClassSequence(curClass.seq)

    // Basic struct declaration
    lang.classHeader(curClass.name)
    compileAttrDeclarations(curClass.seq ++ curClass.params ++ extraAttrs)
    curClass.instances.foreach { case (instName, instSpec) =>
      compileInstanceDeclaration(instName, instSpec)
    }
    compileConstructor(curClass)
    lang.classFooter(curClass.name)

    // Constructor = Read() function
    if (curClass.isTopLevel)
      julialang.fromFile(curClass.name)

    compileEagerRead(curClass.seq, curClass.meta.endian)

    julialang.overrideGetProperty(curClass.name, curClass.instances)
    compileInstances(curClass)

    compileAttrReaders(curClass.seq ++ extraAttrs)

    // Recursive types
    compileSubclasses(curClass)
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
        case LittleEndian => Ast.expr.Bool(true)
        case BigEndian => Ast.expr.Bool(false)
      }
      lang.instanceCalculate(IS_LE_ID, CalcIntType, v)
    }
    lang.switchCases[FixedEndian](IS_LE_ID, ce.on, ce.cases, renderProc, renderProc)
  }

  override def compileAttrDeclarations(attrs: List[MemberSpec]): Unit = {
    attrs.foreach { (attr) =>
      val isNullable = if (lang.switchBytesOnlyAsRaw) {
        attr.isNullableSwitchRaw
      } else {
        attr.isNullable || attr.dataType.isInstanceOf[UserType]
      }
      lang.attributeDeclaration(attr.id, attr.dataTypeComposite, isNullable)
    }
  }
}
