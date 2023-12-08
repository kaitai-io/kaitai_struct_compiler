package io.kaitai.struct

import io.kaitai.struct.datatype.DataType.{CalcIntType, KaitaiStreamType, UserType, UserTypeInstream}
import io.kaitai.struct.datatype.{BigEndian, CalcEndian, Endianness, FixedEndian, InheritedEndian, LittleEndian}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.languages.GoCompiler
import io.kaitai.struct.languages.components.ExtraAttrs
import io.kaitai.struct.translators.GoTranslator

class GoClassCompiler(
  classSpecs: ClassSpecs,
  override val topClass: ClassSpec,
  config: RuntimeConfig
) extends ClassCompiler(classSpecs, topClass, config, GoCompiler) {

  override def compileClass(curClass: ClassSpec): Unit = {
    provider.nowClass = curClass

    compileFetchInstancesProc(curClass.seq ++ curClass.instances.values.collect {
      case inst: AttrLikeSpec => inst
    })

    if (config.readWrite) {
      compileWrite(curClass.seq, curClass.instances, curClass.meta.endian)
      compileCheck(curClass.seq)
    }

    val extraAttrs = List(
      AttrSpec(List(), IoIdentifier, KaitaiStreamType),
      AttrSpec(List(), RootIdentifier, UserTypeInstream(topClassName, None)),
      AttrSpec(List(), ParentIdentifier, curClass.parentType),
    ) ++ ExtraAttrs.forClassSpec(curClass, lang)

    if (!curClass.doc.isEmpty)
      lang.classDoc(curClass.name, curClass.doc)

    // Enums declaration defines types, so they need to go first
    compileEnums(curClass)

    // Basic struct declaration
    lang.classHeader(curClass.name)

    if (config.readWrite) {
      lang.classExtendMarks(List("kaitai.Stream"))
    }

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
    if (config.readWrite)
      instSpec match {
        case _: ParseInstanceSpec =>
          lang.instanceCheckWriteFlagAndWrite(instName)
        case _: ValueInstanceSpec => // do nothing
      }
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

    if (config.readWrite)
      instSpec match {
        case pi: ParseInstanceSpec =>
          lang.attributeSetter(instName, dataType, instSpec.isNullable)
          lang.instanceToWriteSetter(instName)
          lang.writeInstanceHeader(instName)
          lang.attrWrite(pi, instName, endian)
          lang.writeInstanceFooter

          lang.checkInstanceHeader(instName)
          lang.attrCheck(pi, instName)
          lang.checkInstanceFooter
        case _: ValueInstanceSpec =>
          var newInst = instName
          if (lang.translator.isInstanceOf[GoTranslator]) {
            newInst = InstanceIdentifier(instName.name.concat(s"_insplit_${dataType.toString.split('(')(0).toLowerCase()}"))
          }
          lang.instanceInvalidate(newInst)
      }
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
