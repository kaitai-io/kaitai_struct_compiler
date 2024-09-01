package io.kaitai.struct

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.languages.RustCompiler
import io.kaitai.struct.languages.components.ExtraAttrs

import scala.collection.mutable.ListBuffer

class RustClassCompiler(
  classSpecs: ClassSpecs,
  override val topClass: ClassSpec,
  config: RuntimeConfig
) extends ClassCompiler(classSpecs, topClass, config, RustCompiler) {

  override def compileClass(curClass: ClassSpec): Unit = {
    provider.nowClass = curClass

    val extraAttrs = ListBuffer[AttrSpec]()
    extraAttrs += AttrSpec(List(), IoIdentifier, KaitaiStreamType)
    extraAttrs += AttrSpec(List(), RootIdentifier, UserTypeInstream(topClassName, None))
    extraAttrs += AttrSpec(List(), ParentIdentifier, curClass.parentType)

    extraAttrs ++= ExtraAttrs.forClassSpec(curClass, lang)

    if (!curClass.doc.isEmpty)
      lang.classDoc(curClass.name, curClass.doc)

    // Basic struct declaration
    lang.classHeader(curClass.name)

    compileAttrDeclarations(curClass.seq ++ extraAttrs)
    curClass.instances.foreach { case (instName, instSpec) =>
      compileInstanceDeclaration(instName, instSpec)
    }

    // Constructor = Read() function
    compileReadFunction(curClass)

    compileInstances(curClass)

    compileAttrReaders(curClass.seq ++ extraAttrs)
    curClass.toStringExpr.foreach(expr => lang.classToString(expr))
    lang.classFooter(curClass.name)

    compileEnums(curClass)

    // Recursive types
    compileSubclasses(curClass)
  }

  def compileReadFunction(curClass: ClassSpec): Unit = {
    lang.classConstructorHeader(
      curClass.name,
      curClass.parentType,
      topClassName,
      curClass.meta.endian.contains(InheritedEndian),
      curClass.params
    )

    val defEndian = curClass.meta.endian match {
      case Some(fe: FixedEndian) => Some(fe)
      case _ => None
    }

    lang.readHeader(defEndian, isEmpty = false)

    curClass.meta.endian match {
      case Some(ce: CalcEndian) => compileCalcEndian(ce)
      case Some(_) => // Nothing to generate
      case None => // Same here
    }

    compileSeq(curClass.seq, defEndian)
    lang.classConstructorFooter
  }

  override def compileCalcEndian(ce: CalcEndian): Unit = {
    def renderProc(result: FixedEndian): Unit = {
      val v = result match {
        case LittleEndian => Ast.expr.IntNum(1)
        case BigEndian => Ast.expr.IntNum(2)
      }
      lang.instanceCalculate(IS_LE_ID, CalcIntType, v)
    }
    lang.switchCases[FixedEndian](IS_LE_ID, ce.on, ce.cases, renderProc, renderProc)
    lang.runReadCalc()
  }

  override def compileInstances(curClass: ClassSpec): Unit = {
    lang.instanceDeclHeader(curClass.name)
    curClass.instances.foreach { case (instName, instSpec) =>
      compileInstance(curClass.name, instName, instSpec, curClass.meta.endian)
    }
    lang.instanceFooter
  }

  override def compileInstance(className: List[String], instName: InstanceIdentifier, instSpec: InstanceSpec, endian: Option[Endianness]): Unit = {
    // FIXME: support calculated endianness

    // Determine datatype
    val dataType = instSpec.dataTypeComposite

    if (!instSpec.doc.isEmpty)
      lang.attributeDoc(instName, instSpec.doc)
    lang.instanceHeader(className, instName, dataType, instSpec.isNullable)
    lang.instanceCheckCacheAndReturn(instName, dataType)

    lang.instanceSetCalculated(instName)
    instSpec match {
      case vi: ValueInstanceSpec =>
        lang.attrParseIfHeader(instName, vi.ifExpr)
        lang.instanceCalculate(instName, dataType, vi.value)
        lang.attrParseIfFooter(vi.ifExpr)
      case pi: ParseInstanceSpec =>
        lang.attrParse(pi, instName, None) // FIXME
    }

    lang.instanceReturn(instName, dataType)
    lang.instanceFooter
  }
}
