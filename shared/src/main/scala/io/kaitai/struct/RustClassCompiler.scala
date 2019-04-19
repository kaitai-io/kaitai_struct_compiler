package io.kaitai.struct

import io.kaitai.struct.datatype.DataType.{KaitaiStreamType, UserTypeInstream}
import io.kaitai.struct.datatype.{Endianness, FixedEndian, InheritedEndian}
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
    lang.classFooter(curClass.name)

    compileEnums(curClass)

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

    // FIXME
    val defEndian = curClass.meta.endian match {
      case Some(fe: FixedEndian) => Some(fe)
      case _ => None
    }
    
    lang.readHeader(defEndian, false)
    
    compileSeq(curClass.seq, defEndian)
    lang.classConstructorFooter
  }

  override def compileInstances(curClass: ClassSpec) = {
    lang.instanceDeclHeader(curClass.name)
    curClass.instances.foreach { case (instName, instSpec) =>
      compileInstance(curClass.name, instName, instSpec, curClass.meta.endian)
    }
  }

  override def compileInstance(className: List[String], instName: InstanceIdentifier, instSpec: InstanceSpec, endian: Option[Endianness]): Unit = {
    // FIXME: support calculated endianness

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
        lang.attrParse(i, instName, None) // FIXME
    }

    lang.instanceSetCalculated(instName)
    lang.instanceReturn(instName, dataType)
    lang.instanceFooter
  }
}
