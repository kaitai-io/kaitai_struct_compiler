package io.kaitai.struct

import io.kaitai.struct.datatype.DataType.{KaitaiStreamType, UserTypeInstream}
import io.kaitai.struct.format._
import io.kaitai.struct.languages.GoCompiler

import scala.collection.mutable.ListBuffer

class GoClassCompiler(
  override val topClass: ClassSpec,
  config: RuntimeConfig
) extends ClassCompiler(topClass, config, GoCompiler) {

  override def compileClass(curClass: ClassSpec): Unit = {
    provider.nowClass = curClass

    val extraAttrs = ListBuffer[AttrSpec]()
    extraAttrs += AttrSpec(List(), IoIdentifier, KaitaiStreamType)
    extraAttrs += AttrSpec(List(), RootIdentifier, UserTypeInstream(topClassName, None))
    extraAttrs += AttrSpec(List(), ParentIdentifier, curClass.parentType)

    if (!curClass.doc.isEmpty)
      lang.classDoc(curClass.name, curClass.doc)

    // Basic struct declaration
    lang.classHeader(curClass.name)
    compileAttrDeclarations(curClass.seq ++ extraAttrs)
    lang.classFooter(curClass.name)

    // Read method
    lang.classConstructorHeader(curClass.name, curClass.parentTypeName, topClassName)
    curClass.instances.foreach { case (instName, _) => lang.instanceClear(instName) }
    compileSeq(curClass.seq, extraAttrs)
    lang.classConstructorFooter

    curClass.instances.foreach { case (instName, instSpec) => compileInstance(curClass.name, instName, instSpec, extraAttrs) }

    (curClass.seq ++ extraAttrs).foreach { (attr) =>
      if (!attr.doc.isEmpty)
        lang.attributeDoc(attr.id, attr.doc)
      lang.attributeReader(attr.id, attr.dataTypeComposite, attr.cond)
    }

    compileEnums(curClass)

    // Recursive types
    compileSubclasses(curClass)
  }
}
