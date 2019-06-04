package io.kaitai.struct

import io.kaitai.struct.datatype.DataType.{KaitaiStreamType, UserTypeInstream, CalcUserType}
import io.kaitai.struct.datatype.{Endianness, FixedEndian, InheritedEndian}
import io.kaitai.struct.format._
import io.kaitai.struct.languages.TypeScriptCompiler
import io.kaitai.struct.languages.components.ExtraAttrs

class TypeScriptClassCompiler(
  classSpecs: ClassSpecs,
  override val topClass: ClassSpec,
  config: RuntimeConfig
) extends ClassCompiler(classSpecs, topClass, config, TypeScriptCompiler) {

  override def compileClass(curClass: ClassSpec): Unit = {
    provider.nowClass = curClass

    // Forward declarations for recursive types
    curClass.types.foreach { case (typeName, _) => lang.classForwardDeclaration(List(typeName)) }

    // documentation
    compileClassDoc(curClass)

    // class thing {...
    lang.classHeader(curClass.name)

    // static <enum name> = Object.freeze({...})
    compileEnums(curClass)

    // static <subclass name> = class {...
    compileSubclasses(curClass)

    provider.nowClass = curClass


    // public <attrname>: <attrtype>;
    val allAttrs: List[MemberSpec] =
      curClass.seq ++
      curClass.params ++
      List(
        AttrSpec(List(), RootIdentifier, CalcUserType(topClassName, None)),
        AttrSpec(List(), ParentIdentifier, curClass.parentType)
      ) ++
      ExtraAttrs.forClassSpec(curClass, lang)
    compileAttrReaders(allAttrs)

    // constructor() {...}
    compileConstructor(curClass)

    compileEagerRead(curClass.seq, curClass.meta.endian)

    // private <private instance name>: ...;
    // get <public instance name>() {...}
    compileInstances(curClass)

    // }
    lang.classFooter(curClass.name)
  }

  override def compileSeqProc(seq: List[AttrSpec], defEndian: Option[FixedEndian]) = {
    lang.readHeader(defEndian, seq.isEmpty)
    compileSeq(seq, defEndian)
    lang.readFooter()
  }

  override def compileSeq(seq: List[AttrSpec], defEndian: Option[FixedEndian]) = {
    var wasUnaligned = false
    seq.foreach { (attr) =>
      val nowUnaligned = isUnalignedBits(attr.dataType)
      if (wasUnaligned && !nowUnaligned)
        lang.alignToByte(lang.normalIO)
      lang.attrParse(attr, attr.id, defEndian)
      wasUnaligned = nowUnaligned
    }
  }
}
