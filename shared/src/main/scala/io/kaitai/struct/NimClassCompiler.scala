package io.kaitai.struct

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components.{LanguageCompiler, LanguageCompilerStatic}
import io.kaitai.struct.translators.NimTranslator

class NimClassCompiler(
  classSpecs: ClassSpecs,
  topClass: ClassSpec,
  config: RuntimeConfig
) extends AbstractCompiler {
  import NimClassCompiler._

  val out = new StringLanguageOutputWriter(indent)
  val provider = new ClassTypeProvider(classSpecs, topClass)
  val importList = new ImportList
  val globalPragmaList = new ImportList
  val translator = new NimTranslator(provider, importList)

  override def compile: CompileLog.SpecSuccess = {
    importList.add(config.nimModule)
    out.puts("type")
    out.inc
    compileTypes(topClass)
    out.dec


    CompileLog.SpecSuccess(
      "",
      List(CompileLog.FileSuccess(
        outFileName(topClass.nameAsStr),
        imports + "\n" + globalPragmas + "\n" + out.result
      ))
    )
  }

  def indent: String = "  "
  def outFileName(topClassName: String): String = s"$topClassName.nim"
  def imports =
    importList.toList.map((x) => s"import $x").mkString("\n")

  def globalPragmas =
    globalPragmaList.toList.map((x) => s"{.$x.}").mkString("\n")

  def compileSubtypes(curClass: ClassSpec): Unit = {
    curClass.types.foreach { case (_, subClass) => compileTypes(subClass) }
  }

  def compileTypes(curClass: ClassSpec): Unit = {
    compileSubtypes(curClass)

    val t = camelCase(curClass.name.last, true)
    out.puts(s"${t}* = ref ${t}Obj")
    out.puts(s"${t}Obj* = object")
    out.inc

    val extraAttrs = List(
      AttrSpec(List(), IoIdentifier, KaitaiStreamType),
      AttrSpec(List(), RootIdentifier, UserTypeInstream(topClass.name, None)),
      AttrSpec(List(), ParentIdentifier, curClass.parentType)
    )

    (extraAttrs ++ curClass.seq).foreach {
      (attr) => compileAttrDeclaration(attr.id, attr.dataTypeComposite)
    }

    //compileInstDeclarations(curClass)

    out.dec
  }

  def compileAttrDeclaration(id: Identifier, dt: DataType): Unit = {
    val i = idToStr(id)
    val t = ksToNim(dt)
    out.puts(s"${i}${if (id == IoIdentifier) "" else "*" }: $t")
  }

  def camelCase(s: String, upper: Boolean): String = {
    if (upper) {
      s.split("_").map(Utils.capitalize).mkString
    } else {
      if (s.startsWith("_")) {
        camelCase(s.substring(1), false)
      } else {
        val firstWord :: restWords = s.split("_").toList
        (firstWord :: restWords.map(Utils.capitalize)).mkString
      }
    }
  }

  def idToStr(id: Identifier): String = {
    id match {
      case IoIdentifier => "io"
      case NamedIdentifier(name) =>  camelCase(name, false)
      case InstanceIdentifier(name) => camelCase(name, false) + "Inst"
      case IoStorageIdentifier(innerId) => "io" + camelCase(idToStr(innerId), true)

      case SpecialIdentifier(name) => camelCase(name, false)
      case NumberedIdentifier(idx) => s"${NumberedIdentifier.TEMPLATE}$idx"
      case RawIdentifier(innerId) => "raw" + camelCase(idToStr(innerId), true)
    }
  }

  def ksToNim(attrType: DataType): String = {
    attrType match {
      case Int1Type(false) => "uint8"
      case IntMultiType(false, Width2, _) => "uint16"
      case IntMultiType(false, Width4, _) => "uint32"
      case IntMultiType(false, Width8, _) => "uint64"

      case Int1Type(true) => "int8"
      case IntMultiType(true, Width2, _) => "int16"
      case IntMultiType(true, Width4, _) => "int32"
      case IntMultiType(true, Width8, _) => "int64"

      case FloatMultiType(Width4, _) => "float32"
      case FloatMultiType(Width8, _) => "float64"

      case BitsType(_) => "uint64"

      case _: BooleanType => "bool"
      case CalcIntType => "int"
      case CalcFloatType => "float64"

      case _: StrType => "string"
      case _: BytesType => "seq[byte]"

      case KaitaiStructType | CalcKaitaiStructType => "ref RootObj"
      case KaitaiStreamType => "KaitaiStream"

      case t: UserType => camelCase(t.name.last, true)
      case EnumType(name, _) => camelCase(name.last, true)

      case ArrayType(inType) => s"seq[${ksToNim(inType)}]"

      case st: SwitchType => ksToNim(st.combinedType)
    }
  }
}

object NimClassCompiler extends LanguageCompilerStatic {
  // FIXME: Unused, should probably be separated from LanguageCompilerStatic
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = ???
}