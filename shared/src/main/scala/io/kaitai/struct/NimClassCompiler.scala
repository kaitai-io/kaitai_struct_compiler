package io.kaitai.struct

import io.kaitai.struct.datatype.{DataType, Endianness, FixedEndian, InheritedEndian}
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
    out.puts
    compileProcs(topClass)


    CompileLog.SpecSuccess(
      "",
      List(CompileLog.FileSuccess(
        outFileName(topClass.nameAsStr),
        imports + "\n\n" + globalPragmas + "\n\n" + out.result
      ))
    )
  }

  def indent: String = "  "
  def outFileName(topClassName: String): String = s"$topClassName.nim"
  def imports =
    importList.toList.map((x) => s"import $x").mkString("\n")

  def globalPragmas =
    globalPragmaList.toList.map((x) => s"{.$x.}").mkString("\n")

  def compileTypes(curClass: ClassSpec): Unit = {
    compileSubtypes(curClass)

    val t = listToNim(curClass.name)
    out.puts(s"${t}* = ref ${t}Obj")
    out.puts(s"${t}Obj* = object")
    out.inc

    val extraAttrs = List(
      AttrSpec(List(), IoIdentifier, KaitaiStreamType),
      AttrSpec(List(), RootIdentifier, UserTypeInstream(topClass.name, None)),
      AttrSpec(List(), ParentIdentifier, curClass.parentType)
    )

    (extraAttrs ++ curClass.seq).foreach {
      (attr) => {
        val i = idToStr(attr.id)
        val t = ksToNim(attr.dataTypeComposite)
        out.puts(s"${i}${if (attr.id == IoIdentifier) "" else "*" }: $t")
      }
    }

    if (curClass.instances.size != 0) {
      importList.add("options")
      globalPragmaList.add("experimental: \"dotOperators\"")
    }

    curClass.instances.foreach {
      case (id, spec) => {
        val i = idToStr(id)
        val t = ksToNim(spec.dataTypeComposite)
        out.puts(s"${i}: proc(): $t")
      }
    }
    out.dec
  }

  def compileSubtypes(curClass: ClassSpec): Unit = {
    curClass.types.foreach { case (_, subClass) => compileTypes(subClass) }
  }

  def compileProcs(curClass: ClassSpec): Unit = {
    compileSubtypeProcs(curClass)

    val t = listToNim(curClass.name)
    val p = ksToNim(curClass.parentType)
    val r = camelCase(topClass.name.head, true)
    
    out.puts(s"# $t")

    if (curClass.instances.size != 0) {
      out.puts(s"template `.`*(a: $t, b: untyped): untyped =")
      out.inc
      out.puts("(a.`b inst`)()")
      out.dec
      out.puts
    }

    out.puts(s"proc read*(_: typedesc[$t], io: KaitaiStream, root: $r, parent: $p): owned $t =")
    out.inc
    out.puts(s"result = new($t)")
    out.puts(s"let root = if root == nil: cast[$r](result) else: root")
    out.puts("result.io = io")
    out.puts("result.root = root")
    out.puts("result.parent = parent")
    out.puts
    curClass.seq.foreach {
      (attr) => {
        val i = idToStr(attr.id)
        val t = attr.dataTypeComposite
        // XXX: fix endian
        out.puts(s"result.$i = ${parse(t, "io", "result", None)}")
      }
    }
    out.dec
    out.puts
    out.puts(s"proc fromFile*(_: typedesc[$t], filename: string): owned $t =")
    out.inc
    out.puts(s"$t.read(newKaitaiStream(filename), nil, nil)")
    out.dec
    out.puts
    out.puts(s"proc `=destroy`(x: var ${t}Obj) =")
    out.inc
    out.puts("close(x.io)")
    out.dec
    out.puts
  }

  def compileSubtypeProcs(curClass: ClassSpec): Unit = {
    curClass.types.foreach { case (_, subClass) => compileProcs(subClass) }
  }

  def parse(dataType: DataType, io: String, obj: String, endian: Option[FixedEndian]): String = {
    def process(unproc: String, proc: Option[ProcessExpr]): String =
      proc match {
        case None => unproc
        case Some(proc) => 
          proc match {
            case ProcessXor(key) => unproc + s".processXor(${evalLocal(key)})"
          }
      }

    dataType match {
      case t: ReadableType =>
        s"read${Utils.capitalize(t.apiCall(endian))}($io)"
      case t: BytesLimitType =>
        process(s"readBytes($io, int(${evalLocal(t.size)}))", t.process)
      case t: BytesEosType =>
        process(s"readBytesFull($io)", t.process)
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        s"readBytesTerm($io, $terminator, $include, $consume, $eosError)"
      case BitsType1 =>
        s"bool(readBitsInt($io, 1))"
      case BitsType(width: Int) =>
        s"readBitsInt($io, $width)"
      case t: UserType =>
        val addArgs = if (t.isOpaque) {
          ""
        } else {
          val parent = t.forcedParent match {
            case Some(USER_TYPE_NO_PARENT) => "nil"
            case Some(fp) => translator.translate(fp)
            case None => obj
          }
          s", root, $parent"
        }
        s"${listToNim(t.name)}.read($io$addArgs)"
    }
  }

  def evalLocal(e: Ast.expr): String =
    s"${e match {case Ast.expr.Name(_) => "result." case _ => ""}}${translator.translate(e)}"

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

  def listToNim(names: List[String]) = camelCase(names.last, true)

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

      case t: UserType => listToNim(t.name)
      case EnumType(name, _) => listToNim(name)

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