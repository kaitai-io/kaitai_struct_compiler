package io.kaitai.struct

import io.kaitai.struct.ConstructDataclassCompiler.convertTypeToClass
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components.{LanguageCompiler, LanguageCompilerStatic, PythonOps}
import io.kaitai.struct.translators.ConstructTranslator

import scala.collection.mutable.ListBuffer

class ConstructDataclassCompiler(classSpecs: ClassSpecs, topClass: ClassSpec)
  extends AbstractCompiler {

  val out = new StringLanguageOutputWriter(indent)
  val importList = new ImportList
  val provider = new ClassTypeProvider(classSpecs, topClass)
  val translator = new ConstructTranslator(provider, importList)

  private val classList = new ListBuffer[String]()

  override def compile: CompileLog.SpecSuccess = {
    importList.add("# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild")
    importList.add("import typing as t")
    importList.add("import dataclasses as dc")

    // compile class
    this.compileClass(topClass)
    out.puts(s"_schema = ${convertTypeToClass(topClass.name)}")

    // Support for wildcard import of generated python files. By adding __all__ we
    // prevent importing all symbols of construct and construct_dataclasses.
    out.puts(s"__all__ = [")
    out.inc
    out.putsLines("", classList.mkString(",\n"))
    out.dec
    out.puts("]")

    // From ... imports should be placed at the end
    importList.add("\nfrom construct import *")
    importList.add("from construct.lib import *")
    importList.add("from construct_dataclasses import *")
    CompileLog.SpecSuccess(
      "",
      List(CompileLog.FileSuccess(
        outFileName(topClass.nameAsStr),
        importList.toList.mkString("\n") + "\n\n" + out.result + "\n"
      ))
    )
  }

  private def compileClass(cs: ClassSpec): Unit = {
    // 1. Step: compile all present enum definitions
    cs.enums.foreach({
      case (_, spec) => {
        this.compileEnum(spec)
      }
    })


    // 2. Step: compile all other structs
    cs.types.foreach({
      case (_, spec) => this.compileClass(spec)
    })

    // REVISIT: what if we have to process bitwise?
    out.puts("@dc.dataclass")
    out.puts(s"class ${convertTypeToClass(cs.name)}_t:")
    out.inc
    val docStr = PythonOps.compileUniversalDocs(cs.doc)
    if (docStr.nonEmpty) {
      out.putsLines("", "\"\"\"" + docStr + "\"\"\"")
    }

    classList.append(s"'${convertTypeToClass(cs.name)}'") // parser
    classList.append(s"'${convertTypeToClass(cs.name)}_t'") // dataclass
    provider.nowClass = cs
    cs.seq.foreach({
      seqAttribute => compileAttribute(seqAttribute)
    })
    cs.instances.foreach({
      case (identifier, spec) => spec match {
        case vis: ValueInstanceSpec => compileComputedField(identifier, vis)
        case pis: ParseInstanceSpec => compilePointerField(pis)
      }
    })

    out.dec
    out.puts("\n")
    out.puts(s"${convertTypeToClass(cs.name)} = DataclassStruct(${convertTypeToClass(cs.name)}_t)\n")
  }

  private def compileEnum(enumSpec: EnumSpec): Unit = {
    importList.add("import enum")
    out.puts(s"class ${convertTypeToClass(enumSpec.name)}(enum.IntEnum):")
    out.inc
    classList.append(s"'${convertTypeToClass(enumSpec.name)}'")

    enumSpec.sortedSeq.foreach({
      case (id, spec) => out.puts(s"${spec.name} = ${translator.doIntLiteral(id)}")
    })
    out.dec
    out.puts
  }

  private def compileAttribute(attributeSpec: AttrLikeSpec): Unit = {
    val name: String = getName(attributeSpec.id)
    val type_hint: String = getAttributeTypeHint(attributeSpec)
    val fieldType: String = getDataclassFieldType(attributeSpec.dataType)
    val body: String = compileAttributeBody(attributeSpec)
    if (type_hint != null) {
      out.puts(s"$name: ${correctListTypeHint(type_hint, attributeSpec)} = $fieldType$body)")
    } else {
      out.puts(s"$name = $fieldType($body)")
    }

    val docStr = PythonOps.compileUniversalDocs(attributeSpec.doc)
    if (docStr.nonEmpty) {
      out.putsLines("", "\"\"\"" + docStr + "\"\"\"")
    }
  }

  private def indent: String = " ".repeat(4)

  private def outFileName(topClassName: String): String = s"${topClassName}.py"

  private def getName(identifier: Identifier): String = {
    identifier match {
      case SpecialIdentifier(name) => name
      case InstanceIdentifier(name) => name
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case NamedIdentifier(name) => name
    }
  }

  private def withConditional(repeat: String, ifExpr: Option[Ast.expr]): String = {
    ifExpr match {
      case Some(value) => s"If(${translator.translate(value)}, $repeat)"
      case None => repeat
    }
  }

  private def compileAttributeBody(spec: AttrLikeSpec): String = {
    val defaultTypeRepr: String = toString(spec.dataType)
    val repeat: String = withRepeat(defaultTypeRepr, spec)
    withConditional(repeat, spec.cond.ifExpr)
  }

  private def withRepeat(repr: String, spec: AttrLikeSpec): String = {
    spec.cond.repeat match {
      case RepeatExpr(expr) => s"Array(${translator.translate(expr)}, $repr)"
      case RepeatUntil(expr) =>
        provider._currentIteratorType = Some(spec.dataType)
        s"RepeatUntil(lambda obj_, list_, this: ${translator.translate(expr)}, $repr)"
      case RepeatEos => s"GreedyRange($repr)"
      case NoRepeat => repr
    }
  }

  private def toString(dataType: DataType): String = dataType match {
    case Int1Type(signed) => s"Int8${correctSignedType(signed)}b"
    case IntMultiType(signed, width, endian) =>
      s"Int${width.width * 8}${correctSignedType(signed)}${endianToSting(endian.get)}"
    case FloatMultiType(width, endian) =>
      s"Float${width.width * 8}${endianToSting(endian.get)}"
    case BytesEosType(_, _, _, _) => "GreedyBytes"

    case StrFromBytesType(bytes, encoding) =>
      bytes match {
        case BytesEosType(_, _, _, _) => s"GreedyString(encoding='$encoding')"
        case blt: BytesLimitType =>
          createBytesLimitTypeConstruct(blt, s"GreedyString(encoding='$encoding')")
        case btt: BytesTerminatedType =>
          createBytesTerminatedTypeConstruct(btt, s"GreedyString(encoding='$encoding')")
      }
    case blt: BytesLimitType =>
      createBytesLimitTypeConstruct(blt)
    case btt: BytesTerminatedType =>
      createBytesTerminatedTypeConstruct(btt, "GreedyBytes")
    case ut: UserTypeInstream =>
      s"LazyBound(lambda: ${convertTypeToClass(ut.classSpec.get.name)})"
    case utb: UserTypeFromBytes =>
      utb.bytes match {
        case BytesLimitType(size, _, _, _, _) =>
          s"FixedSized(${translator.translate(size)}, LazyBound(lambda: ${convertTypeToClass(utb.classSpec.get.name)}.struct))"
        case _ => "???"
      }
    case BitsType1(endianness) =>
      val swapped = endianness match {
        case LittleBitEndian => "True"
        case BigBitEndian => "False"
      }
      s"Bitwise(BitsInteger(1, swapped=$swapped))"
    case BitsType(width, bitEndian) =>
      val swapped = bitEndian match {
        case LittleBitEndian => "True"
        case BigBitEndian => "False"
      }
      s"Bitwise(BitsInteger($width, swapped=$swapped))"
    case st: SwitchType => createSwitchConstruct(st)
    case enumType: EnumType =>
      s"Enum(${toString(enumType.basedOn)}, ${convertTypeToClass(enumType.enumSpec.get.name)})"
    case any => any.toString
  }

  private def correctSignedType(signed: Boolean): String = if (signed) "s" else "u"

  private def endianToSting(endian: FixedEndian): String = {
    endian match {
      case LittleEndian => "l"
      case BigEndian => "b"
      case _ => "n"
    }
  }

  private def getDataclassFieldType(dataType: DataType): String = {
    dataType match {
      case utb: UserTypeFromBytes => s"subcsfield(${convertTypeToClass(utb.classSpec.get.name)}_t, "
      case ut: UserTypeInstream => s"subcsfield(${convertTypeToClass(ut.classSpec.get.name)}_t, "
      case et: EnumType => s"tfield(${convertTypeToClass(et.enumSpec.get.name)}, "
      case _ => "csfield("
    }
  }

  private def getAttributeTypeHint(spec: AttrLikeSpec): String = {
    spec.dataType match {
      case utb: UserTypeFromBytes => convertTypeToClass(utb.classSpec.get.name)
      case enumType: EnumType => convertTypeToClass(enumType.enumSpec.get.name)
      case _: Int1Type | _: IntMultiType => "int"
      case _: FloatMultiType | _: FloatType => "float"
      case _: BitsType1 | _: BitsType => "int"
      case _: BytesEosType | _: BytesLimitType | _: BytesTerminatedType => "bytes"
      case ut: UserTypeInstream => s"${convertTypeToClass(ut.classSpec.get.name)}_t"
      case utb: UserTypeFromBytes => s"${convertTypeToClass(utb.classSpec.get.name)}_t"
      case StrFromBytesType(_, _) => "str"
      case _ => "t.Any"
    }
  }

  private def correctListTypeHint(typeHint: String, spec: AttrLikeSpec): String = {
    spec.cond.repeat match {
      case NoRepeat => typeHint
      case _ => s"t.List[$typeHint]"
    }
  }

  private def createBytesTerminatedTypeConstruct(btt: BytesTerminatedType, subCon: String): String = {
    val terminator = "\\x%02X".format(btt.terminator & 0xFF)
    s"NullTerminated($subCon, " +
      s"term=b'$terminator', " +
      s"include=${translator.doBoolLiteral(btt.include)}, " +
      s"consume=${translator.doBoolLiteral(btt.consume)})"
  }

  private def createBytesLimitTypeConstruct(blt: BytesLimitType, subCon: String = "GreedyBytes"): String = {
    val subCon2 = blt.terminator match {
      case None => subCon
      case Some(value) =>
        val term = "\\x%02X".format(value & 0xFF)
        return s"NullTerminated($subCon, term=b'$term', include=${translator.doBoolLiteral(blt.include)})"
    }
    val subCon3 = blt.padRight match {
      case None => subCon2
      case Some(value) =>
        val padding = "\\x%02X".format(value & 0xFF)
        return s"NullStripped($subCon2, pad=b'$padding')"
    }

    s"FixedSized(${translator.translate(blt.size)}, $subCon3)"
  }

  private def createSwitchConstruct(st: SwitchType): String = {
    val cases = st.cases.filter({
      case (expr, _) => expr != SwitchType.ELSE_CONST
    }).map({
      case (expr, dataType) => s"${translator.translate(expr)}: ${toString(dataType)}, "
    })

    val defaultSuffix = st.cases.get(SwitchType.ELSE_CONST).map({
      t => s", default=${toString(t)}"
    }).getOrElse("")

    s"Switch(${translator.translate(st.on)}, {${cases.mkString}}$defaultSuffix)"
  }

  private def compileComputedField(id: Identifier, vis: ValueInstanceSpec): Unit = {
    val structRepr = s"Computed(lambda this: ${translator.translate(vis.value)})"
    val conditionalRepr = withConditional(structRepr, vis.ifExpr)
    out.puts(s"${getName(id)}: t.Any = csfield($conditionalRepr)")
    val docStr = PythonOps.compileUniversalDocs(vis.doc)
    if (docStr.nonEmpty) {
      out.putsLines("", "\"\"\"" + docStr + "\"\"\"")
    }
  }

  private def compilePointerField(pis: ParseInstanceSpec): Unit = {
    val typeHint = getAttributeTypeHint(pis)
    val fieldType = getDataclassFieldType(pis.dataType)
    pis.pos match {
      case None => compileAttribute(pis)
      case Some(value) =>
        // TODO: subcsfield and tfield both need an extra argument
        out.puts(s"${getName(pis.id)}: $typeHint = ${fieldType}Pointer(${translator.translate(value)}, ${compileAttributeBody(pis)}))")
        val docStr = PythonOps.compileUniversalDocs(pis.doc)
        if (docStr.nonEmpty) {
          out.putsLines("", "\"\"\"" + docStr + "\"\"\"")
        }
    }
  }

}

object ConstructDataclassCompiler extends LanguageCompilerStatic {
  override def getCompiler(tp: ClassTypeProvider, config: RuntimeConfig): LanguageCompiler = ???

  def convertTypeToClass(name: List[String]): String = {
    // REVISIT:
    name.mkString("__")
  }

}
