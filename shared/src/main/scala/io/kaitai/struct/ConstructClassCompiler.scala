package io.kaitai.struct

import io.kaitai.struct.datatype._
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components.{LanguageCompiler, LanguageCompilerStatic}
import io.kaitai.struct.translators.PythonTranslator

class ConstructClassCompiler(classSpecs: ClassSpecs, topClass: ClassSpec) extends AbstractCompiler {
  val out = new StringLanguageOutputWriter(indent)
  val importList = new ImportList

  val provider = new ClassTypeProvider(classSpecs, topClass)
  val translator = new PythonTranslator(provider, importList)

  override def compile: CompileLog.SpecSuccess = {
    out.puts("from construct import *")
    out.puts("from construct.lib import *")
    out.puts

    compileClass(topClass)

    out.puts(s"_schema = ${type2class(topClass)}")

    CompileLog.SpecSuccess(
      "",
      List(CompileLog.FileSuccess(
        outFileName(topClass.nameAsStr),
        out.result
      ))
    )
  }

  def compileClass(cs: ClassSpec): Unit = {
    cs.types.foreach { case (_, typeSpec) => compileClass(typeSpec) }

    cs.enums.foreach { case (_, enumSpec) => compileEnum(enumSpec) }

    out.puts(s"${type2class(cs)} = Struct(")
    out.inc

    cs.seq.foreach((seqAttr) => compileAttr(seqAttr))

    out.dec
    out.puts(")")
    out.puts
  }

  def compileAttr(attr: AttrSpec): Unit = {
    out.puts(s"'${idToStr(attr.id)}' / ${typeToStr(attr.dataType)},")
  }

  def compileEnum(enumSpec: EnumSpec): Unit = {
    out.puts(s"def ${enumToStr(enumSpec)}(subcon):")
    out.inc
    out.puts("return Enum(subcon,")
    out.inc
    enumSpec.sortedSeq.foreach { case (number, valueSpec) =>
      out.puts(s"${valueSpec.name}=$number,")
    }
    out.dec
    out.puts(")")
    out.dec
    out.puts
  }

  def idToStr(id: Identifier): String = {
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => name
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => name
    }
  }

  def type2class(cs: ClassSpec) = cs.name.mkString("__")

  def enumToStr(enumSpec: EnumSpec) = enumSpec.name.mkString("__")

  def typeToStr(dataType: DataType): String = dataType match {
    case Int1Type(signed) =>
      s"Int8${if (signed) "s" else "u"}b"
    case IntMultiType(signed, width, endianOpt) =>
      s"Int${width.width * 8}${if (signed) "s" else "u"}${fixedEndianToStr(endianOpt.get)}"
    case StrFromBytesType(bytes, encoding) =>
      bytes match {
        case BytesEosType(terminator, include, padRight, process) =>
          s"GreedyString(encoding='$encoding')"
        case BytesLimitType(size, terminator, include, padRight, process) =>
          s"FixedSized(${translator.translate(size)}, GreedyString(encoding='$encoding'))"
        case BytesTerminatedType(terminator, include, consume, eosError, process) =>
          val termStr = "\\x%02X".format(terminator & 0xff)
          s"NullTerminated(GreedyString(encoding='$encoding'), " +
            s"term=b'$termStr', " +
            s"include=${translator.doBoolLiteral(include)}, " +
            s"consume=${translator.doBoolLiteral(consume)})"
      }
    case ut: UserTypeInstream =>
      type2class(ut.classSpec.get)
    case utb: UserTypeFromBytes =>
      utb.bytes match {
        //case BytesEosType(terminator, include, padRight, process) =>
        case BytesLimitType(size, terminator, include, padRight, process) =>
          s"FixedSized(${translator.translate(size)}, ${type2class(utb.classSpec.get)})"
        //case BytesTerminatedType(terminator, include, consume, eosError, process) =>
        case _ => "???"
      }
    case et: EnumType =>
      s"${enumToStr(et.enumSpec.get)}(${typeToStr(et.basedOn)})"
    case _ => "???"
  }

  def fixedEndianToStr(e: FixedEndian) = e match {
    case LittleEndian => "l"
    case BigEndian => "b"
  }

  def indent: String = "\t"
  def outFileName(topClassName: String): String = s"$topClassName.py"
}

object ConstructClassCompiler extends LanguageCompilerStatic {
  // FIXME: Unused, should be probably separated from LanguageCompilerStatic
  override def getCompiler(tp: ClassTypeProvider, config: RuntimeConfig): LanguageCompiler = ???
}
