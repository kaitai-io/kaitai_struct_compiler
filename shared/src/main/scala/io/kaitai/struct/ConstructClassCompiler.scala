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
    cs.instances.foreach { case (id, instSpec) =>
      instSpec match {
        case vis: ValueInstanceSpec =>
          compileValueInstance(id, vis)
        case pis: ParseInstanceSpec =>
          compileAttr(pis)
      }
    }

    out.dec
    out.puts(")")
    out.puts
  }

  def compileAttr(attr: AttrLikeSpec): Unit = {
    val typeStr1 = typeToStr(attr.dataType)
    val typeStr2 = attr.cond.repeat match {
      case RepeatExpr(expr) =>
        s"Array(${translator.translate(expr)}, $typeStr1)"
      case RepeatUntil(expr) =>
        "???"
      case RepeatEos =>
        s"GreedyRange($typeStr1)"
      case NoRepeat =>
        typeStr1
    }
    out.puts(s"'${idToStr(attr.id)}' / $typeStr2,")
  }

  def compileValueInstance(id: Identifier, vis: ValueInstanceSpec): Unit = {
    out.puts(s"'${idToStr(id)}' / Computed(${translator.translate(vis.value)}),")
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
    case fbt: FixedBytesType =>
      s"Const(${translator.doByteArrayLiteral(fbt.contents)})"
    case Int1Type(signed) =>
      s"Int8${signToStr(signed)}b"
    case IntMultiType(signed, width, endianOpt) =>
      s"Int${width.width * 8}${signToStr(signed)}${fixedEndianToStr(endianOpt.get)}"
    case FloatMultiType(width, endianOpt) =>
      s"Float${width.width * 8}${fixedEndianToStr(endianOpt.get)}"
    case BytesEosType(terminator, include, padRight, process) =>
      "GreedyBytes"
    case BytesLimitType(size, terminator, include, padRight, process) =>
      s"Bytes(${translator.translate(size)})"
    case btt: BytesTerminatedType =>
      attrBytesTerminatedType(btt, "GreedyBytes")
    case StrFromBytesType(bytes, encoding) =>
      bytes match {
        case BytesEosType(terminator, include, padRight, process) =>
          s"GreedyString(encoding='$encoding')"
        case BytesLimitType(size, terminator, include, padRight, process) =>
          s"FixedSized(${translator.translate(size)}, GreedyString(encoding='$encoding'))"
        case btt: BytesTerminatedType =>
          attrBytesTerminatedType(btt, s"GreedyString(encoding='$encoding')")
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

  def attrBytesTerminatedType(btt: BytesTerminatedType, subcon: String): String = {
    val termStr = "\\x%02X".format(btt.terminator & 0xff)
    s"NullTerminated($subcon, " +
      s"term=b'$termStr', " +
      s"include=${translator.doBoolLiteral(btt.include)}, " +
      s"consume=${translator.doBoolLiteral(btt.consume)})"
  }

  def signToStr(signed: Boolean) = if (signed) "s" else "u"

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
