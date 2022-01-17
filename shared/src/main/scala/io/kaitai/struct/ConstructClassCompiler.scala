package io.kaitai.struct

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components.{LanguageCompiler, LanguageCompilerStatic}
import io.kaitai.struct.translators.ConstructTranslator

class ConstructClassCompiler(classSpecs: ClassSpecs, topClass: ClassSpec) extends AbstractCompiler {
  val out = new StringLanguageOutputWriter(indent)
  val importList = new ImportList

  val provider = new ClassTypeProvider(classSpecs, topClass)
  val translator = new ConstructTranslator(provider, importList)

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
    cs.enums.foreach { case (_, enumSpec) => compileEnum(enumSpec) }

    cs.types.foreach { case (_, typeSpec) => compileClass(typeSpec) }

    out.puts(s"${type2class(cs)} = Struct(")
    out.inc

    provider.nowClass = cs

    cs.seq.foreach((seqAttr) => compileAttr(seqAttr))
    cs.instances.foreach { case (id, instSpec) =>
      instSpec match {
        case vis: ValueInstanceSpec =>
          compileValueInstance(id, vis)
        case pis: ParseInstanceSpec =>
          compileParseInstance(pis)
      }
    }

    out.dec
    out.puts(")")
    out.puts
  }

  def compileAttr(attr: AttrLikeSpec): Unit = {
    out.puts(s"'${idToStr(attr.id)}' / ${compileAttrBody(attr)},")
  }

  def compileValueInstance(id: Identifier, vis: ValueInstanceSpec): Unit = {
    val typeStr = s"Computed(lambda this: ${translator.translate(vis.value)})"
    val typeStr2 = wrapWithIf(typeStr, vis.ifExpr)
    out.puts(s"'${idToStr(id)}' / $typeStr2,")
  }

  def compileParseInstance(attr: ParseInstanceSpec): Unit = {
    attr.pos match {
      case None =>
        compileAttr(attr)
      case Some(pos) =>
        out.puts(s"'${idToStr(attr.id)}' / " +
          s"Pointer(${translator.translate(pos)}, ${compileAttrBody(attr)}),")
    }
  }

  def compileAttrBody(attr: AttrLikeSpec): String = {
    val typeStr1 = typeToStr(attr.dataType)
    val typeStr2 = wrapWithRepeat(typeStr1, attr.cond.repeat, attr.dataType)
    wrapWithIf(typeStr2, attr.cond.ifExpr)
  }

  def wrapWithRepeat(typeStr: String, repeat: RepeatSpec, dataType: DataType) = repeat match {
    case RepeatExpr(expr) =>
      s"Array(${translator.translate(expr)}, $typeStr)"
    case RepeatUntil(expr) =>
      provider._currentIteratorType = Some(dataType)
      s"RepeatUntil(lambda obj_, list_, this: ${translator.translate(expr)}, $typeStr)"
    case RepeatEos =>
      s"GreedyRange($typeStr)"
    case NoRepeat =>
      typeStr
  }

  def wrapWithIf(typeStr: String, ifExpr: Option[Ast.expr]) = ifExpr match {
    case Some(expr) => s"If(${translator.translate(expr)}, $typeStr)"
    case None => typeStr
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
      s"Int8${signToStr(signed)}b"
    case IntMultiType(signed, width, endianOpt) =>
      s"Int${width.width * 8}${signToStr(signed)}${fixedEndianToStr(endianOpt.get)}"
    case FloatMultiType(width, endianOpt) =>
      s"Float${width.width * 8}${fixedEndianToStr(endianOpt.get)}"
    case BytesEosType(terminator, include, padRight, process) =>
      "GreedyBytes"
    case blt: BytesLimitType =>
      attrBytesLimitType(blt)
    case btt: BytesTerminatedType =>
      attrBytesTerminatedType(btt, "GreedyBytes")
    case StrFromBytesType(bytes, encoding) =>
      bytes match {
        case BytesEosType(terminator, include, padRight, process) =>
          s"GreedyString(encoding='$encoding')"
        case blt: BytesLimitType =>
          attrBytesLimitType(blt, s"GreedyString(encoding='$encoding')")
        case btt: BytesTerminatedType =>
          attrBytesTerminatedType(btt, s"GreedyString(encoding='$encoding')")
      }
    case ut: UserTypeInstream =>
      s"LazyBound(lambda: ${type2class(ut.classSpec.get)})"
    case utb: UserTypeFromBytes =>
      utb.bytes match {
        //case BytesEosType(terminator, include, padRight, process) =>
        case BytesLimitType(size, terminator, include, padRight, process) =>
          s"FixedSized(${translator.translate(size)}, LazyBound(lambda: ${type2class(utb.classSpec.get)}))"
        //case BytesTerminatedType(terminator, include, consume, eosError, process) =>
        case _ => "???"
      }
    case et: EnumType =>
      s"${enumToStr(et.enumSpec.get)}(${typeToStr(et.basedOn)})"
    case st: SwitchType =>
      attrSwitchType(st)
    case _ => "???"
  }

  def attrBytesLimitType(blt: BytesLimitType, subcon: String = "GreedyBytes"): String = {
    val subcon2 = blt.terminator match {
      case None => subcon
      case Some(term) =>
        val termStr = "\\x%02X".format(term & 0xff)
        s"NullTerminated($subcon, term=b'$termStr', include=${translator.doBoolLiteral(blt.include)})"
    }
    val subcon3 = blt.padRight match {
      case None => subcon2
      case Some(padRight) =>
        val padStr = "\\x%02X".format(padRight & 0xff)
        s"NullStripped($subcon2, pad=b'$padStr')"
    }
    s"FixedSized(${translator.translate(blt.size)}, $subcon3)"
  }

  def attrBytesTerminatedType(btt: BytesTerminatedType, subcon: String): String = {
    val termStr = "\\x%02X".format(btt.terminator & 0xff)
    s"NullTerminated($subcon, " +
      s"term=b'$termStr', " +
      s"include=${translator.doBoolLiteral(btt.include)}, " +
      s"consume=${translator.doBoolLiteral(btt.consume)})"
  }

  def attrSwitchType(st: SwitchType): String = {
    val cases = st.cases.filter { case (caseExpr, _) =>
      caseExpr != SwitchType.ELSE_CONST
    }.map { case (caseExpr, caseType) =>
      s"${translator.translate(caseExpr)}: ${typeToStr(caseType)}, "
    }

    val defaultSuffix = st.cases.get(SwitchType.ELSE_CONST).map((t) =>
      s", default=${typeToStr(t)}"
    ).getOrElse("")

    s"Switch(${translator.translate(st.on)}, {${cases.mkString}}$defaultSuffix)"
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
