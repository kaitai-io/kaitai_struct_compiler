package io.kaitai.struct.languages

import io.kaitai.struct.{LanguageOutputWriter, RuntimeConfig, Utils}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.{BaseTranslator, RubyTranslator, TypeProvider}

class RubyCompiler(config: RuntimeConfig, out: LanguageOutputWriter)
  extends LanguageCompiler(config, out)
    with ObjectOrientedLanguage
    with UniversalFooter
    with UniversalDoc
    with UpperCamelCaseClasses
    with AllocateIOLocalVar
    with EveryReadIsExpression
    with FixedContentsUsingArrayByteLiteral
    with NoNeedForFullClassPath {

  import RubyCompiler._

  override def getStatic = RubyCompiler

  override def universalFooter: Unit = {
    out.dec
    out.puts("end")
  }

  override def fileHeader(topClassName: String): Unit = {
    out.puts(s"# $headerComment")
    out.puts
    out.puts("require 'kaitai/struct/struct'")
    out.puts("require 'zlib'") // TODO: add only if actually used
    out.puts
  }

  override def classHeader(name: String): Unit = {
    out.puts(s"class ${type2class(name)} < $kstructName")
    out.inc
    if (debug)
      out.puts("attr_reader :_debug")
  }

  override def classConstructorHeader(name: String, parentClassName: String, rootClassName: String): Unit = {
    out.puts("def initialize(_io, _parent = nil, _root = self)")
    out.inc
    out.puts("super(_io, _parent, _root)")
    if (debug) {
      out.puts("@_debug = {}")
      out.dec
      out.puts("end")
      out.puts
      out.puts("def _read")
      out.inc
    }
  }

  override def classConstructorFooter: Unit = {
    if (debug) {
      // Actually, it's not constructor in debug mode, but a "_read" method. Make sure it returns an instance of the
      // class, just as normal Foo.new call does.
      out.puts
      out.puts("self")
    }
    universalFooter
  }

  override def attributeDeclaration(attrName: Identifier, attrType: BaseType, condSpec: ConditionalSpec): Unit = {}

  override def attributeReader(attrName: Identifier, attrType: BaseType, condSpec: ConditionalSpec): Unit = {
    attrName match {
      case RootIdentifier | ParentIdentifier =>
        // ignore, they are already added in Kaitai::Struct::Struct
      case _ =>
        out.puts(s"attr_reader :${publicMemberName(attrName)}")
    }
  }

  override def universalDoc(doc: String): Unit = {
    out.puts
    out.puts( "##")
    out.putsLines("# ", doc)
  }

  override def attrFixedContentsParse(attrName: Identifier, contents: String): Unit =
    out.puts(s"${privateMemberName(attrName)} = $normalIO.ensure_fixed_contents($contents)")

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit = {
    val srcName = privateMemberName(varSrc)
    val destName = privateMemberName(varDest)

    out.puts(proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "process_xor_one"
          case _: BytesType => "process_xor_many"
        }
        s"$destName = $kstreamName::$procName($srcName, ${expression(xorValue)})"
      case ProcessZlib =>
        s"$destName = Zlib::Inflate.inflate($srcName)"
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        s"$destName = $kstreamName::process_rotate_left($srcName, $expr, 1)"
    })
  }

  override def allocateIO(id: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(id)

    val args = rep match {
      case RepeatEos | RepeatUntil(_) => s"$memberName.last"
      case RepeatExpr(_) => s"$memberName[i]"
      case NoRepeat => s"$memberName"
    }

    out.puts(s"io = $kstreamName.new($args)")
    "io"
  }

  override def useIO(ioEx: expr): String = {
    out.puts(s"io = ${expression(ioEx)}")
    "io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"_pos = $io.pos")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io.seek(${expression(pos)})")

  override def popPos(io: String): Unit =
    out.puts(s"$io.seek(_pos)")

  override def alignToByte(io: String): Unit =
    out.puts(s"$io.align_to_byte")

  override def attrDebugStart(attrId: Identifier, attrType: BaseType, ios: Option[String], rep: RepeatSpec): Unit = {
    ios.foreach { (io) =>
      val name = attrId match {
        case NamedIdentifier(name) => name
        case InstanceIdentifier(name) => name
        case _: RawIdentifier | _: SpecialIdentifier => return
      }
      rep match {
        case NoRepeat =>
          out.puts(s"(@_debug['$name'] ||= {})[:start] = $io.pos")
        case _: RepeatExpr =>
          out.puts(s"(@_debug['$name'][:arr] ||= [])[i] = {:start => $io.pos}")
        case RepeatEos | _: RepeatUntil =>
          out.puts(s"(@_debug['$name'][:arr] ||= [])[${privateMemberName(attrId)}.size] = {:start => $io.pos}")
      }
    }
  }

  override def attrDebugEnd(attrId: Identifier, attrType: BaseType, io: String, rep: RepeatSpec): Unit = {
    val name = attrId match {
      case NamedIdentifier(name) => name
      case InstanceIdentifier(name) => name
      case _: RawIdentifier | _: SpecialIdentifier => return
    }
    rep match {
      case NoRepeat =>
        out.puts(s"(@_debug['$name'] ||= {})[:end] = $io.pos")
      case _: RepeatExpr =>
        out.puts(s"@_debug['$name'][:arr][i][:end] = $io.pos")
      case RepeatEos | _: RepeatUntil =>
        out.puts(s"@_debug['$name'][:arr][${privateMemberName(attrId)}.size - 1][:end] = $io.pos")
    }
  }

  override def condIfHeader(expr: Ast.expr): Unit = {
    out.puts(s"if ${expression(expr)}")
    out.inc
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = []")

    out.puts(s"${privateMemberName(id)} = []")
    out.puts(s"while not $io.eof?")
    out.inc
  }
  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)} << $expr")

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean, repeatExpr: expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = Array.new(${expression(repeatExpr)})")
    out.puts(s"${privateMemberName(id)} = Array.new(${expression(repeatExpr)})")
    out.puts(s"(${expression(repeatExpr)}).times { |i|")
    out.inc
  }
  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)}[i] = $expr")
  override def condRepeatExprFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean, untilExpr: expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = []")
    out.puts(s"${privateMemberName(id)} = []")
    out.puts("begin")
    out.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String): Unit = {
    out.puts(s"${translator.doName("_")} = $expr")
    out.puts(s"${privateMemberName(id)} << ${translator.doName("_")}")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean, untilExpr: expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.dec
    out.puts(s"end until ${expression(untilExpr)}")
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)} = $expr")

  override def handleAssignmentTempVar(dataType: BaseType, id: String, expr: String): Unit =
    out.puts(s"$id = $expr")

  override def parseExpr(dataType: BaseType, io: String): String = {
    dataType match {
      case t: ReadableType =>
        s"$io.read_${t.apiCall}"
      // Aw, crap, can't use interpolated strings here: https://issues.scala-lang.org/browse/SI-6476
      case StrByteLimitType(bs, encoding) =>
        s"$io.read_str_byte_limit(${expression(bs)}, " + '"' + encoding + "\")"
      case StrEosType(encoding) =>
        io + ".read_str_eos(\"" + encoding + "\")"
      case StrZType(encoding, terminator, include, consume, eosError) =>
        io + ".read_strz(\"" + encoding + '"' + s", $terminator, $include, $consume, $eosError)"
      case BytesLimitType(size, _) =>
        s"$io.read_bytes(${expression(size)})"
      case BytesEosType(_) =>
        s"$io.read_bytes_full"
      case BitsType1 =>
        s"$io.read_bits_int(1) != 0"
      case BitsType(width: Int) =>
        s"$io.read_bits_int($width)"
      case t: UserType =>
        val addArgs = if (t.isOpaque) "" else ", self, @_root"
        s"${type2class(t.name.last)}.new($io$addArgs)"
    }
  }

  override def userTypeDebugRead(id: String): Unit =
    out.puts(s"$id._read")

  override def switchStart(id: Identifier, on: Ast.expr): Unit =
    out.puts(s"case ${expression(on)}")

  override def switchCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"when ${expression(condition)}")
    out.inc
  }

  override def switchCaseEnd(): Unit =
    out.dec

  override def switchElseStart(): Unit = {
    out.puts("else")
    out.inc
  }

  override def switchEnd(): Unit =
    out.puts("end")

  override def instanceHeader(className: String, instName: InstanceIdentifier, dataType: BaseType): Unit = {
    out.puts(s"def ${instName.name}")
    out.inc
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier): Unit = {
    out.puts(s"return ${privateMemberName(instName)} unless ${privateMemberName(instName)}.nil?")
  }

  override def instanceReturn(instName: InstanceIdentifier): Unit = {
    out.puts(privateMemberName(instName))
  }

  override def enumDeclaration(curClass: String, enumName: String, enumColl: Seq[(Long, String)]): Unit = {
    out.puts
    out.puts(s"${value2Const(enumName)} = {")
    out.inc
    enumColl.foreach { case (id, label) =>
      out.puts(s"$id => ${enumValue(enumName, label)},")
    }
    out.dec
    out.puts("}")
  }

  def enumValue(enumName: String, enumLabel: String) = translator.doEnumByLabel(List(enumName), enumLabel)

  def value2Const(s: String) = s.toUpperCase

  override def debugClassSequence(seq: List[AttrSpec]) = {
    val seqStr = seq.map((attr) => "\"" + idToStr(attr.id) + "\"").mkString(", ")
    out.puts(s"SEQ_FIELDS = [$seqStr]")
  }

  override def idToStr(id: Identifier): String = {
    id match {
      case NamedIdentifier(name) => name
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case si: SpecialIdentifier => si.name
      case RawIdentifier(inner) => s"_raw_${idToStr(inner)}"
      case InstanceIdentifier(name) => name
    }
  }

  override def privateMemberName(id: Identifier): String = s"@${idToStr(id)}"

  override def publicMemberName(id: Identifier): String = idToStr(id)
}

object RubyCompiler extends LanguageCompilerStatic
  with StreamStructNames {
  override def getTranslator(tp: TypeProvider): BaseTranslator = new RubyTranslator(tp)
  override def outFileName(topClassName: String): String = s"$topClassName.rb"
  override def indent: String = "  "

  override def kstreamName: String = "Kaitai::Struct::Stream"
  override def kstructName: String = "Kaitai::Struct::Struct"
}
