package io.kaitai.struct.languages

import io.kaitai.struct.{LanguageOutputWriter, Utils}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format._
import io.kaitai.struct.translators.{BaseTranslator, RubyTranslator, TypeProvider}

class RubyCompiler(verbose: Boolean, override val debug: Boolean, out: LanguageOutputWriter)
  extends LanguageCompiler(verbose, out)
    with UniversalFooter
    with UpperCamelCaseClasses
    with AllocateIOLocalVar
    with EveryReadIsExpression
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

  override def attributeReader(attrName: Identifier, attrType: BaseType): Unit = {
    out.puts(s"attr_reader :${publicMemberName(attrName)}")
  }

  override def attrFixedContentsParse(attrName: Identifier, contents: Array[Byte]): Unit = {
    out.puts(s"${privateMemberName(attrName)} = @_io.ensure_fixed_contents(${contents.length}, [${contents.map(x => x.toInt & 0xff).mkString(", ")}])")
  }

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit = {
    val srcName = privateMemberName(varSrc)
    val destName = privateMemberName(varDest)

    out.puts(proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "process_xor_one"
          case _: BytesType => "process_xor_many"
        }
        s"$destName = _io.$procName($srcName, ${expression(xorValue)})"
      case ProcessZlib =>
        s"$destName = Zlib::Inflate.inflate($srcName)"
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        s"$destName = _io.process_rotate_left($srcName, $expr, 1)"
    })
  }

  override def normalIO: String = "@_io"

  override def allocateIO(id: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(id)

    val args = rep match {
      case RepeatEos => s"$memberName.last"
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

  override def attrDebugStart(attrId: NamedIdentifier, io: String, rep: RepeatSpec): Unit = {
    val name = attrId.name
    rep match {
      case NoRepeat =>
        out.puts(s"(@_debug['$name'] ||= {})[:start] = $io.pos")
      case _: RepeatExpr =>
        out.puts(s"(@_debug['$name'][:arr] ||= [])[i] = {:start => $io.pos}")
      case RepeatEos =>
        out.puts(s"(@_debug['$name'][:arr] ||= [])[${privateMemberName(attrId)}.size] = {:start => $io.pos}")
    }
  }

  override def attrDebugEnd(attrId: NamedIdentifier, io: String, rep: RepeatSpec): Unit = {
    val name = attrId.name
    rep match {
      case NoRepeat =>
        out.puts(s"(@_debug['$name'] ||= {})[:end] = $io.pos")
      case _: RepeatExpr =>
        out.puts(s"@_debug['$name'][:arr][i][:end] = $io.pos")
      case RepeatEos =>
        out.puts(s"@_debug['$name'][:arr][${privateMemberName(attrId)}.size - 1][:end] = $io.pos")
    }
  }

  override def condIfHeader(expr: Ast.expr): Unit = {
    out.puts(s"if ${expression(expr)}")
    out.inc
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean): Unit = {
    if (needRaw)
      out.puts(s"@_raw_$id = []")

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

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)} = $expr")

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
      case EnumType(enumName, t) =>
        s"${value2Const(enumName)}[${parseExpr(t, io)}]"

      case BytesLimitType(size, _) =>
        s"$io.read_bytes(${expression(size)})"
      case BytesEosType(_) =>
        s"$io.read_bytes_full"
      case t: UserType =>
        val r = s"${type2class(t.name.last)}.new($io, self, @_root)"
        if (debug) {
          s"$r._read"
        } else {
          r
        }
    }
  }

  override def instanceHeader(className: String, instName: InstanceIdentifier, dataType: BaseType): Unit = {
    out.puts(s"def ${instName.name}")
    out.inc
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier): Unit = {
    out.puts(s"return ${privateMemberName(instName)} if ${privateMemberName(instName)}")
  }

  override def instanceReturn(instName: InstanceIdentifier): Unit = {
    out.puts(privateMemberName(instName))
  }

  override def enumDeclaration(curClass: String, enumName: String, enumColl: Map[Long, String]): Unit = {
    out.puts
    out.puts(s"${value2Const(enumName)} = {")
    out.inc
    enumColl.foreach { case (id, label) =>
      out.puts(s"$id => ${enumValue(enumName, label)},")
    }
    out.dec
    out.puts("}")
  }

  def enumValue(enumName: String, enumLabel: String) = translator.doEnumByLabel(enumName, enumLabel)

  def value2Const(s: String) = s.toUpperCase

  override def idToStr(id: Identifier): String = {
    id match {
      case NamedIdentifier(name) => name
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
