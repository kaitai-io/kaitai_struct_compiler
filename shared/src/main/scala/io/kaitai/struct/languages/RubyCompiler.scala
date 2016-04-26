package io.kaitai.struct.languages

import io.kaitai.struct.{LanguageOutputWriter, Utils}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format._
import io.kaitai.struct.translators.{BaseTranslator, RubyTranslator, TypeProvider}

class RubyCompiler(verbose: Boolean, override val debug: Boolean, out: LanguageOutputWriter)
  extends LanguageCompiler(verbose, out)
    with UpperCamelCaseClasses
    with EveryReadIsExpression
    with NoNeedForFullClassPath {
  override def getTranslator(tp: TypeProvider): BaseTranslator = new RubyTranslator(tp)

  override def fileHeader(topClassName: String): Unit = {
    out.puts(s"# $headerComment")
    out.puts
    out.puts("require 'kaitai/struct/struct'")
    out.puts("require 'zlib'") // TODO: add only if actually used
    out.puts
  }

  override def classHeader(name: String): Unit = {
    out.puts(s"class ${type2class(name)} < Kaitai::Struct::Struct")
    out.inc
    if (debug)
      out.puts("attr_reader :_debug")
  }

  override def classFooter(name: String = null): Unit = {
    out.dec
    out.puts("end")
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
    classFooter()
  }

  override def attributeDeclaration(attrName: String, attrType: BaseType): Unit = {}

  override def attributeReader(attrName: String, attrType: BaseType): Unit = {
    out.puts(s"attr_reader :$attrName")
  }

  override def attrFixedContentsParse(attrName: String, contents: Array[Byte]): Unit = {
    out.puts(s"${privateMemberName(attrName)} = @_io.ensure_fixed_contents(${contents.length}, [${contents.map(x => x.toInt & 0xff).mkString(", ")}])")
  }

  override def attrProcess(proc: ProcessExpr, varSrc: String, varDest: String): Unit = {
    out.puts(proc match {
      case ProcessXor(xorValue) =>
        s"${privateMemberName(varDest)} = ${privateMemberName(varSrc)}.bytes.map { |x| (x ^ (${expression(xorValue)})) }.pack('C*')"
      case ProcessZlib =>
        s"${privateMemberName(varDest)} = Zlib::Inflate.inflate(${privateMemberName(varSrc)})"
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        s"${privateMemberName(varDest)} = _io.process_rotate_left($varSrc, $expr, 1)"
    })
  }

  override def normalIO: String = "@_io"

  override def allocateIO(varName: String, rep: RepeatSpec): String = {
    val args = rep match {
      case RepeatEos => s"@$varName.last"
      case RepeatExpr(_) => s"@$varName[i]"
      case NoRepeat => s"@$varName"
    }

    out.puts(s"io = Kaitai::Struct::Stream.new($args)")
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

  override def attrDebugStart(attrId: String, io: String, rep: RepeatSpec): Unit = {
    rep match {
      case NoRepeat =>
        out.puts(s"(@_debug['$attrId'] ||= {})[:start] = $io.pos")
      case _: RepeatExpr =>
        out.puts(s"(@_debug['$attrId'][:arr] ||= [])[i] = {:start => $io.pos}")
      case RepeatEos =>
        out.puts(s"(@_debug['$attrId'][:arr] ||= [])[@$attrId.size] = {:start => $io.pos}")
    }
  }

  override def attrDebugEnd(attrId: String, io: String, rep: RepeatSpec): Unit = {
    rep match {
      case NoRepeat =>
        out.puts(s"(@_debug['$attrId'] ||= {})[:end] = $io.pos")
      case _: RepeatExpr =>
        out.puts(s"@_debug['$attrId'][:arr][i][:end] = $io.pos")
      case RepeatEos =>
        out.puts(s"@_debug['$attrId'][:arr][@$attrId.size - 1][:end] = $io.pos")
    }
  }

  override def condIfHeader(expr: Ast.expr): Unit = {
    out.puts(s"if ${expression(expr)}")
    out.inc
  }
  override def condIfFooter(expr: Ast.expr): Unit = {
    out.dec
    out.puts("end")
  }

  override def condRepeatEosHeader(id: String, io: String, dataType: BaseType, needRaw: Boolean): Unit = {
    if (needRaw)
      out.puts(s"@_raw_$id = []")

    out.puts(s"@$id = []")
    out.puts(s"while not $io.eof?")
    out.inc
  }
  override def handleAssignmentRepeatEos(id: String, expr: String): Unit =
    out.puts(s"@$id << $expr")
  override def condRepeatEosFooter: Unit = {
    out.dec
    out.puts("end")
  }

  override def condRepeatExprHeader(id: String, io: String, dataType: BaseType, needRaw: Boolean, repeatExpr: expr): Unit = {
    if (needRaw)
      out.puts(s"@_raw_$id = Array.new(${expression(repeatExpr)})")
    out.puts(s"@$id = Array.new(${expression(repeatExpr)})")
    out.puts(s"(${expression(repeatExpr)}).times { |i|")
    out.inc
  }
  override def handleAssignmentRepeatExpr(id: String, expr: String): Unit =
    out.puts(s"@$id[i] = $expr")
  override def condRepeatExprFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def handleAssignmentSimple(id: String, expr: String): Unit =
    out.puts(s"@$id = $expr")

  override def parseExpr(dataType: BaseType, io: String): String = {
    dataType match {
      case t: IntType =>
        s"$io.read_${t.apiCall}"
      // Aw, crap, can't use interpolated strings here: https://issues.scala-lang.org/browse/SI-6476
      case StrByteLimitType(bs, encoding) =>
        s"$io.read_str_byte_limit(${expression(bs)}, " + '"' + encoding + "\")"
      case StrEosType(encoding) =>
        io + ".read_str_eos(\"" + encoding + "\")"
      case StrZType(encoding, terminator, include, consume, eosError) =>
        io + ".read_strz(\"" + encoding + '"' + s", $terminator, $include, $consume, $eosError)"
      case EnumType(enumName, t) =>
        s"${value2Const(enumName)}[$io.read_${t.apiCall}]"

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

  override def instanceHeader(className: String, instName: String, dataType: BaseType): Unit = {
    out.puts(s"def $instName")
    out.inc
  }

  override def instanceAttrName(instName: String): String = instName

  override def instanceFooter: Unit = classFooter()

  override def instanceCheckCacheAndReturn(instName: String): Unit = {
    out.puts(s"return @$instName if @$instName")
  }

  override def instanceReturn(instName: String): Unit = {
    out.puts(s"@$instName")
  }

  override def instanceCalculate(instName: String, value: expr): Unit = {
    out.puts(s"@${instanceAttrName(instName)} = ${expression(value)}")
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

  override def privateMemberName(ksName: String): String = s"@$ksName"
}

object RubyCompiler extends LanguageCompilerStatic {
  override def outFileName(topClassName: String): String = s"$topClassName.rb"
  override def indent: String = "  "
}
