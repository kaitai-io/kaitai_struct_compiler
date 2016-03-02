package io.kaitai.struct.languages

import io.kaitai.struct.LanguageOutputWriter
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format.{ProcessXor, ProcessExpr, AttrSpec}
import io.kaitai.struct.translators.{BaseTranslator, TypeProvider, RubyTranslator}

class RubyCompiler(verbose: Boolean, outDir: String) extends LanguageCompiler(verbose, outDir) with UpperCamelCaseClasses with EveryReadIsExpression {
  override def getTranslator(tp: TypeProvider): BaseTranslator = new RubyTranslator(tp)

  override def outFileName(topClassName: String): String = s"${topClassName}.rb"
  override def indent: String = "  "

  override def fileHeader(sourceFileName: String, topClassName: String): Unit = {
    out.puts(s"# This file was generated from '${sourceFileName}' with kaitai_struct compiler")
    out.puts
    out.puts("require 'kaitai_struct'")
    out.puts
  }

  override def classHeader(name: String): Unit = {
    out.puts(s"class ${type2class(name)}")
    out.inc
    out.puts("include KaitaiStruct")
    out.puts

    // Helper method to read from local file
    out.puts("def self.from_file(filename)")
    out.inc
    out.puts("self.new(File.open(filename, 'rb:ASCII-8BIT'))")
    out.dec
    out.puts("end")
    out.puts
  }

  override def classFooter(name: String = null): Unit = {
    out.dec
    out.puts("end")
  }

  override def classConstructorHeader(name: String): Unit = {
    out.puts("def initialize(io, parent = nil)")
    out.inc
    out.puts("@_io = io")
    out.puts("@_parent = parent")
  }

  override def classConstructorFooter: Unit = classFooter()

  override def attributeDeclaration(attrName: String, attrType: String, isArray: Boolean): Unit = {}

  override def attributeReader(attrName: String, attrType: String, isArray: Boolean): Unit = {
    out.puts(s"attr_reader :${attrName}")
  }

  override def attrFixedContentsParse(attrName: String, contents: Array[Byte]): Unit = {
    out.puts(s"@${attrName} = ensure_fixed_contents(${contents.size}, [${contents.mkString(", ")}])")
  }

  override def attrNoTypeWithSize(varName: String, size: Ast.expr) {
    out.puts(s"@${varName} = @_io.read(${expression(size)})")
  }

  override def attrNoTypeWithSizeEos(varName: String) {
    out.puts(s"@${varName} = @_io.read")
  }

  override def attrUserTypeParse(id: String, attr: AttrSpec, io: String): Unit = {
    handleAssignment(id, attr, s"${type2class(attr.dataType)}.new(${io}, self)", io)
  }

  override def attrProcess(proc: ProcessExpr, varSrc: String, varDest: String): Unit = {
    out.puts(proc match {
      case ProcessXor(xorValue) =>
        s"@$varDest = @$varSrc.bytes.map { |x| (x ^ (${expression(xorValue)})) }.pack('C*')"
    })
  }

  override def normalIO: String = "@_io"

  override def allocateIO(varName: String): String = {
    out.puts(s"io = StringIO.new(@${varName})")
    "io"
  }

  override def seek(io: String, pos: Ast.expr): Unit = {
    out.puts(s"${io}.seek(${expression(pos)})")
  }

  override def handleAssignment(id: String, attr: AttrSpec, expr: String, io: String): Unit = {
    if (attr.ifExpr.isDefined) {
      out.puts(s"if ${expression(attr.ifExpr.get)}")
      out.inc
    }

    attr.repeat match {
      case Some("eos") =>
        out.puts(s"@${id} = []")
        out.puts(s"while not ${io}.eof?")
        out.inc
        out.puts(s"@${id} << ${expr}")
        out.dec
        out.puts("end")
      case Some("expr") =>
        attr.repeatExpr match {
          case Some(repeatExpr) =>
            out.puts(s"@${id} = Array.new(${expression(repeatExpr)}) {")
            out.inc
            out.puts(expr)
            out.dec
            out.puts("}")

          case None =>
            throw new RuntimeException("repeat: expr, but no repeat-expr value given")
        }
      case None => out.puts(s"@${id} = ${expr}")
    }

    if (attr.ifExpr.isDefined) {
      out.dec
      out.puts("end")
    }
  }

  def stdTypeParseExpr(attr: AttrSpec, endian: Option[String]): String = {
    attr.dataType match {
      case "u1" | "s1" | "u2le" | "u2be" | "u4le" | "u4be" | "u8le" | "u8be" | "s2le" | "s2be" | "s4le" | "s4be" | "s8le" | "s8be"  =>
        s"read_${attr.dataType}"
      case "u2" | "u4" | "u8" | "s2" | "s4" | "s8" =>
        endian match {
          case Some(e) => s"read_${attr.dataType}${e}"
          case None => throw new RuntimeException(s"Unable to parse ${attr.dataType} with no default endianess defined")
        }
      case null => throw new RuntimeException("should never happen")
      // Aw, crap, can't use interpolated strings here: https://issues.scala-lang.org/browse/SI-6476

      case "str" =>
        ((attr.size, attr.sizeEos)) match {
          case (Some(bs: Ast.expr), false) =>
            s"read_str_byte_limit(${expression(bs)}, " + '"' + attr.encoding.get + "\")"
          case (None, true) =>
            "read_str_eos(\"" + attr.encoding.get + "\")"
          case (None, false) =>
            throw new RuntimeException("type str: either \"size\" or \"size-eos\" must be specified")
          case (Some(_), true) =>
            throw new RuntimeException("type str: only one of \"size\" or \"size-eos\" must be specified")
        }
      case "strz" =>
        "read_strz(\"" + attr.encoding.get + '"' + s", ${attr.terminator}, ${attr.include}, ${attr.consume}, ${attr.eosError})"
    }
  }

  override def instanceHeader(className: String, instName: String, dataType: String, isArray: Boolean): Unit = {
    out.puts(s"def ${instName}")
    out.inc
  }

  override def instanceAttrName(instName: String): String = instName

  override def instanceFooter: Unit = classFooter()

  override def instanceCheckCacheAndReturn(instName: String): Unit = {
    out.puts(s"return @${instName} if @${instName}")
  }

  override def instanceReturn(instName: String): Unit = {
    out.puts(s"@${instName}")
  }

  override def instanceCalculate(instName: String, value: expr): Unit = {
    out.puts(s"@${instanceAttrName(instName)} = ${expression(value)}")
  }
}
