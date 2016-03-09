package io.kaitai.struct.languages

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format._
import io.kaitai.struct.translators.{BaseTranslator, RubyTranslator, TypeProvider}

class RubyCompiler(verbose: Boolean, outDir: String) extends LanguageCompiler(verbose, outDir) with UpperCamelCaseClasses with EveryReadIsExpression {
  override def getTranslator(tp: TypeProvider): BaseTranslator = new RubyTranslator(tp)

  override def outFileName(topClassName: String): String = s"$topClassName.rb"
  override def indent: String = "  "

  override def fileHeader(sourceFileName: String, topClassName: String): Unit = {
    out.puts(s"# This file was generated from '$sourceFileName' with kaitai_struct compiler")
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

  override def classConstructorHeader(name: String, parentClassName: String, rootClassName: String): Unit = {
    out.puts("def initialize(io, parent = nil, root = self)")
    out.inc
    out.puts("@_io = io")
    out.puts("@_parent = parent")
    out.puts("@_root = root")
  }

  override def classConstructorFooter: Unit = classFooter()

  override def attributeDeclaration(attrName: String, attrType: BaseType): Unit = {}

  override def attributeReader(attrName: String, attrType: BaseType): Unit = {
    out.puts(s"attr_reader :$attrName")
  }

  override def attrFixedContentsParse(attrName: String, contents: Array[Byte]): Unit = {
    out.puts(s"@$attrName = ensure_fixed_contents(${contents.length}, [${contents.map(x => x.toInt & 0xff).mkString(", ")}])")
  }

  override def attrProcess(proc: ProcessExpr, varSrc: String, varDest: String): Unit = {
    out.puts(proc match {
      case ProcessXor(xorValue) =>
        s"@$varDest = @$varSrc.bytes.map { |x| (x ^ (${expression(xorValue)})) }.pack('C*')"
    })
  }

  override def normalIO: String = "@_io"

  override def allocateIO(varName: String, rep: RepeatSpec): String = {
    val args = rep match {
      case RepeatEos | RepeatExpr(_) => s"@$varName.last"
      case NoRepeat => s"@$varName"
    }

    out.puts(s"io = StringIO.new($args)")
    "io"
  }

  override def seek(io: String, pos: Ast.expr): Unit = {
    out.puts(s"$io.seek(${expression(pos)})")
  }

  override def condIfHeader(expr: Ast.expr): Unit = {
    out.puts(s"if ${expression(expr)}")
    out.inc
  }
  override def condIfFooter(expr: Ast.expr): Unit = {
    out.dec
    out.puts("end")
  }

  override def condRepeatEosHeader(id: String, io: String, dataType: BaseType): Unit = {
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

  override def condRepeatExprHeader(id: String, io: String, dataType: BaseType, repeatExpr: expr): Unit = {
    out.puts(s"@$id = Array.new(${expression(repeatExpr)}) {")
    out.inc
  }
  override def handleAssignmentRepeatExpr(id: String, expr: String): Unit = out.puts(expr)
  override def condRepeatExprFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def handleAssignmentSimple(id: String, expr: String): Unit =
    out.puts(s"@$id = $expr")

  override def parseExpr(dataType: BaseType, io: String): String = {
    dataType match {
      case t: IntType =>
        s"read_${t.apiCall}"
      // Aw, crap, can't use interpolated strings here: https://issues.scala-lang.org/browse/SI-6476
      case StrByteLimitType(bs, encoding) =>
        s"read_str_byte_limit(${expression(bs)}, " + '"' + encoding + "\")"
      case StrEosType(encoding) =>
        "read_str_eos(\"" + encoding + "\")"
      case StrZType(encoding, terminator, include, consume, eosError) =>
        "read_strz(\"" + encoding + '"' + s", $terminator, $include, $consume, $eosError)"
      case EnumType(enumName, t) =>
        s"${value2Const(enumName)}[read_${t.apiCall}]"

      case BytesLimitType(size, _) =>
        s"$io.read(${expression(size)})"
      case BytesEosType(_) =>
        s"$io.read"
      case t: UserType =>
        s"${type2class(t.name)}.new($io, self, @_root)"
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
}
