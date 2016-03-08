package io.kaitai.struct.languages

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format.{AttrLikeSpec, AttrSpec, ProcessExpr, ProcessXor}
import io.kaitai.struct.translators.{BaseTranslator, RubyTranslator, TypeProvider}

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

  override def classConstructorHeader(name: String, parentClassName: String, rootClassName: String): Unit = {
    out.puts("def initialize(io, parent = nil, root = self)")
    out.inc
    out.puts("@_io = io")
    out.puts("@_parent = parent")
    out.puts("@_root = root")
  }

  override def classConstructorFooter: Unit = classFooter()

  override def attributeDeclaration(attrName: String, attrType: BaseType, isArray: Boolean): Unit = {}

  override def attributeReader(attrName: String, attrType: BaseType, isArray: Boolean): Unit = {
    out.puts(s"attr_reader :${attrName}")
  }

  override def attrFixedContentsParse(attrName: String, contents: Array[Byte]): Unit = {
    out.puts(s"@${attrName} = ensure_fixed_contents(${contents.size}, [${contents.map(x => x.toInt & 0xff).mkString(", ")}])")
  }

  override def attrUserTypeParse(id: String, attrType: UserType, attr: AttrLikeSpec, io: String): Unit =
    handleAssignment(id, attr, s"${type2class(attrType.name)}.new(${io}, self, @_root)", io)

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

  override def condIfHeader(expr: Ast.expr): Unit = {
    out.puts(s"if ${expression(expr)}")
    out.inc
  }
  override def condIfFooter(expr: Ast.expr): Unit = {
    out.dec
    out.puts("end")
  }

  override def condRepeatEosHeader(id: String, io: String, dataType: BaseType): Unit = {
    out.puts(s"@${id} = []")
    out.puts(s"while not ${io}.eof?")
    out.inc
  }
  override def handleAssignmentRepeatEos(id: String, expr: String): Unit =
    out.puts(s"@${id} << ${expr}")
  override def condRepeatEosFooter: Unit = {
    out.dec
    out.puts("end")
  }

  override def condRepeatExprHeader(id: String, io: String, dataType: BaseType, repeatExpr: expr): Unit = {
    out.puts(s"@${id} = Array.new(${expression(repeatExpr)}) {")
    out.inc
  }
  override def handleAssignmentRepeatExpr(id: String, expr: String): Unit = out.puts(expr)
  override def condRepeatExprFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def handleAssignmentSimple(id: String, expr: String): Unit =
    out.puts(s"@${id} = ${expr}")

  override def stdTypeParseExpr(attr: AttrLikeSpec): String = {
    attr.dataType match {
      case t: IntType =>
        s"read_${t.apiCall}"
      // Aw, crap, can't use interpolated strings here: https://issues.scala-lang.org/browse/SI-6476
      case StrByteLimitType(bs, encoding) =>
        s"read_str_byte_limit(${expression(bs)}, " + '"' + encoding + "\")"
      case StrEosType(encoding) =>
        "read_str_eos(\"" + encoding + "\")"
      case StrZType(encoding, terminator, include, consume, eosError) =>
        "read_strz(\"" + encoding + '"' + s", ${terminator}, ${include}, ${consume}, ${eosError})"
      case EnumType(enumName, t) =>
        s"${value2Const(enumName)}[read_${t.apiCall}]"
    }
  }

  override def noTypeWithSizeExpr(size: expr): String = s"@_io.read(${expression(size)})"

  override def noTypeWithSizeEosExpr: String = s"@_io.read"

  override def instanceHeader(className: String, instName: String, dataType: BaseType, isArray: Boolean): Unit = {
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
