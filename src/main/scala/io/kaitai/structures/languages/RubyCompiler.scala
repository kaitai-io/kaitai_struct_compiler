package io.kaitai.structures.languages

import io.kaitai.structures.LanguageOutputWriter
import io.kaitai.structures.format.AttrSpec

class RubyCompiler(outFileName: String) extends LanguageCompiler with UpperCamelCaseClasses {
  val out = new LanguageOutputWriter(outFileName, "  ")

  override def fileHeader(sourceFileName: String, topClassName: String): Unit = {
    out.puts(s"# This file was generated from '${sourceFileName}' with kaitai-structures compiler")
    out.puts
    out.puts("require 'kaitai_structures'")
    out.puts
  }

  override def classHeader(name: String): Unit = {
    out.puts(s"class ${type2class(name)}")
    out.inc
    out.puts("include KaitaiStructures")
    out.puts

    // Helper method to read from local file
    out.puts("def self.from_file(filename)")
    out.inc
    out.puts("self.new(File.open(filename, 'rb:ASCII-8BIT'))")
    out.dec
    out.puts("end")
    out.puts
  }

  override def classFooter: Unit = {
    out.dec
    out.puts("end")
  }

  override def classConstructorHeader(name: String): Unit = {
    out.puts("def initialize(io, parent = nil)")
    out.inc
    out.puts("@_io = io")
    out.puts("@_parent = parent")
  }

  override def classConstructorFooter: Unit = classFooter

  override def attributeDeclaration(attrName: String, attrType: String): Unit = {}

  override def attributeReader(attrName: String, attrType: String): Unit = {
    out.puts(s"attr_reader :${attrName}")
  }

  override def attrFixedContentsParse(attrName: String, contents: Array[Byte]): Unit = {
    out.puts(s"@${attrName} = ensure_fixed_contents(${contents.size}, [${contents.mkString(", ")}])")
  }

  override def attrNoTypeWithSize(varName: String, size: String) {
    out.puts(s"this.${varName} = @_io.read(${size})")
  }

  override def attrNoTypeWithSizeEos(varName: String) {
    out.puts(s"this.${varName} = @_io.read)")
  }

  override def attrStdTypeParse(attr: AttrSpec, endian: Option[String]): Unit = {
    handleAssignment(attr, stdTypeParseExpr(attr, endian), normalIO)
  }

  override def attrUserTypeParse(attr: AttrSpec, io: String): Unit = {
    handleAssignment(attr, s"${type2class(attr.dataType)}.new(${io}, self)", io)
  }

  override def normalIO: String = "@_io"

  override def allocateIO(varName: String): String = {
    out.puts(s"io = StringIO.new(@${varName})")
    "io"
  }

  def handleAssignment(attr: AttrSpec, expr: String, io: String): Unit = {
    attr.repeat match {
      case Some("eos") =>
        out.puts(s"@${attr.id} = []")
        out.puts(s"while not ${io}.eof?")
        out.inc
        out.puts(s"@${attr.id} << ${expr}")
        out.dec
        out.puts("end")
      case Some("expr") =>
//        repeat_expr = node['repeat-expr']
//        raise FormatError.new(self, node, "repeat: expr, but no repeat-expr value given") unless repeat_expr
//        @out.puts "@#{node_id} = Array.new(#{repeat_expr}) {"
//        @out.inc
//        @out.puts "#{class_name}.new(#{io_name})"
//        @out.dec
//        @out.puts "}"
      case None => out.puts(s"@${attr.id} = ${expr}")
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
      case _ => "foo"
    }
  }

  override def instanceHeader(instName: String, dataType: String): Unit = {
    out.puts(s"def ${instName}")
    out.inc
  }

  override def instanceFooter: Unit = classFooter
}
