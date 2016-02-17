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

  override def attrStdTypeParse(attr: AttrSpec, endian: Option[String]): Unit = {
    out.puts(s"@${attr.id} = ${stdTypeParseExpr(attr, endian)}")
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
      case _ => "foo"
    }
  }
}
