package io.kaitai.structures.languages

import io.kaitai.structures.LanguageOutputWriter
import io.kaitai.structures.format.AttrSpec

class JavaCompiler(outDir: String) extends LanguageCompiler with UpperCamelCaseClasses {
  var out: LanguageOutputWriter = null

  override def fileHeader(sourceFileName: String, topClassName: String): Unit = {
    out = new LanguageOutputWriter(s"${outDir}/${type2class(topClassName)}.java", "    ")

    out.puts(s"// This file was generated from '${sourceFileName}' with kaitai-structures compiler")
    out.puts
    out.puts("import io.kaitai.structures.KaitaiStruct;")
    out.puts("import io.kaitai.structures.KaitaiStream;")
    out.puts
    out.puts("import java.io.IOException;")
    out.puts
  }

  override def classHeader(name: String): Unit = {
    val staticStr = if (out.indentLevel > 0) {
      "static "
    } else {
      ""
    }

    out.puts(s"public ${staticStr}class ${type2class(name)} extends KaitaiStruct {")
    out.inc

    out.puts(s"public static ${type2class(name)} fromFile(String fileName) throws IOException {")
    out.inc
    out.puts(s"return new ${type2class(name)}(new KaitaiStream(fileName));")
    out.dec
    out.puts("}")
  }

  override def classFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def classConstructorHeader(name: String): Unit = {
    out.puts
    out.puts(s"public ${type2class(name)}(KaitaiStream _io) throws IOException {")
    out.inc
    out.puts("super(_io);")
    out.puts("_parse();")
    out.dec
    out.puts("}")

    out.puts
    out.puts(s"public ${type2class(name)}(KaitaiStream _io, KaitaiStruct _parent) throws IOException {")
    out.inc
    out.puts("super(_io, _parent);")
    out.puts("_parse();")
    out.dec
    out.puts("}")

    out.puts("private void _parse() throws IOException {")
    out.inc
  }

  override def classConstructorFooter: Unit = classFooter

  override def attributeDeclaration(attrName: String, attrType: String): Unit = {
    out.puts(s"private ${kaitaiType2JavaType(attrType)} ${lowerCamelCase(attrName)};")
  }

  override def attributeReader(attrName: String, attrType: String): Unit = {
    out.puts(s"public ${kaitaiType2JavaType(attrType)} ${lowerCamelCase(attrName)}() { return ${lowerCamelCase(attrName)}; }")
  }

  override def attrFixedContentsParse(attrName: String, contents: Array[Byte]): Unit = {
    out.puts(s"this.${lowerCamelCase(attrName)} = _io.ensureFixedContents(${contents.size}, new byte[] { ${contents.mkString(", ")} });")
  }

  override def attrStdTypeParse(attr: AttrSpec, endian: Option[String]): Unit = {
    out.puts(s"this.${lowerCamelCase(attr.id)} = ${stdTypeParseExpr(attr, endian)};")
  }

  def stdTypeParseExpr(attr: AttrSpec, endian: Option[String]): String = {
    attr.dataType match {
      case "u1" | "s1" | "u2le" | "u2be" | "u4le" | "u4be" | "u8le" | "u8be" | "s2le" | "s2be" | "s4le" | "s4be" | "s8le" | "s8be"  =>
        s"_io.read${capitalize(attr.dataType)}()"
      case "u2" | "u4" | "u8" | "s2" | "s4" | "s8" =>
        endian match {
          case Some(e) => s"_io.read${capitalize(attr.dataType)}${e}()"
          case None => throw new RuntimeException(s"Unable to parse ${attr.dataType} with no default endianess defined")
        }
      case null =>
        val size = attr.size
        val sizeEos = attr.sizeEos
        if ((size != null) && (sizeEos)) {
          throw new scala.RuntimeException("not type and both size and size_eos specified")
        } else if (size != null) {
          s"_io.readBytes(${expression2Java(size)})"
        } else if (sizeEos) {
          "_io.readBytesFull()"
        } else {
          throw new scala.RuntimeException("not type and no size or size_eos specified")
        }
      case _ => "foo"
    }
  }

  override def instanceHeader(instName: String, dataType: String): Unit = {
    out.puts(s"public ${kaitaiType2JavaType(dataType)} ${instName}() {")
    out.inc
  }

  override def instanceFooter: Unit = classFooter

  def kaitaiType2JavaType(attrType: String): String = {
    attrType match {
      case "u1" => "int"
      case "u2" | "u2le" | "u2be" => "int"
      case "u4" | "u4le" | "u4be" => "long"
      case "u8" | "u8le" | "u8be" => "long"

      case "s1" => "byte"
      case "s2" | "s2le" | "s2be" => "short"
      case "s4" | "s4le" | "s4be" => "int"
      case "s8" | "s8le" | "s8be" => "long"

      case "str" | "strz" => "String"

      case null => "byte[]"

      case _ => type2class(attrType)
    }
  }

  def lowerCamelCase(s: String) = {
    val firstWord :: restWords = s.split("_").toList
    (firstWord :: restWords.map(capitalize)).mkString
  }

  def capitalize(x: String): String = {
    x.charAt(0).toUpper + x.substring(1)
  }

  val ReInt = "^\\d+$".r
  val ReLiteral = "^[A-Za-z_][A-Za-z0-9_]*$".r

  def expression2Java(s: String): String = {
    s match {
      case ReInt() => s
      case ReLiteral() => lowerCamelCase(s)
    }
  }
}
