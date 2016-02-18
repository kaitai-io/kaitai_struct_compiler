package io.kaitai.structures.languages

import io.kaitai.structures.LanguageOutputWriter
import io.kaitai.structures.format.AttrSpec

class JavaCompiler(outDir: String, destPackage: String = "") extends LanguageCompiler with UpperCamelCaseClasses {
  var out: LanguageOutputWriter = null

  override def fileHeader(sourceFileName: String, topClassName: String): Unit = {
    out = new LanguageOutputWriter(s"${outDir}/${type2class(topClassName)}.java", "    ")

    out.puts(s"// This file was generated from '${sourceFileName}' with kaitai-structures compiler")
    if (!destPackage.isEmpty) {
      out.puts
      out.puts(s"package ${destPackage};")
    }
    out.puts
    out.puts("import io.kaitai.structures.KaitaiStruct;")
    out.puts("import io.kaitai.structures.KaitaiStream;")
    out.puts
    out.puts("import java.io.IOException;")
    out.puts("import java.util.ArrayList;")
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

  override def attributeDeclaration(attrName: String, attrType: String, isArray: Boolean): Unit = {
    out.puts(s"private ${kaitaiType2JavaType(attrType, isArray)} ${lowerCamelCase(attrName)};")
  }

  override def attributeReader(attrName: String, attrType: String, isArray: Boolean): Unit = {
    out.puts(s"public ${kaitaiType2JavaType(attrType, isArray)} ${lowerCamelCase(attrName)}() { return ${lowerCamelCase(attrName)}; }")
  }

  override def attrFixedContentsParse(attrName: String, contents: Array[Byte]): Unit = {
    out.puts(s"this.${lowerCamelCase(attrName)} = _io.ensureFixedContents(${contents.size}, new byte[] { ${contents.mkString(", ")} });")
  }

  override def attrNoTypeWithSize(varName: String, size: String): Unit = {
    out.puts(s"this.${lowerCamelCase(varName)} = _io.readBytes(${expression2Java(size)});")
  }

  override def attrNoTypeWithSizeEos(varName: String): Unit = {
    out.puts(s"this.${lowerCamelCase(varName)} = _io.readBytesFull();")
  }

  override def attrStdTypeParse(attr: AttrSpec, endian: Option[String]): Unit = {
    handleAssignment(attr, stdTypeParseExpr(attr, endian), normalIO)
  }

  override def attrUserTypeParse(attr: AttrSpec, io: String): Unit = {
    handleAssignment(attr, s"new ${type2class(attr.dataType)}(${io}, this)", io)
  }

  override def normalIO: String = "_io"

  override def allocateIO(varName: String): String = {
    val ioName = s"_io_${lowerCamelCase(varName)}"
    out.puts(s"KaitaiStream ${ioName} = new KaitaiStream(${lowerCamelCase(varName)});")
    ioName
  }

  def handleAssignment(attr: AttrSpec, expr: String, io: String): Unit = {
    attr.repeat match {
      case Some("eos") =>
        out.puts(s"${attr.id} = new ${kaitaiType2JavaType(attr.dataType, true)}();")
        out.puts(s"while (!${io}.isEof()) {")
        out.inc
        out.puts(s"${attr.id}.add(${expr});")
        out.dec
        out.puts("}")
      case Some("expr") =>
        attr.repeatExpr match {
          case Some(repeatExpr) =>
            out.puts(s"${attr.id} = new ${kaitaiType2JavaType(attr.dataType, true)}((int) (${expression2Java(repeatExpr)}));")
            out.puts(s"for (int i = 0; i < ${expression2Java(repeatExpr)}; i++) {")
            out.inc
            out.puts(s"${attr.id}.set(i, ${expr});")
            out.dec
            out.puts("}")
          case None =>
            throw new RuntimeException("repeat: expr, but no repeat-expr value given")
        }
      case None =>
        out.puts(s"this.${lowerCamelCase(attr.id)} = ${expr};")
    }
  }

  def stdTypeParseExpr(attr: AttrSpec, endian: Option[String]): String = {
    attr.dataType match {
      case "u1" | "s1" | "u2le" | "u2be" | "u4le" | "u4be" | "u8le" | "u8be" | "s2le" | "s2be" | "s4le" | "s4be" | "s8le" | "s8be"  =>
        s"_io.read${capitalize(attr.dataType)}()"
      case "u2" | "u4" | "u8" | "s2" | "s4" | "s8" =>
        endian match {
          case Some(e) => s"_io.read${capitalize(attr.dataType)}${e}()"
          case None => throw new RuntimeException(s"type ${attr.dataType}: unable to parse with no default endianess defined")
        }
      case null => throw new RuntimeException("should never happen")
      case "str" =>
        // Aw, crap, can't use interpolated strings here: https://issues.scala-lang.org/browse/SI-6476
        ((attr.byteSize, attr.sizeEos)) match {
          case (Some(bs: String), false) =>
            s"_io.readStrByteLimit(${bs}, " + '"' + attr.encoding.get + "\")"
          case (None, true) =>
            "_io.readStrEos(\"" + attr.encoding.get + "\")"
          case (None, false) =>
            throw new RuntimeException("type str: either \"byte_size\" or \"size_eos\" must be specified")
          case (Some(_), true) =>
            throw new RuntimeException("type str: only one of \"byte_size\" or \"size_eos\" must be specified")
        }
      case "strz" =>
        "foo"
    }
  }

  override def instanceHeader(instName: String, dataType: String, isArray: Boolean): Unit = {
    out.puts(s"public ${kaitaiType2JavaType(dataType, isArray)} ${instName}() throws IOException {")
    out.inc
  }

  override def instanceFooter: Unit = classFooter

  override def instanceCheckCacheAndReturn(instName: String): Unit = {
    out.puts(s"if (${lowerCamelCase(instName)} != null)")
    out.inc
    instanceReturn(instName)
    out.dec
  }

  override def instanceReturn(instName: String): Unit = {
    out.puts(s"return ${lowerCamelCase(instName)};")
  }

  def kaitaiType2JavaType(attrType: String, isArray: Boolean): String = {
    if (isArray) {
      val primType = attrType match {
        case "u1" => "Integer"
        case "u2" | "u2le" | "u2be" => "Integer"
        case "u4" | "u4le" | "u4be" => "Long"
        case "u8" | "u8le" | "u8be" => "Long"

        case "s1" => "Byte"
        case "s2" | "s2le" | "s2be" => "Short"
        case "s4" | "s4le" | "s4be" => "Int"
        case "s8" | "s8le" | "s8be" => "Long"

        case "str" | "strz" => "String"

        case null => "byte[]"

        case _ => type2class(attrType)
      }
      s"ArrayList<${primType}>"
    } else {
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
