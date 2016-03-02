package io.kaitai.struct.languages

import io.kaitai.struct.Utils
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{AttrSpec, ProcessExpr, ProcessXor}
import io.kaitai.struct.translators.{JavaTranslator, BaseTranslator, TypeProvider}

class JavaCompiler(verbose: Boolean, outDir: String, destPackage: String = "") extends LanguageCompiler(verbose, outDir) with UpperCamelCaseClasses with EveryReadIsExpression {
  override def getTranslator(tp: TypeProvider): BaseTranslator = new JavaTranslator(tp)

  override def outFileName(topClassName: String): String = s"${type2class(topClassName)}.java"
  override def indent: String = "    "

  override def fileHeader(sourceFileName: String, topClassName: String): Unit = {
    out.puts(s"// This file was generated from '${sourceFileName}' with kaitai-struct compiler")
    if (!destPackage.isEmpty) {
      out.puts
      out.puts(s"package ${destPackage};")
    }
    out.puts
    out.puts("import io.kaitai.struct.KaitaiStruct;")
    out.puts("import io.kaitai.struct.KaitaiStream;")
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

  override def classFooter(name: String): Unit = {
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

  override def classConstructorFooter: Unit = classFooter(null)

  override def attributeDeclaration(attrName: String, attrType: String, isArray: Boolean): Unit = {
    out.puts(s"private ${kaitaiType2JavaType(attrType, isArray)} ${lowerCamelCase(attrName)};")
  }

  override def attributeReader(attrName: String, attrType: String, isArray: Boolean): Unit = {
    out.puts(s"public ${kaitaiType2JavaType(attrType, isArray)} ${lowerCamelCase(attrName)}() { return ${lowerCamelCase(attrName)}; }")
  }

  override def attrFixedContentsParse(attrName: String, contents: Array[Byte]): Unit = {
    out.puts(s"this.${lowerCamelCase(attrName)} = _io.ensureFixedContents(${contents.length}, new byte[] { ${contents.mkString(", ")} });")
  }

  override def attrNoTypeWithSize(varName: String, size: Ast.expr): Unit = {
    out.puts(s"this.${lowerCamelCase(varName)} = _io.readBytes(${expression(size)});")
  }

  override def attrNoTypeWithSizeEos(varName: String): Unit = {
    out.puts(s"this.${lowerCamelCase(varName)} = _io.readBytesFull();")
  }

  override def attrUserTypeParse(id: String, attr: AttrSpec, io: String): Unit = {
    handleAssignment(id, attr, s"new ${type2class(attr.dataType)}(${io}, this)", io)
  }

  override def attrProcess(proc: ProcessExpr, varSrc: String, varDest: String): Unit = {
    proc match {
      case ProcessXor(xorValue) =>
        out.puts(s"this.$varDest = new byte[this.$varSrc.length];")
        out.puts(s"for (int i = 0; i < this.$varSrc.length; i++) {")
        out.inc
        out.puts(s"this.$varDest[i] = (byte) (this.$varSrc[i] ^ (${expression(xorValue)}));")
        out.dec
        out.puts("}")
    }
  }

  override def normalIO: String = "_io"

  override def allocateIO(varName: String): String = {
    val ioName = s"_io_${lowerCamelCase(varName)}"
    out.puts(s"KaitaiStream ${ioName} = new KaitaiStream(${lowerCamelCase(varName)});")
    ioName
  }

  override def seek(io: String, pos: Ast.expr): Unit = {
    out.puts(s"${io}.seek(${expression(pos)});")
  }

  override def handleAssignment(id: String, attr: AttrSpec, expr: String, io: String): Unit = {
    if (attr.ifExpr.isDefined) {
      out.puts(s"if (${expression(attr.ifExpr.get)}) {")
      out.inc
    }

    attr.repeat match {
      case Some("eos") =>
        out.puts(s"${id} = new ${kaitaiType2JavaType(attr.dataType, true)}();")
        out.puts(s"while (!${io}.isEof()) {")
        out.inc
        out.puts(s"${id}.add(${expr});")
        out.dec
        out.puts("}")
      case Some("expr") =>
        attr.repeatExpr match {
          case Some(repeatExpr) =>
            out.puts(s"${id} = new ${kaitaiType2JavaType(attr.dataType, true)}((int) (${expression(repeatExpr)}));")
            out.puts(s"for (int i = 0; i < ${expression(repeatExpr)}; i++) {")
            out.inc
            out.puts(s"${id}.add(${expr});")
            out.dec
            out.puts("}")
          case None =>
            throw new RuntimeException("repeat: expr, but no repeat-expr value given")
        }
      case None =>
        out.puts(s"this.${lowerCamelCase(id)} = ${expr};")
    }

    if (attr.ifExpr.isDefined) {
      out.dec
      out.puts("}")
    }
  }

  override def stdTypeParseExpr(attr: AttrSpec, endian: Option[String]): String = {
    attr.dataType match {
      case "u1" | "s1" | "u2le" | "u2be" | "u4le" | "u4be" | "u8le" | "u8be" | "s2le" | "s2be" | "s4le" | "s4be" | "s8le" | "s8be" =>
        s"_io.read${Utils.capitalize(attr.dataType)}()"
      case "u2" | "u4" | "u8" | "s2" | "s4" | "s8" =>
        endian match {
          case Some(e) => s"_io.read${Utils.capitalize(attr.dataType)}${e}()"
          case None => throw new RuntimeException(s"type ${attr.dataType}: unable to parse with no default endianess defined")
        }
      case null => throw new RuntimeException("should never happen")

      // Aw, crap, can't use interpolated strings here: https://issues.scala-lang.org/browse/SI-6476
      case "str" =>
        ((attr.size, attr.sizeEos)) match {
          case (Some(bs: Ast.expr), false) =>
            s"_io.readStrByteLimit(${expression(bs)}, " + '"' + attr.encoding.get + "\")"
          case (None, true) =>
            "_io.readStrEos(\"" + attr.encoding.get + "\")"
          case (None, false) =>
            throw new RuntimeException("type str: either \"byte_size\" or \"size_eos\" must be specified")
          case (Some(_), true) =>
            throw new RuntimeException("type str: only one of \"byte_size\" or \"size_eos\" must be specified")
        }
      case "strz" =>
        "_io.readStrz(\"" + attr.encoding.get + '"' + s", ${attr.terminator}, ${attr.include}, ${attr.consume}, ${attr.eosError})"
    }
  }

  override def instanceDeclaration(attrName: String, attrType: String, isArray: Boolean): Unit = {
    out.puts(s"private ${kaitaiType2JavaTypeBoxed(attrType, isArray)} ${lowerCamelCase(attrName)};")
  }

  override def instanceHeader(className: String, instName: String, dataType: String, isArray: Boolean): Unit = {
    out.puts(s"public ${kaitaiType2JavaTypeBoxed(dataType, isArray)} ${lowerCamelCase(instName)}() throws IOException {")
    out.inc
  }

  override def instanceAttrName(instName: String): String = instName

  override def instanceFooter: Unit = classConstructorFooter

  override def instanceCheckCacheAndReturn(instName: String): Unit = {
    out.puts(s"if (${lowerCamelCase(instName)} != null)")
    out.inc
    instanceReturn(instName)
    out.dec
  }

  override def instanceReturn(instName: String): Unit = {
    out.puts(s"return ${lowerCamelCase(instName)};")
  }

  override def instanceCalculate(instName: String, value: Ast.expr): Unit = {
    out.puts(s"${lowerCamelCase(instName)} = ${expression(value)};")
  }

  def kaitaiType2JavaType(attrType: String, isArray: Boolean): String = {
    if (isArray) {
      kaitaiType2JavaTypeBoxed(attrType, true)
    } else {
      kaitaiType2JavaTypePrim(attrType)
    }
  }

  /**
    * Determine Java data type corresponding to a KS data type. A "primitive" type (i.e. "int", "long", etc) will
    * be returned if possible.
    *
    * @param attrType KS data type
    * @return Java data type
    */
  def kaitaiType2JavaTypePrim(attrType: String): String = {
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

  /**
    * Determine Java data type corresponding to a KS data type. A non-primitive type (i.e. "Integer", "Long", etc) will
    * be returned, to be used when proper objects should be used.
    *
    * @param attrType KS data type
    * @return Java data type
    */
  def kaitaiType2JavaTypeBoxed(attrType: String, isArray: Boolean = false): String = {
    val r = attrType match {
      case "u1" => "Integer"
      case "u2" | "u2le" | "u2be" => "Integer"
      case "u4" | "u4le" | "u4be" => "Long"
      case "u8" | "u8le" | "u8be" => "Long"

      case "s1" => "Byte"
      case "s2" | "s2le" | "s2be" => "Short"
      case "s4" | "s4le" | "s4be" => "Integer"
      case "s8" | "s8le" | "s8be" => "Long"

      case "str" | "strz" => "String"

      case null => "byte[]"

      case _ => type2class(attrType)
    }

    if (isArray) {
      s"ArrayList<$r>"
    } else {
      r
    }
  }

  def lowerCamelCase(s: String): String = {
    if (s.startsWith("_raw_")) {
      return "_raw_" + Utils.lowerCamelCase(s.substring("_raw_".length))
    } else {
      Utils.lowerCamelCase(s)
    }
  }
}
