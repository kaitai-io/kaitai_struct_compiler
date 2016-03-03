package io.kaitai.struct.languages

import io.kaitai.struct.Utils
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{AttrSpec, ProcessExpr, ProcessXor}
import io.kaitai.struct.languages.JavaScriptCompiler.{DataStreamAPI, KaitaiStreamAPI, RuntimeAPI}
import io.kaitai.struct.translators.{BaseTranslator, TypeProvider, JavaScriptTranslator}

object JavaScriptCompiler {
  sealed abstract class RuntimeAPI
  case object DataStreamAPI extends RuntimeAPI
  case object KaitaiStreamAPI extends RuntimeAPI
}

class JavaScriptCompiler(verbose: Boolean, outDir: String, api: RuntimeAPI = KaitaiStreamAPI) extends LanguageCompiler(verbose, outDir) with UpperCamelCaseClasses with EveryReadIsExpression {

  override def getTranslator(tp: TypeProvider): BaseTranslator = new JavaScriptTranslator(tp)

  override def outFileName(topClassName: String): String = s"${type2class(topClassName)}.js"
  override def indent: String = "  "

  override def fileHeader(sourceFileName: String, topClassName: String): Unit = {
    out.puts(s"// This file was generated from '${sourceFileName}' with kaitai-struct compiler")
    out.puts
  }

  override def fileFooter(name: String): Unit = {
    out.puts
    out.puts("// Export for amd environments")
    out.puts("if (typeof define === 'function' && define.amd) {")
    out.inc
    out.puts(s"define('${type2class(name)}', [], function() {")
    out.inc
    out.puts(s"return ${type2class(name)};")
    out.dec
    out.puts("});")
    out.dec
    out.puts("}")

    out.puts

    out.puts("// Export for CommonJS")
    out.puts("if (typeof module === 'object' && module && module.exports) {")
    out.inc
    out.puts(s"module.exports = ${type2class(name)};")
    out.dec
    out.puts("}")
  }

  override def classHeader(name: String): Unit = {
    out.puts(s"var ${type2class(name)} = (function() {")
    out.inc
  }

  override def classFooter(name: String): Unit = {
    out.puts
    out.puts(s"return ${type2class(name)};")
    out.dec
    out.puts("})();")
  }

  override def classConstructorHeader(name: String): Unit = {
    out.puts(s"function ${type2class(name)}(_io, _parent) {")
    out.inc
    out.puts("if (_parent == null)")
    out.inc
    out.puts("_parent = null;")
    out.dec
    out.puts("this._io = _io;")
    out.puts("this._parent = _parent;")
    out.puts
  }

  override def classConstructorFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def attributeDeclaration(attrName: String, attrType: String, isArray: Boolean): Unit = {}

  override def attributeReader(attrName: String, attrType: String, isArray: Boolean): Unit = {}

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
        out.puts(s"this.$varDest = new Uint8Array(this.$varSrc.length);")
        out.puts(s"for (var i = 0; i < this.$varSrc.length; i++) {")
        out.inc
        out.puts(s"this.$varDest[i] = this.$varSrc[i] ^ (${expression(xorValue)});")
        out.dec
        out.puts("}")
    }
  }

  override def normalIO: String = "this._io"

  override def allocateIO(varName: String): String = {
    val ioName = s"this._io_${lowerCamelCase(varName)}"
    out.puts(s"${ioName} = new KaitaiStream(this.${lowerCamelCase(varName)});")
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
        out.puts(s"this.${id} = [];")
        out.puts(s"while (!${io}.isEof()) {")
        out.inc
        out.puts(s"this.${id}.push(${expr});")
        out.dec
        out.puts("}")
      case Some("expr") =>
        attr.repeatExpr match {
          case Some(repeatExpr) =>
            out.puts(s"this.${id} = new Array(${expression(repeatExpr)});")
            out.puts(s"for (var i = 0; i < ${expression(repeatExpr)}; i++) {")
            out.inc
            out.puts(s"this.${id}[i] = ${expr};")
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
    api match {
      case DataStreamAPI => stdTypeDataStream(attr, endian)
      case KaitaiStreamAPI => stdTypeKaitaiStream(attr, endian)
    }
  }

  def stdTypeDataStream(attr: AttrSpec, endian: Option[String]): String = {
    val exactType = attr.dataType match {
      case "u2" | "u4" | "u8" | "s2" | "s4" | "s8" =>
        endian match {
          case Some(e) => s"${attr.dataType}${e}"
          case None => throw new RuntimeException(s"type ${attr.dataType}: unable to parse with no default endianess defined")
        }
      case t => t
    }

    exactType match {
      case "u1" => "_io.readUint8()"
      case "s1" => "_io.readSint8()"
      case "u2le" => "_io.readUint16(1)"
      case "u2be" => "_io.readUint16()"
      case "u4le" => "_io.readUint32(1)"
      case "u4be" => "_io.readUint32()"
      // "u8le" | "u8be"
      case "s2le" => "_io.readInt16(1)"
      case "s2be" => "_io.readInt16()"
      case "s4le" => "_io.readInt32(1)"
      case "s4be" => "_io.readInt32()"
      // "s8le" | "s8be"
    }
  }

  def stdTypeKaitaiStream(attr: AttrSpec, endian: Option[String]): String = {
    attr.dataType match {
      case "u1" | "s1" | "u2le" | "u2be" | "u4le" | "u4be" | "u8le" | "u8be" | "s2le" | "s2be" | "s4le" | "s4be" | "s8le" | "s8be" =>
        s"_io.read${Utils.capitalize(attr.dataType)}()"
      case "u2" | "u4" | "u8" | "s2" | "s4" | "s8" =>
        endian match {
          case Some(e) => s"this._io.read${Utils.capitalize(attr.dataType)}${e}()"
          case None => throw new RuntimeException(s"type ${attr.dataType}: unable to parse with no default endianess defined")
        }
      case null => throw new RuntimeException("should never happen")

      // Aw, crap, can't use interpolated strings here: https://issues.scala-lang.org/browse/SI-6476
      case "str" =>
        ((attr.size, attr.sizeEos)) match {
          case (Some(bs: Ast.expr), false) =>
            s"this._io.readStrByteLimit(${expression(bs)}, " + '"' + attr.encoding.get + "\")"
          case (None, true) =>
            "this._io.readStrEos(\"" + attr.encoding.get + "\")"
          case (None, false) =>
            throw new RuntimeException("type str: either \"size\" or \"size-eos\" must be specified")
          case (Some(_), true) =>
            throw new RuntimeException("type str: only one of \"size\" or \"size-eos\" must be specified")
        }
      case "strz" =>
        "this._io.readStrz(\"" + attr.encoding.get + '"' + s", ${attr.terminator}, ${attr.include}, ${attr.consume}, ${attr.eosError})"
    }
  }

  override def instanceHeader(className: String, instName: String, dataType: String, isArray: Boolean): Unit = {
    out.puts(s"Object.defineProperty(${type2class(className)}.prototype, '${lowerCamelCase(instName)}', {")
    out.inc
    out.puts("get: function() {")
    out.inc
  }

  override def instanceAttrName(instName: String) = s"_m_${instName}"

  override def instanceFooter: Unit = {
    out.dec
    out.puts("}")
    out.dec
    out.puts("});")
  }

  override def instanceCheckCacheAndReturn(instName: String): Unit = {
    out.puts(s"if (this.${instanceAttrName(instName)} !== undefined)")
    out.inc
    instanceReturn(instName)
    out.dec
  }

  override def instanceReturn(instName: String): Unit = {
    out.puts(s"return this.${instanceAttrName(instName)};")
  }

  override def instanceCalculate(instName: String, value: Ast.expr): Unit = {
    out.puts(s"this.${instanceAttrName(instName)} = ${expression(value)};")
  }

  def lowerCamelCase(s: String): String = {
    if (s.charAt(0) == '_') {
      if (s.startsWith("_raw_")) {
        return "_raw_" + Utils.lowerCamelCase(s.substring("_raw_".length))
      } else if (s.startsWith("_m_")) {
        return "_m_" + Utils.lowerCamelCase(s.substring("_m_".length))
      } else {
        throw new RuntimeException(s"internal error: don't know how to make '$s' a field name")
      }
    } else {
      Utils.lowerCamelCase(s)
    }
  }
}
