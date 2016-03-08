package io.kaitai.struct.languages

import io.kaitai.struct.Utils
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format.{AttrLikeSpec, AttrSpec, ProcessExpr, ProcessXor}
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

  override def classConstructorHeader(name: String, parentClassName: String, rootClassName: String): Unit = {
    out.puts(s"function ${type2class(name)}(_io, _parent, _root) {")
    out.inc
    out.puts("this._io = _io;")
    out.puts("this._parent = _parent;")
    out.puts("this._root = _root || this;")
    out.puts
  }

  override def classConstructorFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def attributeDeclaration(attrName: String, attrType: BaseType, isArray: Boolean): Unit = {}

  override def attributeReader(attrName: String, attrType: BaseType, isArray: Boolean): Unit = {}

  override def attrFixedContentsParse(attrName: String, contents: Array[Byte]): Unit = {
    out.puts(s"this.${lowerCamelCase(attrName)} = _io.ensureFixedContents(${contents.length}, new byte[] { ${contents.mkString(", ")} });")
  }

  override def attrUserTypeParse(id: String, attrType: UserType, attr: AttrLikeSpec, io: String): Unit =
    handleAssignment(id, attr, s"new ${type2class(attrType.name)}(${io}, this, this._root)", io)

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

  override def condIfHeader(expr: expr): Unit = {
    out.puts(s"if (${expression(expr)}) {")
    out.inc
  }

  override def condIfFooter(expr: expr): Unit = {
    out.dec
    out.puts("}")
  }

  override def condRepeatEosHeader(id: String, io: String, dataType: BaseType): Unit = {
    out.puts(s"this.${id} = [];")
    out.puts(s"while (!${io}.isEof()) {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: String, expr: String): Unit = {
    out.puts(s"this.${id}.push(${expr});")
  }

  override def condRepeatEosFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def condRepeatExprHeader(id: String, io: String, dataType: BaseType, repeatExpr: expr): Unit = {
    out.puts(s"this.${id} = new Array(${expression(repeatExpr)});")
    out.puts(s"for (var i = 0; i < ${expression(repeatExpr)}; i++) {")
    out.inc
  }

  override def handleAssignmentRepeatExpr(id: String, expr: String): Unit = {
    out.puts(s"this.${id}[i] = ${expr};")
  }

  override def condRepeatExprFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def handleAssignmentSimple(id: String, expr: String): Unit = {
    out.puts(s"this.${lowerCamelCase(id)} = ${expr};")
  }

  override def stdTypeParseExpr(attr: AttrLikeSpec, endian: Option[String]): String = {
    api match {
//      case DataStreamAPI => stdTypeDataStream(attr, endian)
      case KaitaiStreamAPI => stdTypeKaitaiStream(attr, endian)
    }
  }

  override def noTypeWithSizeExpr(size: expr): String = s"this._io.readBytes(${expression(size)})"

  override def noTypeWithSizeEosExpr: String = "this._io.readBytesFull()"

  def stdTypeKaitaiStream(attr: AttrLikeSpec, endian: Option[String]): String = {
    attr.dataType match {
      case t: IntType =>
        s"this._io.read${Utils.capitalize(t.apiCall)}()"

      // Aw, crap, can't use interpolated strings here: https://issues.scala-lang.org/browse/SI-6476
      case StrByteLimitType(bs, encoding) =>
        s"this._io.readStrByteLimit(${expression(bs)}, " + '"' + encoding + "\")"
      case StrEosType(encoding) =>
        "this._io.readStrEos(\"" + encoding + "\")"
      case StrZType(encoding, terminator, include, consume, eosError) =>
        "this._io.readStrz(\"" + encoding + '"' + s", ${terminator}, ${include}, ${consume}, ${eosError})"
      case EnumType(enumName, t) =>
        // Just an integer, without any casts / resolutions - one would have to look up constants manually
        s"this._io.read${Utils.capitalize(t.apiCall)}()"
    }
  }

  override def instanceHeader(className: String, instName: String, dataType: BaseType, isArray: Boolean): Unit = {
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

  override def enumDeclaration(curClass: String, enumName: String, enumColl: Map[Long, String]): Unit = {
    out.puts
    out.puts(s"${type2class(curClass)}.${type2class(enumName)} = Object.freeze({")
    out.inc
    enumColl.foreach { case (id, label) =>
      out.puts(s"${enumValue(enumName, label)}: $id,")
    }
    out.dec
    out.puts("});")
  }

  def enumValue(enumName: String, label: String) = label.toUpperCase

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
