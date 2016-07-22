package io.kaitai.struct.languages

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format._
import io.kaitai.struct.languages.JavaScriptCompiler.{KaitaiStreamAPI, RuntimeAPI}
import io.kaitai.struct.translators.{BaseTranslator, JavaScriptTranslator, TypeProvider}
import io.kaitai.struct.{LanguageOutputWriter, Utils}

class JavaScriptCompiler(verbose: Boolean, out: LanguageOutputWriter, api: RuntimeAPI = KaitaiStreamAPI)
  extends LanguageCompiler(verbose, out)
    with EveryReadIsExpression
    with NoNeedForFullClassPath {
  import JavaScriptCompiler._

  override def getTranslator(tp: TypeProvider): BaseTranslator = new JavaScriptTranslator(tp)

  override def fileHeader(topClassName: String): Unit = {
    out.puts(s"// $headerComment")
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

  override def attributeDeclaration(attrName: String, attrType: BaseType, condSpec: ConditionalSpec): Unit = {}

  override def attributeReader(attrName: String, attrType: BaseType): Unit = {}

  override def attrFixedContentsParse(attrName: String, contents: Array[Byte]): Unit = {
    out.puts(s"this.${lowerCamelCase(attrName)} = _io.ensureFixedContents(${contents.length}, new byte[] { ${contents.mkString(", ")} });")
  }

  override def attrProcess(proc: ProcessExpr, varSrc: String, varDest: String): Unit = {
    proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "processXorOne"
          case _: BytesType => "processXorMany"
        }
        out.puts(s"${privateMemberName(varDest)} = KaitaiStream.$procName(${privateMemberName(varSrc)}, ${expression(xorValue)});")
      case ProcessZlib =>
        out.puts(s"this.$varDest = KaitaiStream.processZlib(this.$varSrc);")
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        out.puts(s"this.$varDest = KaitaiStream.processRotateLeft(this.$varSrc, $expr, 1);")
    }
  }

  override def normalIO: String = "this._io"

  override def allocateIO(varName: String, rep: RepeatSpec): String = {
    val langName = lowerCamelCase(varName)

    val ioName = s"this._io_$langName"

    val args = rep match {
      case RepeatEos => s"this.$langName[this.$langName.length - 1]"
      case RepeatExpr(_) => s"this.$langName[i]"
      case NoRepeat => s"this.$langName"
    }

    out.puts(s"$ioName = new KaitaiStream($args);")
    ioName
  }

  override def useIO(ioEx: expr): String = {
    out.puts(s"var io = ${expression(ioEx)};")
    "io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"var _pos = $io.pos;")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io.seek(${expression(pos)});")

  override def popPos(io: String): Unit =
    out.puts(s"$io.seek(_pos);")

  override def condIfHeader(expr: expr): Unit = {
    out.puts(s"if (${expression(expr)}) {")
    out.inc
  }

  override def condIfFooter(expr: expr): Unit = {
    out.dec
    out.puts("}")
  }

  override def condRepeatEosHeader(id: String, io: String, dataType: BaseType, needRaw: Boolean): Unit = {
    if (needRaw)
      out.puts(s"this._raw_${lowerCamelCase(id)} = [];")
    out.puts(s"this.${lowerCamelCase(id)} = [];")
    out.puts(s"while (!$io.isEof()) {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: String, expr: String): Unit = {
    out.puts(s"this.${lowerCamelCase(id)}.push($expr);")
  }

  override def condRepeatEosFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def condRepeatExprHeader(id: String, io: String, dataType: BaseType, needRaw: Boolean, repeatExpr: expr): Unit = {
    if (needRaw)
      out.puts(s"this._raw_${lowerCamelCase(id)} = new Array(${expression(repeatExpr)});")
    out.puts(s"this.${lowerCamelCase(id)} = new Array(${expression(repeatExpr)});")
    out.puts(s"for (var i = 0; i < ${expression(repeatExpr)}; i++) {")
    out.inc
  }

  override def handleAssignmentRepeatExpr(id: String, expr: String): Unit = {
    out.puts(s"this.${lowerCamelCase(id)}[i] = $expr;")
  }

  override def condRepeatExprFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def handleAssignmentSimple(id: String, expr: String): Unit = {
    out.puts(s"this.${lowerCamelCase(id)} = $expr;")
  }

  override def parseExpr(dataType: BaseType, io: String): String = {
    dataType match {
      case t: IntType =>
        s"$io.read${Utils.capitalize(t.apiCall)}()"

      // Aw, crap, can't use interpolated strings here: https://issues.scala-lang.org/browse/SI-6476
      case StrByteLimitType(bs, encoding) =>
        s"$io.readStrByteLimit(${expression(bs)}, " + '"' + encoding + "\")"
      case StrEosType(encoding) =>
        io + ".readStrEos(\"" + encoding + "\")"
      case StrZType(encoding, terminator, include, consume, eosError) =>
        io + ".readStrz(\"" + encoding + '"' + s", $terminator, $include, $consume, $eosError)"
      case EnumType(enumName, t) =>
        // Just an integer, without any casts / resolutions - one would have to look up constants manually
        s"$io.read${Utils.capitalize(t.apiCall)}()"

      case BytesLimitType(size, _) =>
        s"$io.readBytes(${expression(size)})"
      case BytesEosType(_) =>
        s"$io.readBytesFull()"
      case t: UserType =>
        s"new ${type2class(t.name.last)}($io, this, this._root)"
    }
  }

  override def instanceHeader(className: String, instName: String, dataType: BaseType): Unit = {
    out.puts(s"Object.defineProperty(${type2class(className)}.prototype, '${lowerCamelCase(instName)}', {")
    out.inc
    out.puts("get: function() {")
    out.inc
  }

  override def instanceAttrName(instName: String) = s"_m_$instName"

  override def instanceFooter: Unit = {
    out.dec
    out.puts("}")
    out.dec
    out.puts("});")
  }

  override def instanceCheckCacheAndReturn(instName: String): Unit = {
    out.puts(s"if (this.${lowerCamelCase(instanceAttrName(instName))} !== undefined)")
    out.inc
    instanceReturn(instName)
    out.dec
  }

  override def instanceReturn(instName: String): Unit = {
    out.puts(s"return this.${lowerCamelCase(instanceAttrName(instName))};")
  }

  override def instanceCalculate(instName: String, value: Ast.expr): Unit = {
    out.puts(s"this.${lowerCamelCase(instanceAttrName(instName))} = ${expression(value)};")
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
        "_raw_" + lowerCamelCase(s.substring("_raw_".length))
      } else if (s.startsWith("_m_")) {
        "_m_" + lowerCamelCase(s.substring("_m_".length))
      } else {
        throw new RuntimeException(s"internal error: don't know how to make '$s' a field name")
      }
    } else {
      Utils.lowerCamelCase(s)
    }
  }

  override def privateMemberName(ksName: String): String = s"this.$ksName"
}

object JavaScriptCompiler extends LanguageCompilerStatic with UpperCamelCaseClasses {
  override def indent: String = "  "
  override def outFileName(topClassName: String): String = s"${type2class(topClassName)}.js"

  sealed abstract class RuntimeAPI
  case object DataStreamAPI extends RuntimeAPI
  case object KaitaiStreamAPI extends RuntimeAPI
}
