package io.kaitai.struct.translators

import io.kaitai.struct.{ExternalEnum, ImportList, RuntimeConfig, Utils}
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{EnumSpec, Identifier}
import io.kaitai.struct.languages.{PythonCompiler, RubyCompiler}

class PythonTranslator(provider: TypeProvider, importList: ImportList, config: RuntimeConfig) extends BaseTranslator(provider) {
  override def genericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr, extPrec: Int) = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Div) =>
        genericBinOpStr(left, op, "//", right, extPrec)
      case _ =>
        super.genericBinOp(left, op, right, extPrec)
    }
  }

  override def doStringLiteral(s: String): String = "u" + super.doStringLiteral(s)
  override def doBoolLiteral(n: Boolean): String = if (n) "True" else "False"

  /**
    * https://docs.python.org/2.7/reference/lexical_analysis.html#string-literals
    * https://docs.python.org/3.6/reference/lexical_analysis.html#string-and-bytes-literals
    */
  override val asciiCharQuoteMap: Map[Char, String] = Map(
    '\t' -> "\\t",
    '\n' -> "\\n",
    '\r' -> "\\r",
    '"' -> "\\\"",
    '\\' -> "\\\\",

    '\u0007' -> "\\a",
    '\f' -> "\\f",
    '\u000b' -> "\\v",
    '\b' -> "\\b"
  )

  override def doByteArrayLiteral(arr: Seq[Byte]): String =
    "b\"" + Utils.hexEscapeByteArray(arr) + "\""
  override def doByteArrayNonLiteral(elts: Seq[Ast.expr]): String = {
    importList.add("import struct")
    // Use `unsigned char` representation (see https://docs.python.org/3/library/struct.html#struct-format-strings)
    s"struct.pack('${elts.length}B', ${elts.map(translate).mkString(", ")})"
  }

  override def doLocalName(s: String) = {
    s match {
      case Identifier.ITERATOR => "_"
      case Identifier.INDEX => "i"
      case _ => s"self.${doName(s)}"
    }
  }
  override def doName(s: String) =
    s
  override def doInternalName(id: Identifier): String =
    PythonCompiler.privateMemberName(id)

  override def doEnumByLabel(enumSpec: EnumSpec, label: String): String = {
    val isExternal = enumSpec.isExternal(provider.nowClass)
    if (isExternal) {
      PythonCompiler.externalTypeDeclaration(ExternalEnum(enumSpec), importList, config)
    }
    s"${PythonCompiler.types2class(enumSpec.name, isExternal)}.$label"
  }
  override def doEnumById(enumSpec: EnumSpec, id: String): String =
    s"${PythonCompiler.kstreamName}.resolve_enum(${PythonCompiler.types2class(enumSpec.name, enumSpec.isExternal(provider.nowClass))}, $id)"

  override def booleanOp(op: Ast.boolop) = op match {
    case Ast.boolop.Or => "or"
    case Ast.boolop.And => "and"
  }

  override def unaryOp(op: Ast.unaryop) = op match {
    case Ast.unaryop.Not => "not "
    case _ => super.unaryOp(op)
  }

  override def arraySubscript(container: Ast.expr, idx: Ast.expr): String =
    s"${translate(container)}[${translate(idx)}]"
  override def doIfExp(condition: Ast.expr, ifTrue: Ast.expr, ifFalse: Ast.expr): String =
    s"(${translate(ifTrue)} if ${translate(condition)} else ${translate(ifFalse)})"

  // Predefined methods of various types
  override def strToInt(s: Ast.expr, base: Ast.expr): String = {
    val baseStr = translate(base)
    val add = baseStr match {
      case "10" => ""
      case _ => s", $baseStr"
    }
    s"int(${translate(s)}$add)"
  }
  override def enumToInt(v: Ast.expr, et: EnumType): String =
    s"int(${translate(v)})"
  override def boolToInt(v: Ast.expr): String =
    s"int(${translate(v)})"
  override def floatToInt(v: Ast.expr): String =
    s"int(${translate(v)})"
  override def intToStr(i: Ast.expr): String =
    s"str(${translate(i)})"
  override def bytesToStr(bytesExpr: String, encoding: String): String =
    s"""($bytesExpr).decode(${doStringLiteral(encoding)})"""

  override def bytesLength(value: Ast.expr): String =
    s"len(${translate(value)})"
  override def bytesSubscript(container: Ast.expr, idx: Ast.expr): String =
    s"${PythonCompiler.kstreamName}.byte_array_index(${translate(container)}, ${translate(idx)})"
  override def bytesFirst(a: Ast.expr): String =
    bytesSubscript(a, Ast.expr.IntNum(0))
  override def bytesLast(a: Ast.expr): String =
    bytesSubscript(a, Ast.expr.IntNum(-1))
  override def bytesMin(b: Ast.expr): String =
    s"${PythonCompiler.kstreamName}.byte_array_min(${translate(b)})"
  override def bytesMax(b: Ast.expr): String =
    s"${PythonCompiler.kstreamName}.byte_array_max(${translate(b)})"


  override def strLength(value: Ast.expr): String =
    s"len(${translate(value)})"
  override def strReverse(value: Ast.expr): String =
    s"(${translate(value)})[::-1]"
  override def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): String =
    s"${translate(s, METHOD_PRECEDENCE)}[${translate(from)}:${translate(to)}]"

  override def arrayFirst(a: Ast.expr): String =
    s"${translate(a)}[0]"
  override def arrayLast(a: Ast.expr): String =
    s"${translate(a)}[-1]"
  override def arraySize(a: Ast.expr): String =
    s"len(${translate(a)})"
  override def arrayMin(a: Ast.expr): String =
    s"min(${translate(a)})"
  override def arrayMax(a: Ast.expr): String =
    s"max(${translate(a)})"

  override def kaitaiStreamSize(value: Ast.expr): String =
    s"${translate(value, METHOD_PRECEDENCE)}.size()"
  override def kaitaiStreamEof(value: Ast.expr): String =
    s"${translate(value, METHOD_PRECEDENCE)}.is_eof()"
  override def kaitaiStreamPos(value: Ast.expr): String =
    s"${translate(value, METHOD_PRECEDENCE)}.pos()"
}
