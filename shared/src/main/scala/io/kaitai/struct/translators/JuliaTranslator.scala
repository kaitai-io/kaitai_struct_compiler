package io.kaitai.struct.translators

import io.kaitai.struct.{ImportList, Utils}
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.Identifier
import io.kaitai.struct.languages.{JuliaCompiler, RubyCompiler}

class JuliaTranslator(provider: TypeProvider, importList: ImportList) extends BaseTranslator(provider) {
  override def numericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr): String = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Div) =>
        s"${translate(left)} ÷ ${translate(right)}"
      case _ =>
        super.numericBinOp(left, op, right)
    }
  }

  override def binOp(op: Ast.operator): String = {
    op match {
      case Ast.operator.Add => "+"
      case Ast.operator.Sub => "-"
      case Ast.operator.Mult => "*"
      case Ast.operator.Div => "/"
      case Ast.operator.Mod => "%"
      case Ast.operator.BitAnd => "&"
      case Ast.operator.BitOr => "|"
      case Ast.operator.BitXor => "⊻"
      case Ast.operator.LShift => "<<"
      case Ast.operator.RShift => ">>"
    }
  }

//  override def doStringLiteral(s: String): String = super.doStringLiteral(s)
//  override def doBoolLiteral(n: Boolean): String = if (n) "True" else "False"

  /**
    * https://docs.Julia.org/2.7/reference/lexical_analysis.html#string-literals
    * https://docs.Julia.org/3.6/reference/lexical_analysis.html#string-and-bytes-literals
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
    s"Vector{UInt8}(${super.doByteArrayLiteral(arr)})"
  override def doByteArrayNonLiteral(elts: Seq[Ast.expr]): String = {
//    importList.add("import struct")
//    s"struct.pack('${elts.length}b', ${elts.map(translate).mkString(", ")})"
    s"Vector{UInt8}([${elts.map(translate).mkString(", ")}])"
  }

  override def doLocalName(s: String): String = {
    s match {
      case Identifier.ITERATOR => "_it"
      case Identifier.INDEX => "i"
      case _ => s"this.${doName(s)}"
    }
  }
  override def doName(s: String): String = {
    s match {
      case Identifier.ITERATOR => "_it"
      case _ => s
    }
  }

  override def doInternalName(id: Identifier): String =
    s"this.${JuliaCompiler.publicMemberName(id)}"

  override def doEnumByLabel(enumTypeAbs: List[String], label: String): String =
    s"${JuliaCompiler.enumToStr(enumTypeAbs, label)}"
  override def doEnumById(enumTypeAbs: List[String], id: String): String =
    s"${JuliaCompiler.types2class(enumTypeAbs)}($id)"

  override def arraySubscript(container: Ast.expr, idx: Ast.expr): String =
    s"${translate(container)}[${translate(idx)}]"
  override def doIfExp(condition: Ast.expr, ifTrue: Ast.expr, ifFalse: Ast.expr): String =
    s"(${translate(condition)} ? ${translate(ifTrue)} : ${translate(ifFalse)})"

  // Predefined methods of various types
  override def strToInt(s: Ast.expr, base: Ast.expr): String = {
    val baseStr = translate(base)
    s"parse(Int64, ${translate(s)}, $baseStr)"
  }
  override def enumToInt(v: Ast.expr, et: EnumType): String =
    s"Int(${translate(v)})"
  override def boolToInt(v: Ast.expr): String =
    s"Int(${translate(v)})"
  override def floatToInt(v: Ast.expr): String =
    s"trunc(${translate(v)})"
  override def intToStr(i: Ast.expr, base: Ast.expr): String = {
    val baseStr = translate(base)
    s"string(${translate(i)}, base = $baseStr)"
  }
  override def bytesToStr(bytesExpr: String, encoding: Ast.expr): String = {
    importList.add("using StringEncodings")
    s"decode(($bytesExpr), ${translate(encoding)})"
  }

  override def strConcat(left: Ast.expr, right: Ast.expr): String = s"${translate(left)} * ${translate(right)}"

  override def bytesLength(value: Ast.expr): String =
    s"length(${translate(value)})"
  override def bytesSubscript(container: Ast.expr, idx: Ast.expr): String =
    s"${JuliaCompiler.kstreamName}.byte_array_index(${translate(container)}, ${translate(idx)})"
  override def bytesFirst(a: Ast.expr): String =
    bytesSubscript(a, Ast.expr.IntNum(0))
  override def bytesLast(a: Ast.expr): String =
    bytesSubscript(a, Ast.expr.IntNum(-1))
  override def bytesMin(b: Ast.expr): String =
    s"${JuliaCompiler.kstreamName}.byte_array_min(${translate(b)})"
  override def bytesMax(b: Ast.expr): String =
    s"${JuliaCompiler.kstreamName}.byte_array_max(${translate(b)})"


  override def strLength(value: Ast.expr): String =
    s"length(${translate(value)})"
  override def strReverse(value: Ast.expr): String =
    s"reverse(${translate(value)})"
  override def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): String =
    s"(${translate(s)})[${translate(from)}:${translate(to)}]"

  override def arrayFirst(a: Ast.expr): String =
    s"${translate(a)}[1]"
  override def arrayLast(a: Ast.expr): String =
    s"${translate(a)}[end]"
  override def arraySize(a: Ast.expr): String =
    s"size(${translate(a)}, 1)"
  override def arrayMin(a: Ast.expr): String =
    s"minimum(${translate(a)})"
  override def arrayMax(a: Ast.expr): String =
    s"maximum(${translate(a)})"

  override def kaitaiStreamSize(value: Ast.expr): String =
    s"size(${translate(value)})"
  override def kaitaiStreamEof(value: Ast.expr): String =
    s"iseof(${translate(value)})"
  override def kaitaiStreamPos(value: Ast.expr): String =
    s"pos(${translate(value)})"
}
