package io.kaitai.struct.translators

import io.kaitai.struct.Utils
import io.kaitai.struct.datatype.DataType.EnumType
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.Identifier
import io.kaitai.struct.languages.RubyCompiler

class RubyTranslator(provider: TypeProvider) extends BaseTranslator(provider)
  with ByteArraysAsTrueArrays[String] {
  override def doByteArrayLiteral(arr: Seq[Byte]): String =
    s"${super.doByteArrayLiteral(arr)}.pack('C*')"
  override def doByteArrayNonLiteral(elts: Seq[Ast.expr]): String =
    s"[${elts.map(translate).mkString(", ")}].pack('C*')"

  // https://github.com/ruby/ruby/blob/trunk/doc/syntax/literals.rdoc#strings
  // https://github.com/ruby/ruby/blob/trunk/string.c - see "rb_str_inspect"
  override val asciiCharQuoteMap: Map[Char, String] = Map(
    '\t' -> "\\t",
    '\n' -> "\\n",
    '\r' -> "\\r",
    '"' -> "\\\"",
    '\\' -> "\\\\",

    '#' -> "\\#",
    '\u0007' -> "\\a",
    '\f' -> "\\f",
    '\u000b' -> "\\v",
    '\u001b' -> "\\e",
    '\b' -> "\\b"
  )

  override def doName(s: String) = {
    s match {
      case Identifier.INDEX => "i" // FIXME: probably would clash with attribute named "i"
      case _ => s
    }
  }

  override def doEnumByLabel(enumTypeAbs: List[String], label: String): String =
    s":${enumTypeAbs.last}_$label"
  override def doEnumById(enumType: List[String], id: String): String =
    s"${RubyCompiler.kstreamName}::resolve_enum(${enumDirectMap(enumType)}, $id)"

  def enumDirectMap(enumTypeAndName: List[String]): String = {
    val enumTypeAbs = enumTypeAndName.dropRight(1)
    val enumTypeName = Utils.upperUnderscoreCase(enumTypeAndName.last)

    val enumTypeRel = Utils.relClass(enumTypeAbs, provider.nowClass.name)

    if (enumTypeRel.nonEmpty) {
      (enumTypeRel.map((x) => Utils.upperCamelCase(x)) ++ List(enumTypeName)).mkString("::")
    } else {
      enumTypeName
    }
  }

  def enumInverseMap(et: EnumType): String = {
    val enumTypeAndName = et.enumSpec.get.name
    val enumDirectMap = this.enumDirectMap(enumTypeAndName)
    val enumNameDirect = Utils.upperUnderscoreCase(enumTypeAndName.last)
    val enumNameInverse = RubyCompiler.inverseEnumName(enumNameDirect)

    enumDirectMap.replace(enumNameDirect, enumNameInverse)
  }

  override def arraySubscript(container: Ast.expr, idx: Ast.expr): String =
    s"${translate(container)}[${translate(idx)}]"
  override def doIfExp(condition: Ast.expr, ifTrue: Ast.expr, ifFalse: Ast.expr): String =
    s"(${translate(condition)} ? ${translate(ifTrue)} : ${translate(ifFalse)})"

  // Predefined methods of various types
  override def strToInt(s: Ast.expr, base: Ast.expr): String = {
    val baseStr = translate(base)
    translate(s) + ".to_i" + (baseStr match {
      case "10" => ""
      case _ => s"($baseStr)"
    })
  }
  override def enumToInt(v: Ast.expr, et: EnumType): String =
    s"${enumInverseMap(et)}[${translate(v)}]"
  override def floatToInt(v: Ast.expr): String =
    s"(${translate(v)}).to_i"
  override def intToStr(i: Ast.expr, base: Ast.expr): String =
    translate(i) + s".to_s(${translate(base)})"

  override def bytesToStr(bytesExpr: String, encoding: Ast.expr): String =
    s"($bytesExpr).force_encoding(${translate(encoding)})"
  override def bytesLength(b: Ast.expr): String =
    s"${translate(b)}.size"
  /**
    * Alternatives considered:
    *
    * * value[0].ord => 6341 => winner by performance
    * * value.bytes[0] => 8303
    */
  override def bytesSubscript(container: Ast.expr, idx: Ast.expr): String =
    s"${translate(container)}[${translate(idx)}].ord"
  override def bytesFirst(b: Ast.expr): String =
    s"${translate(b)}[0].ord"
  override def bytesLast(b: Ast.expr): String =
    s"${translate(b)}[-1].ord"
  override def bytesMin(b: Ast.expr): String =
    s"${translate(b)}.bytes.min"
  override def bytesMax(b: Ast.expr): String =
    s"${translate(b)}.bytes.max"

  override def strLength(s: Ast.expr): String =
    s"${translate(s)}.size"
  override def strReverse(s: Ast.expr): String =
    s"${translate(s)}.reverse"
  override def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): String =
    s"${translate(s)}[${translate(from)}..(${translate(to)} - 1)]"

  override def arrayFirst(a: Ast.expr): String =
    s"${translate(a)}.first"
  override def arrayLast(a: Ast.expr): String =
    s"${translate(a)}.last"
  override def arraySize(a: Ast.expr): String =
    s"${translate(a)}.length"
  override def arrayMin(a: Ast.expr): String =
    s"${translate(a)}.min"
  override def arrayMax(a: Ast.expr): String =
    s"${translate(a)}.max"

  override def kaitaiStreamEof(value: Ast.expr): String =
    s"${translate(value)}.eof?"
}
