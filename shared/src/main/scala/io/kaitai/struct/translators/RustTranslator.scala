package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format.{EnumSpec, Identifier}
import io.kaitai.struct.languages.RustCompiler
import io.kaitai.struct.{RuntimeConfig, Utils}

class RustTranslator(provider: TypeProvider, config: RuntimeConfig) extends BaseTranslator(provider) {
  override def doByteArrayLiteral(arr: Seq[Byte]): String =
    "vec!([" + arr.map((x) =>
    	     "%0#2x".format(x & 0xff)
    ).mkString(", ") + "])"
  override def doByteArrayNonLiteral(elts: Seq[Ast.expr]): String =
    s"pack('C*', ${elts.map(translate).mkString(", ")})"

  override val asciiCharQuoteMap: Map[Char, String] = Map(
    '\t' -> "\\t",
    '\n' -> "\\n",
    '\r' -> "\\r",
    '"' -> "\\\"",
    '\\' -> "\\\\"
  )

  /**
    * Hex escapes in form `\xHH` in Rust allows only codes in the range 0x00 - 0x7f.
    *
    * @see https://doc.rust-lang.org/reference/tokens.html#examples
    * @param code character code to represent
    * @return string literal representation of given code
    */
  override def strLiteralUnicode(code: Char): String =
    "\\u{%x}".format(code.toInt)

  override def doLocalName(s: String) = {
    s match {
      case Identifier.ITERATOR => "tmpa"
      case Identifier.ITERATOR2 => "tmpb"
      case Identifier.INDEX => "i"
      case _ => s"self.${doName(s)}"
    }
  }

  override def doName(s: String) = s

  override def doEnumByLabel(enumSpec: EnumSpec, label: String): String = {
    val enumClass = types2classAbs(enumSpec.name)
    s"$enumClass::${Utils.upperUnderscoreCase(label)}"
  }
  override def doEnumById(enumSpec: EnumSpec, id: String): String =
    // Just an integer, without any casts / resolutions - one would have to look up constants manually
    id

  override def arraySubscript(container: expr, idx: expr): String =
    s"${translate(container)}[${translate(idx)}]"
  override def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String =
    "if " + translate(condition) + " { " + translate(ifTrue) + " } else { " + translate(ifFalse) + " }"

  // Predefined methods of various types
  override def strConcat(left: expr, right: expr, extPrec: Int) =
    "format!(\"{}{}\", " + translate(left) + ", " + translate(right) + ")"

  override def strToInt(s: expr, base: expr): String =
    translate(base) match {
      case "10" =>
        s"${translate(s)}.parse().unwrap()"
      case _ =>
        "panic!(\"Converting from string to int in base {} is unimplemented\", " + translate(base) + ")"
    }

  override def enumToInt(v: expr, et: EnumType): String =
    translate(v)

  override def boolToInt(v: expr): String =
    s"${translate(v, METHOD_PRECEDENCE)} as i32"

  override def floatToInt(v: expr): String =
    s"${translate(v, METHOD_PRECEDENCE)} as i32"

  override def intToStr(i: expr): String =
    s"${translate(i, METHOD_PRECEDENCE)}.to_string()"

  override def bytesToStr(bytesExpr: String, encoding: String): String =
    encoding match {
      case "ASCII" =>
        s"String::from_utf8_lossy($bytesExpr)"
      case _ =>
        "panic!(\"Unimplemented encoding for bytesToStr: {}\", \"" + encoding + "\")"
    }
  override def bytesLength(b: Ast.expr): String =
    s"${translate(b, METHOD_PRECEDENCE)}.len()"
  override def strLength(s: expr): String =
    s"${translate(s, METHOD_PRECEDENCE)}.len()"
  override def strReverse(s: expr): String =
    s"${translate(s, METHOD_PRECEDENCE)}.graphemes(true).rev().flat_map(|g| g.chars()).collect()"
  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"${translate(s, METHOD_PRECEDENCE)}.substring(${translate(from)}, ${translate(to)})"

  override def arrayFirst(a: expr): String =
    s"${translate(a, METHOD_PRECEDENCE)}.first()"
  override def arrayLast(a: expr): String =
    s"${translate(a, METHOD_PRECEDENCE)}.last()"
  override def arraySize(a: expr): String =
    s"${translate(a, METHOD_PRECEDENCE)}.len()"
  override def arrayMin(a: Ast.expr): String =
    s"${translate(a, METHOD_PRECEDENCE)}.iter().min()"
  override def arrayMax(a: Ast.expr): String =
    s"${translate(a, METHOD_PRECEDENCE)}.iter().max()"

  def types2classAbs(names: List[String]) =
    names match {
      case List("kaitai_struct") => RustCompiler.kstructName
      case _ => RustCompiler.types2classRel(names)
    }
}
