package io.kaitai.struct.translators

import io.kaitai.struct.{ImportList, Utils}
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.Identifier
import io.kaitai.struct.languages.PerlCompiler

class PerlTranslator(provider: TypeProvider, importList: ImportList) extends BaseTranslator(provider) {
  // http://perldoc.perl.org/perlrebackslash.html#Character-Escapes
  override val asciiCharQuoteMap: Map[Char, String] = Map(
    '\t' -> "\\t",
    '\n' -> "\\n",
    '\r' -> "\\r",
    '"' -> "\\\"",
    '\\' -> "\\\\",

    // Perl double-quoted interpolation variables
    '$' -> "\\$",
    '@' -> "\\@",
    '%' -> "\\%",

    '\u0007' -> "\\a",
    '\f' -> "\\f",
    '\u001b' -> "\\e",
    '\b' -> "\\b"

    // \v is available since 5.10, but according to documentation
    // it's used for a class of "vertical tabulation" characters,
    // not a single character
  )

  override def strLiteralUnicode(code: Char): String =
    "\\N{U+%04x}".format(code.toInt)

  override def numericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Div) =>
        s"int(${translate(left)} / ${translate(right)})"
      case _ =>
        super.numericBinOp(left, op, right)
    }
  }

  override def doBoolLiteral(n: Boolean): String = if (n) "1" else "0"

  override def doArrayLiteral(t: DataType, value: Seq[Ast.expr]): String =
    "[" + value.map((v) => translate(v)).mkString(", ") + "]"

  override def doByteArrayLiteral(arr: Seq[Byte]): String =
    s"pack('C*', (${arr.map(_ & 0xff).mkString(", ")}))"
  override def doByteArrayNonLiteral(elts: Seq[Ast.expr]): String =
    s"pack('C*', (${elts.map(translate).mkString(", ")}))"

  override def anyField(value: Ast.expr, attrName: String): String =
    s"${translate(value)}->${doName(attrName)}"

  override def doLocalName(s: String) = {
    s match {
      case "_" | "_on" => "$" + s
      case Identifier.INDEX => doName(s)
      case _ => s"$$self->${doName(s)}"
    }
  }

  override def doName(s: String) = {
    s match {
      case Identifier.ITERATOR => "$_"
      case Identifier.ITERATOR2 => "$_buf"
      case Identifier.INDEX => "$i"
      case _ => s"$s()"
    }
  }

  override def doEnumByLabel(enumType: List[String], label: String): String = {
    val enumClass = PerlCompiler.types2class(enumType.init)
    val enumClassWithScope = if (enumClass.isEmpty) "" else s"$enumClass::"
    val enumName = Utils.upperUnderscoreCase(enumType.last)
    s"$$$enumClassWithScope${enumName}_${Utils.upperUnderscoreCase(label)}"
  }
  override def doEnumById(enumTypeAbs: List[String], id: String): String =
    // Just an integer, without any casts / resolutions - one would have to look up constants manually
    id

  def enumClass(enumTypeAbs: List[String]): String = {
    val enumTypeRel = Utils.relClass(enumTypeAbs, provider.nowClass.name)
    enumTypeRel.map((x) => Utils.upperCamelCase(x)).mkString(".")
  }

  override def doStrCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr) = {
    val opStr = op match {
      case Ast.cmpop.Eq => "eq"
      case Ast.cmpop.NotEq => "ne"
      case Ast.cmpop.Lt => "lt"
      case Ast.cmpop.LtE => "le"
      case Ast.cmpop.Gt => "gt"
      case Ast.cmpop.GtE => "ge"
    }
    s"${translate(left)} $opStr ${translate(right)}"
  }

  override def doBytesCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): String =
    doStrCompareOp(left, op, right)

  override def arraySubscript(container: Ast.expr, idx: Ast.expr): String =
    s"@{${translate(container)}}[${translate(idx)}]"
  override def doIfExp(condition: Ast.expr, ifTrue: Ast.expr, ifFalse: Ast.expr): String =
    s"(${translate(condition)} ? ${translate(ifTrue)} : ${translate(ifFalse)})"

  // Predefined methods of various types
  override def strConcat(left: Ast.expr, right: Ast.expr): String =
    s"${translate(left)} . ${translate(right)}"
  override def strToInt(s: Ast.expr, base: Ast.expr): String = {
    val baseStr = translate(base)
    baseStr match {
      case "8" =>
        s"oct(${translate(s)})"
      case "10" =>
        translate(s)
      case "16" =>
        s"hex(${translate(s)})"
      case _ => throw new UnsupportedOperationException(baseStr)
    }
  }
  override def enumToInt(v: Ast.expr, et: EnumType): String =
    translate(v)
  override def boolToInt(v: Ast.expr): String =
    translate(v)
  override def floatToInt(v: Ast.expr): String =
    s"int(${translate(v)})"
  override def intToStr(i: Ast.expr, base: Ast.expr): String = {
    val baseStr = translate(base)
    val format = baseStr match {
      case "8" =>
        s"%o"
      case "10" =>
        s"%d"
      case "16" =>
        s"0x%X"
      case _ => throw new UnsupportedOperationException(baseStr)
    }

    s"sprintf('$format', ${translate(i)})"
  }
  override def bytesToStr(bytesExpr: String, encoding: Ast.expr): String = {
    importList.add("Encode")
    s"Encode::decode(${translate(encoding)}, $bytesExpr)"
  }
  override def bytesLength(b: Ast.expr): String =
    strLength(b)

  override def strLength(value: Ast.expr): String =
    s"length(${translate(value)})"
  override def strReverse(value: Ast.expr): String =
    s"scalar(reverse(${translate(value)}))"
  override def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): String =
    s"${translate(s)}[${translate(from)}:${translate(to)}]"

  override def arrayFirst(a: Ast.expr): String =
    s"@{${translate(a)}}[0]"
  override def arrayLast(a: Ast.expr): String =
    s"@{${translate(a)}}[-1]"
  override def arraySize(a: Ast.expr): String =
    s"scalar(@{${translate(a)}})"
  override def arrayMin(a: Ast.expr): String = {
    val funcName = detectType(a).asInstanceOf[ArrayType].elType match {
      case _: StrType => "minstr"
      case _ => "min"
    }
    importList.add("List::Util")
    s"List::Util::$funcName(@{${translate(a)}})"
  }
  override def arrayMax(a: Ast.expr): String = {
    val funcName = detectType(a).asInstanceOf[ArrayType].elType match {
      case _: StrType => "maxstr"
      case _ => "max"
    }
    importList.add("List::Util")
    s"List::Util::$funcName(@{${translate(a)}})"
  }

  override def kaitaiStreamSize(value: Ast.expr): String =
    s"${translate(value)}->size()"
}
