package io.kaitai.struct.translators

import io.kaitai.struct.{ImportList, Utils}
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{EnumSpec, Identifier}
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

  override def genericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr, extPrec: Int) = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Div) =>
        s"int(${super.genericBinOp(left, op, right, 0)})"
      case _ =>
        super.genericBinOp(left, op, right, extPrec)
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
      case Identifier.SWITCH_ON => "$_on"
      case Identifier.ITERATOR | Identifier.INDEX => doName(s)
      case _ => s"$$self->${doName(s)}"
    }
  }

  override def doName(s: String) = {
    s match {
      case Identifier.ITERATOR => "$_it"
      case Identifier.ITERATOR2 => "$_buf"
      case Identifier.INDEX => "$i"
      case _ => s"$s()"
    }
  }

  override def doInternalName(id: Identifier): String =
    PerlCompiler.privateMemberName(id)

  override def doEnumByLabel(enumSpec: EnumSpec, label: String): String = {
    val isExternal = enumSpec.isExternal(provider.nowClass)
    if (isExternal) {
      importList.add(PerlCompiler.type2class(enumSpec.name.head))
    }
    val enumClass = PerlCompiler.types2class(enumSpec.name.init)
    val enumClassWithScope = if (enumClass.isEmpty) "" else s"$enumClass::"
    val enumName = Utils.upperUnderscoreCase(enumSpec.name.last)
    s"$$$enumClassWithScope${enumName}_${Utils.upperUnderscoreCase(label)}"
  }
  override def doEnumById(enumSpec: EnumSpec, id: String): String =
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
  override def strConcat(left: Ast.expr, right: Ast.expr, extPrec: Int) =
    genericBinOpStr(left, Ast.operator.Add, ".", right, extPrec)
  override def strToInt(s: Ast.expr, base: Ast.expr): String = {
    val baseStr = translate(base)
    baseStr match {
      case "2" =>
        s"oct('0b' . ${translate(s)})"
      case "8" =>
        s"oct(${translate(s)})"
      case "10" =>
        s"${translate(s)} + 0"
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
  override def intToStr(i: Ast.expr): String =
    s"sprintf('%d', ${translate(i)})"
  override def bytesToStr(bytesExpr: String, encoding: String): String = {
    importList.add("Encode")
    s"""Encode::decode(${doStringLiteral(encoding)}, $bytesExpr)"""
  }
  override def bytesLength(b: Ast.expr): String =
    strLength(b)
  override def bytesSubscript(container: Ast.expr, idx: Ast.expr): String =
    s"unpack('C', substr(${translate(container)}, ${translate(idx)}, 1))"
  override def bytesFirst(b: Ast.expr): String =
    s"unpack('C', substr(${translate(b)}, 0, 1))"
  override def bytesLast(b: Ast.expr): String =
    s"unpack('C', substr(${translate(b)}, -1, 1))"
  override def bytesMin(b: Ast.expr): String = {
    importList.add("List::Util")
    s"List::Util::min(unpack('C*', ${translate(b)}))"
  }
  override def bytesMax(b: Ast.expr): String = {
    importList.add("List::Util")
    s"List::Util::max(unpack('C*', ${translate(b)}))"
  }

  override def strLength(value: Ast.expr): String =
    s"length(${translate(value)})"
  override def strReverse(value: Ast.expr): String =
    s"scalar(reverse(${translate(value)}))"
  override def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): String =
    s"substr(${translate(s)}, ${translate(from)}, ${genericBinOp(to, Ast.operator.Sub, from, 0)})"

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
    s"${translate(value, METHOD_PRECEDENCE)}->size()"

  override def doInterpolatedStringLiteral(exprs: Seq[Ast.expr]): String =
    if (exprs.isEmpty) {
      doStringLiteral("")
    } else {
      exprs.map(anyToStr).mkString(" . ")
    }
}
