package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format.Identifier
import io.kaitai.struct.languages.PHPCompiler
import io.kaitai.struct.{RuntimeConfig, Utils}

class PHPTranslator(provider: TypeProvider, config: RuntimeConfig) extends BaseTranslator(provider)
    with MinSignedIntegers {
  override def doIntLiteral(n: BigInt): String = {
    super.doIntLiteral(if (n >= Long.MinValue && n <= Utils.MAX_UINT64) {
      n.toLong // output unsigned 64-bit integers as signed (otherwise we would get a float and
               // lose precision)
    } else {
      n
    })
  }

  override def doByteArrayLiteral(arr: Seq[Byte]): String =
    "\"" + Utils.hexEscapeByteArray(arr) + "\""
  override def doByteArrayNonLiteral(elts: Seq[Ast.expr]): String =
    s"pack('C*', ${elts.map(translate).mkString(", ")})"

  // http://php.net/manual/en/language.types.string.php#language.types.string.syntax.double
  override val asciiCharQuoteMap: Map[Char, String] = Map(
    '\t' -> "\\t",
    '\n' -> "\\n",
    '\r' -> "\\r",
    '"' -> "\\\"",
    '\\' -> "\\\\",

    // allowed and required to not trigger variable interpolation
    '$' -> "\\$",

    '\f' -> "\\f",
    '\u000b' -> "\\v",
    '\u001b' -> "\\e"
  )

  override def strLiteralUnicode(code: Char): String =
    "\\u{%x}".format(code.toInt)

  override def numericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Div) =>
        s"intval(${translate(left)} / ${translate(right)})"
      case (_: IntType, _: IntType, Ast.operator.Mod) =>
        s"${PHPCompiler.kstreamName}::mod(${translate(left)}, ${translate(right)})"
      case _ =>
        super.numericBinOp(left, op, right)
    }
  }

  override def anyField(value: expr, attrName: String): String =
    s"${translate(value)}->${doName(attrName)}"

  override def doLocalName(s: String) = {
    s match {
      case Identifier.ITERATOR => "$_"
      case Identifier.ITERATOR2 => "$_buf"
      case Identifier.INDEX => "$i"
      case _ => s"$$this->${doName(s)}"
    }
  }

  override def doName(s: String) = s"${Utils.lowerCamelCase(s)}()"

  override def doEnumByLabel(enumTypeAbs: List[String], label: String): String = {
    val enumClass = types2classAbs(enumTypeAbs)
    s"$enumClass::${Utils.upperUnderscoreCase(label)}"
  }
  override def doEnumById(enumTypeAbs: List[String], id: String) =
    // Just an integer, without any casts / resolutions - one would have to look up constants manually
    id

  override def arraySubscript(container: expr, idx: expr): String =
    s"${translate(container)}[${translate(idx)}]"
  override def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String =
    s"(${translate(condition)} ? ${translate(ifTrue)} : ${translate(ifFalse)})"

  // Predefined methods of various types
  override def strConcat(left: Ast.expr, right: Ast.expr): String =
    s"${translate(left)} . ${translate(right)}"

  override def strToInt(s: expr, base: expr): String =
    s"intval(${translate(s)}, ${translate(base)})"

  override def enumToInt(v: expr, et: EnumType): String =
    translate(v)

  override def boolToInt(v: expr): String =
    s"intval(${translate(v)})"

  override def floatToInt(v: expr): String =
    s"intval(${translate(v)})"

  override def intToStr(i: expr, base: expr): String = {
    val baseStr = translate(base)
    baseStr match {
      case "10" =>
        s"strval(${translate(i)})"
      case _ =>
        s"base_convert(strval(${translate(i)}), 10, $baseStr)"
    }
  }
  override def bytesToStr(bytesExpr: String, encoding: Ast.expr): String =
    s"${PHPCompiler.kstreamName}::bytesToStr($bytesExpr, ${translate(encoding)})"

  override def bytesLength(b: Ast.expr): String =
    s"strlen(${translate(b)})"
  override def bytesSubscript(container: Ast.expr, idx: Ast.expr): String =
    s"ord(${translate(container)}[${translate(idx)}])"
  override def bytesFirst(b: Ast.expr): String =
    s"ord(${translate(b)}[0])"
  override def bytesLast(b: Ast.expr): String =
    s"ord(${translate(b)}[${bytesLength(b)} - 1])"
  override def bytesMin(b: Ast.expr): String =
    s"${PHPCompiler.kstreamName}::byteArrayMin(${translate(b)})"
  override def bytesMax(b: Ast.expr): String =
    s"${PHPCompiler.kstreamName}::byteArrayMax(${translate(b)})"

  override def strLength(s: expr): String =
    s"strlen(${translate(s)})"
  override def strReverse(s: expr): String =
    s"strrev(${translate(s)})"
  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"${PHPCompiler.kstreamName}::substring(${translate(s)}, ${translate(from)}, ${translate(to)})"

  override def arrayFirst(a: expr): String =
    s"${translate(a)}[0]"
  override def arrayLast(a: expr): String = {
    // For huge debate on efficiency of PHP last element of array methods, see:
    // http://stackoverflow.com/a/41795859/487064
    val v = translate(a)
    s"$v[count($v) - 1]"
  }
  override def arraySize(a: expr): String =
    s"count(${translate(a)})"
  override def arrayMin(a: Ast.expr): String =
    s"min(${translate(a)})"
  override def arrayMax(a: Ast.expr): String =
    s"max(${translate(a)})"

  val namespaceRef = if (config.phpNamespace.isEmpty) {
    ""
  } else {
    "\\" + config.phpNamespace
  }

  def types2classAbs(names: List[String]) =
    names match {
      case List("kaitai_struct") => PHPCompiler.kstructName
      case _ =>
        namespaceRef + "\\" + PHPCompiler.types2classRel(names)
    }
}
