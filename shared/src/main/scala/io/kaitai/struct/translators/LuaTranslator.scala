package io.kaitai.struct.translators

import io.kaitai.struct.ImportList
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.format.Identifier
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.languages.LuaCompiler
import io.kaitai.struct.Utils

class LuaTranslator(provider: TypeProvider, importList: ImportList) extends BaseTranslator(provider)
    with MinSignedIntegers {
  override def doIntLiteral(n: BigInt): String = {
    if (n > Long.MaxValue && n <= Utils.MAX_UINT64) {
      // See <https://www.lua.org/manual/5.4/manual.html#3.1>:
      //
      // - "A numeric constant (...), if its value fits in an integer or it is a hexadecimal
      //   constant, it denotes an integer; otherwise (that is, a decimal integer numeral that
      //   overflows), it denotes a float."
      // - "Hexadecimal numerals with neither a radix point nor an exponent always denote an
      //   integer value; if the value overflows, it wraps around to fit into a valid integer."
      //
      // This is written only in the Lua 5.4 manual, but applies to Lua 5.3 too (experimentally
      // verified).
      "0x" + n.toString(16)
    } else {
      super.doIntLiteral(n)
    }
  }

  override val asciiCharQuoteMap: Map[Char, String] = Map(
    '\t' -> "\\t",
    '\n' -> "\\n",
    '\r' -> "\\r",
    '"' -> "\\\"",
    '\\' -> "\\\\",

    '\u0007' -> "\\a",
    '\b' -> "\\b",
    '\u000b' -> "\\v",
    '\f' -> "\\f",
    '\u001b' -> "\\027"
  )

  override def strLiteralUnicode(code: Char): String =
    "\\u{%04x}".format(code.toInt)

  override def numericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Div) =>
        s"math.floor(${translate(left)} / ${translate(right)})"
      case _ =>
        super.numericBinOp(left, op, right)
    }
  }

  override def arraySubscript(container: Ast.expr, idx: Ast.expr): String = {
    // Lua indexes start at 1, so we need to offset them
    s"${translate(container)}[${translate(idx)} + 1]"
  }
  override def doIfExp(condition: Ast.expr, ifTrue: Ast.expr, ifFalse: Ast.expr): String = {
    importList.add("local utils = require(\"utils\")")

    // http://lua-users.org/wiki/TernaryOperator (section Boxing/unboxing, using functions)
    s"utils.box_unwrap((${translate(condition)}) and utils.box_wrap(${translate(ifTrue)}) or (${translate(ifFalse)}))"
  }

  override def doBoolLiteral(n: Boolean): String =
    if (n) "true" else "false"
  override def doArrayLiteral(t: DataType, value: Seq[Ast.expr]): String =
    "{" + value.map((v) => translate(v)).mkString(", ") + "}"
  override def doByteArrayLiteral(arr: Seq[Byte]): String =
    "\"" + decEscapeByteArray(arr) + "\""

  override def doLocalName(s: String) = s match {
    case Identifier.ITERATOR => "_"
    case Identifier.INDEX => "i"
    case _ => s"self.${doName(s)}"
  }
  override def doName(s: String): String =
    s
  override def doEnumByLabel(enumTypeAbs: List[String], label: String): String =
    s"${LuaCompiler.types2class(enumTypeAbs)}.$label"
  override def doEnumById(enumTypeAbs: List[String], id: String): String =
    s"${LuaCompiler.types2class(enumTypeAbs)}($id)"

  // This is very hacky because integers and booleans cannot be compared
  override def doNumericCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): String = {
    val bool2Int = (n: Boolean) => { if (n) "1" else "0" }
    (left, right) match {
      case (Ast.expr.Bool(l), Ast.expr.Bool(r)) => s"${bool2Int(l)} ${cmpOp(op)} ${bool2Int(r)}"
      case (Ast.expr.Bool(l), r) => s"${bool2Int(l)} ${cmpOp(op)} ${translate(r)}"
      case (l, Ast.expr.Bool(r)) => s"${translate(l)} ${cmpOp(op)} ${bool2Int(r)}"
      case _ => super.doNumericCompareOp(left, op, right)
    }
  }

  override def strConcat(left: Ast.expr, right: Ast.expr): String =
    s"${translate(left)} .. ${translate(right)}"
  override def strToInt(s: Ast.expr, base: Ast.expr): String = {
    val baseStr = translate(base)
    val add = baseStr match {
      case "10" => ""
      case _ => s", $baseStr"
    }
    s"tonumber(${translate(s)}$add)"
  }
  override def enumToInt(v: Ast.expr, et: EnumType): String =
    s"${translate(v)}.value"
  override def boolToInt(v: Ast.expr): String =
    s"(${translate(v)} and 1 or 0)"
  override def floatToInt(v: Ast.expr): String =
    s"(${translate(v)} > 0) and math.floor(${translate(v)}) or math.ceil(${translate(v)})"
  override def intToStr(i: Ast.expr, base: Ast.expr): String = {
    val baseStr = translate(base)
    baseStr match {
      case "10" => s"tostring(${translate(i)})"
      case _ => throw new UnsupportedOperationException(baseStr)
    }
  }
  override def bytesToStr(bytesExpr: String, encoding: Ast.expr): String = {
    importList.add("local str_decode = require(\"string_decode\")")

    s"str_decode.decode($bytesExpr, ${translate(encoding)})"
  }
  override def bytesSubscript(container: Ast.expr, idx: Ast.expr): String = {
    s"string.byte(${translate(container)}, ${translate(idx)} + 1)"
  }
  override def bytesFirst(a: Ast.expr): String =
    s"string.byte(${translate(a)}, 1)"
  override def bytesLast(a: Ast.expr): String = {
    val table = translate(a)
    s"string.byte(${table}, #${table})"
  }
  override def bytesMin(a: Ast.expr): String = {
    importList.add("local utils = require(\"utils\")")

    s"utils.byte_array_min(${translate(a)})"
  }
  override def bytesMax(a: Ast.expr): String = {
    importList.add("local utils = require(\"utils\")")

    s"utils.byte_array_max(${translate(a)})"
  }
  override def strLength(s: Ast.expr): String =
    s"string.len(${translate(s)})"
  override def strReverse(s: Ast.expr): String =
    s"string.reverse(${translate(s)})"
  override def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): String =
    s"string.sub(${translate(s)}, ${translate(from)} + 1, ${translate(to)})"

  override def arrayFirst(a: Ast.expr): String =
    s"${translate(a)}[1]"
  override def arrayLast(a: Ast.expr): String = {
    val table = translate(a)
    s"${table}[#${table}]"
  }
  override def arraySize(a: Ast.expr): String =
    s"#${translate(a)}"
  override def arrayMin(a: Ast.expr): String = {
    importList.add("local utils = require(\"utils\")")

    s"utils.array_min(${translate(a)})"
  }
  override def arrayMax(a: Ast.expr): String ={
    importList.add("local utils = require(\"utils\")")

    s"utils.array_max(${translate(a)})"
  }

  override def kaitaiStreamSize(value: Ast.expr): String =
    s"${translate(value)}:size()"
  override def kaitaiStreamEof(value: Ast.expr): String =
    s"${translate(value)}:is_eof()"
  override def kaitaiStreamPos(value: Ast.expr): String =
    s"${translate(value)}:pos()"

  override def binOp(op: Ast.operator): String = op match {
    case Ast.operator.BitXor => "~"
    case _ => super.binOp(op)
  }
  override def cmpOp(op: Ast.cmpop): String = op match {
    case Ast.cmpop.NotEq => "~="
    case _ => super.cmpOp(op)
  }
  override def booleanOp(op: Ast.boolop): String = op match {
    case Ast.boolop.Or => "or"
    case Ast.boolop.And => "and"
  }
  override def unaryOp(op: Ast.unaryop): String = op match {
    case Ast.unaryop.Not => "not"
    case _ => super.unaryOp(op)
  }

  /**
   * Converts byte array (Seq[Byte]) into decimal-escaped Lua-style literal
   * characters (i.e. like \255).
   *
   * @param arr byte array to escape
   * @return array contents decimal-escaped as string
   */
  private def decEscapeByteArray(arr: Seq[Byte]): String =
    arr.map((x) => "\\%03d".format(x & 0xff)).mkString
}
