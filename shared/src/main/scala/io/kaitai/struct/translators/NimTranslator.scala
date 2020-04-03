package io.kaitai.struct.translators

import io.kaitai.struct.{ImportList, Utils}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast._
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.format.Identifier
import io.kaitai.struct.languages.NimCompiler.{ksToNim, namespaced}

class NimTranslator(provider: TypeProvider, importList: ImportList) extends BaseTranslator(provider) {
  // Members declared in io.kaitai.struct.translators.BaseTranslator
  override def bytesToStr(bytesExpr: String, encoding: Ast.expr): String = {
    importList.add("encodings")
    s"convert($bytesExpr, srcEncoding = ${translate(encoding)})"
  }
  override def doEnumById(enumTypeAbs: List[String], id: String): String = s"${namespaced(enumTypeAbs)}($id)"
  override def doEnumByLabel(enumTypeAbs: List[String], label: String): String = s"${namespaced(enumTypeAbs)}($label)"
  override def doName(s: String): String =
    s match {
      case Identifier.PARENT => "parent"
      case Identifier.IO => "stream"
      case Identifier.ITERATOR2 => "it"
      case _ => s"${Utils.lowerCamelCase(s)}"
    }
  override def doLocalName(s: String): String = "this." + doName(s)
  override def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String =
    s"(if ${translate(condition)}: ${translate(ifTrue)} else: ${translate(ifFalse)})"
  override def arraySubscript(container: expr, idx: expr): String =
    s"${translate(container)}[${translate(idx)}]"

  override def strConcat(left: Ast.expr, right: Ast.expr): String = s"${translate(left)} & ${translate(right)}"

  // Members declared in io.kaitai.struct.translators.CommonMethods

  override def unaryOp(op: Ast.unaryop): String = op match {
    case Ast.unaryop.Invert => "not"
    case Ast.unaryop.Minus => "-"
    case Ast.unaryop.Not => "not"
  }

  override def binOp(op: Ast.operator): String = {
    op match {
      case Ast.operator.Add => "+"
      case Ast.operator.Sub => "-"
      case Ast.operator.Mult => "*"
      case Ast.operator.Div => "div"
      case Ast.operator.Mod => "%%%"
      case Ast.operator.BitAnd => "and"
      case Ast.operator.BitOr => "or"
      case Ast.operator.BitXor => "xor"
      case Ast.operator.LShift => "shl"
      case Ast.operator.RShift => "shr"
    }
  }
  override def doCast(value: Ast.expr, typeName: DataType): String =
    s"(${ksToNim(typeName)}(${translate(value)}))"
  override def doIntLiteral(n: BigInt): String = {
    if (n < -9223372036854775808L) {
      s"$n" // too low, no type conversion would help anyway
    } else if (n <= -2147483649L) {
      s"$n'i64" // -9223372036854775808..–2147483649
    } else if (n <= 2147483647L) {
      s"$n" // -2147483648..2147483647
    } else if (n <= 4294967295L) {
      s"$n'u32" // 2147483648..4294967295
    } else if (n <= 9223372036854775807L) {
      s"$n'i64" // 4294967296..9223372036854775807
    } else if (n <= Utils.MAX_UINT64) {
      s"$n'u64" // 9223372036854775808..18446744073709551615
    } else {
      s"$n" // too high, no type conversion would help anyway
    }
  }
  override def doArrayLiteral(t: DataType, value: Seq[expr]): String = {
    //s"@[${value.map((v) => translate(v)).mkString(", ")}].mapIt(${ksToNim(t)}(it))"
    s"@[${value.map((v) => translate(v)).mkString(", ")}]"
  }
  override def doByteArrayLiteral(arr: Seq[Byte]): String = {
    //importList.add("sequtils")
    //s"@[${arr.mkString(", ")}].mapIt(it.toByte).toString"
    val first = s"${arr.head}'i8"
    if (arr.size == 0)
      s"@[]"
    else
      s"@[${first + ", " + arr.tail.mkString(", ")}].toString"
  }
  override def doByteArrayNonLiteral(elts: Seq[expr]): String =
    s"@[${elts.map(translate).mkString(", ")}]"
  override def arrayFirst(a: expr): String = s"${translate(a)}[0]"
  override def arrayLast(a: expr): String = s"${translate(a)}[^1]"
  override def arrayMax(a: expr): String = s"max(${translate(a)})"
  override def arrayMin(a: expr): String = s"min(${translate(a)})"
  override def arraySize(a: expr): String = s"len(${translate(a)})"
  override def enumToInt(v: expr, et: EnumType): String = s"ord(${translate(v)})"
  override def floatToInt(v: expr): String = s"int(${translate(v)})"
  override def intToStr(v: expr, base: expr): String = {
    importList.add("strutils")
    s"intToStr(${translate(v)})"
  }
  override def strLength(s: expr): String = s"len(${translate(s)})"
  override def strReverse(s: expr): String = {
    importList.add("unicode")
    s"reversed(${translate(s)})"
  }
  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"${translate(s)}.substr(${translate(from)}, ${translate(to)})"
  override def strToInt(s: expr, base: expr): String =
    s"${translate(s)}.parseInt(${translate(base)}"
}
