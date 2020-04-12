package io.kaitai.struct.translators

import io.kaitai.struct.{ImportList, Utils}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast._
import io.kaitai.struct.datatype._
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.format.Identifier
import io.kaitai.struct.languages.NimCompiler.{ksToNim, namespaced, camelCase}

class NimTranslator(provider: TypeProvider, importList: ImportList) extends BaseTranslator(provider) {
  // Members declared in io.kaitai.struct.translators.BaseTranslator
  override def bytesToStr(bytesExpr: String, encoding: Ast.expr): String = {
    importList.add("encodings")
    s"convert($bytesExpr, srcEncoding = ${translate(encoding)})"
  }
  override def doEnumById(enumTypeAbs: List[String], id: String): String = s"${namespaced(enumTypeAbs)}($id)"
//  override def doEnumByLabel(enumTypeAbs: List[String], label: String): String = s"${namespaced(enumTypeAbs)}($label)"
  override def doEnumByLabel(enumTypeAbs: List[String], label: String): String = s"${enumTypeAbs.head}.$label"
  override def doName(s: String): String =
    s match {
      case Identifier.ROOT => "root"
      case Identifier.PARENT => "parent"
      case Identifier.IO => "io"
      case Identifier.ITERATOR => "it"
      case Identifier.ITERATOR2 => "buf"
      case Identifier.INDEX => "i"
      case Identifier.SWITCH_ON => "on"
      case Identifier.IS_LE => "isLe"
      case Identifier.SIZEOF => "size"
      case _ => s"${camelCase(s, false)}"
    }
  override def doLocalName(s: String): String =
    s match {
      case Identifier.INDEX => doName(s)
      case Identifier.ROOT => s"${ksToNim(provider.determineType(Identifier.ROOT))}(this.${doName(s)})"
      case _ => s"this.${doName(s)}"
    }
  override def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String =
    s"(if ${translate(condition)}: ${translate(ifTrue)} else: ${translate(ifFalse)})"
  override def arraySubscript(container: expr, idx: expr): String =
    s"${translate(container)}[${translate(idx)}]"

  override def strConcat(left: Ast.expr, right: Ast.expr): String = "($" + s"${translate(left)} & " + "$" + s"${translate(right)})"

  // Members declared in io.kaitai.struct.translators.CommonMethods

  override def unaryOp(op: Ast.unaryop): String = op match {
    case Ast.unaryop.Invert => "not"
    case Ast.unaryop.Minus => "-"
    case Ast.unaryop.Not => "not"
  }

  override def booleanOp(op: Ast.boolop): String = op match {
      case Ast.boolop.And => "and"
      case Ast.boolop.Or => "or"
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
    if (n < -9223372036854775808L) { // too low, no type conversion would help anyway
      s"$n"
    } else if (n <= -2147483649L) { // -9223372036854775808..–2147483649
      s"$n'i64"
    } else if (n <= 2147483647L) { // -2147483648..2147483647
      s"$n"
    } else if (n <= Utils.MAX_UINT64) { // 2147483648..18446744073709551615
      s"$n'u64"
    } else { // too high, no type conversion would help anyway
      s"$n"
    }
  }
  override def doArrayLiteral(t: DataType, value: Seq[expr]): String = {
    s"@[${value.map((v) => translate(v)).mkString(", ")}]"
  }
  override def doByteArrayLiteral(arr: Seq[Byte]): String = {
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
    s"${translate(s)}.substr(${translate(from)}, ${translate(to)} - 1)"
  override def strToInt(s: expr, base: expr): String =
    s"${translate(s)}.parseInt(${translate(base)})"
}
