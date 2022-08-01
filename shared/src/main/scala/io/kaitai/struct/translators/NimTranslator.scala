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
    s"encode($bytesExpr, ${translate(encoding)})"
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
      case Identifier.ITERATOR => doName(s)
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
    typeName match {
      case at: ArrayType => {
        importList.add("sequtils")
        s"${translate(value)}.mapIt(it.${ksToNim(at.elType)})"
      }
      case _ => s"(${ksToNim(typeName)}(${translate(value)}))"
    }
  override def doIntLiteral(n: BigInt): String = {
    if (n <= -2147483649L) { // -9223372036854775808..-2147483649
      s"$n'i64"
    } else if (n <= 2147483647L) { // -2147483648..2147483647
      s"$n"
    } else if (n <= 9223372036854775807L) { // 2147483648..9223372036854775807
      s"$n'i64"
    } else if (n <= Utils.MAX_UINT64) { // 9223372036854775807..18446744073709551615
      s"$n'u64"
    } else {
      s"$n"
    }
  }
  override def doArrayLiteral(t: DataType, value: Seq[expr]): String = {
    val contents = value.map(v => s"${ksToNim(t)}(${translate(v)})").mkString(", ")
    s"@[$contents]"
  }
  override def doByteArrayLiteral(arr: Seq[Byte]): String = {
    if (arr.size == 0)
      s"@[]"
    else
      "@[" + arr.map(b => {
        val ub: Int = b & 0xff
        ub
      }).mkString("'u8, ") + "'u8]"
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
    s"intToStr(int(${translate(v)}))"
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
