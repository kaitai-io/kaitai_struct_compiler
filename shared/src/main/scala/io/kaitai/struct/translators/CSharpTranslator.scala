package io.kaitai.struct.translators

import io.kaitai.struct.Utils
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast._
import io.kaitai.struct.languages.CSharpCompiler

class CSharpTranslator(provider: TypeProvider) extends BaseTranslator(provider) {
  override def doArrayLiteral(t: DataType, value: Seq[expr]): String = {
    val nativeType = CSharpCompiler.kaitaiType2NativeType(t)
    val commaStr = value.map((v) => translate(v)).mkString(", ")
    s"new List<$nativeType> { $commaStr }"
  }

  override def doByteArrayLiteral(arr: Seq[Byte]): String =
    s"new byte[] { ${arr.map(_ & 0xff).mkString(", ")} }"

  override val asciiCharQuoteMap: Map[Char, String] = Map(
    '\t' -> "\\t",
    '\n' -> "\\n",
    '\r' -> "\\r",
    '"' -> "\\\"",
    '\\' -> "\\\\",

    '\0' -> "\\0",
    '\7' -> "\\a",
    '\f' -> "\\f",
    '\13' -> "\\v",
    '\b' -> "\\b"
  )

  override def strLiteralGenericCC(code: Char): String = strLiteralUnicode(code)

  override def numericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Mod) =>
        s"${CSharpCompiler.kstreamName}.Mod(${translate(left)}, ${translate(right)})"
      case _ =>
        super.numericBinOp(left, op, right)
    }
  }

  override def doName(s: String) =
    if (s.startsWith("_"))
      s"M${Utils.upperCamelCase(s)}"
    else
      s"${Utils.upperCamelCase(s)}"

  override def doEnumByLabel(enumTypeAbs: List[String], label: String): String =
    s"${enumClass(enumTypeAbs)}.${Utils.upperCamelCase(label)}"
  override def doEnumById(enumTypeAbs: List[String], id: String): String =
    s"((${enumClass(enumTypeAbs)}) $id)"

  def enumClass(enumTypeAbs: List[String]): String = {
    val enumTypeRel = Utils.relClass(enumTypeAbs, provider.nowClass.name)
    CSharpCompiler.types2class(enumTypeRel)
  }

  override def doStrCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr) = {
    if (op == Ast.cmpop.Eq) {
      s"${translate(left)} == ${translate(right)}"
    } else if (op == Ast.cmpop.NotEq) {
      s"${translate(left)} != ${translate(right)}"
    } else {
      s"(${translate(left)}.CompareTo(${translate(right)}) ${cmpOp(op)} 0)"
    }
  }

  override def doBytesCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): String =
    s"(${CSharpCompiler.kstreamName}.ByteArrayCompare(${translate(left)}, ${translate(right)}) ${cmpOp(op)} 0)"

  override def doSubscript(container: expr, idx: expr): String =
    s"${translate(container)}[${translate(idx)}]"
  override def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String =
    s"(${translate(condition)} ? ${translate(ifTrue)} : ${translate(ifFalse)})"
  override def doCast(value: Ast.expr, typeName: String): String =
    s"((${Utils.upperCamelCase(typeName)}) (${translate(value)}))"

  // Predefined methods of various types
  override def strToInt(s: expr, base: expr): String =
    s"Convert.ToInt64(${translate(s)}, ${translate(base)})"
  override def enumToInt(v: expr, et: EnumType): String =
    translate(v)
  override def intToStr(i: expr, base: expr): String =
    s"Convert.ToString(${translate(i)}, ${translate(base)})"
  override def bytesToStr(bytesExpr: String, encoding: Ast.expr): String =
    s"System.Text.Encoding.GetEncoding(${translate(encoding)}).GetString($bytesExpr)"
  override def strLength(s: expr): String =
    s"${translate(s)}.Length"

  // FIXME: This is not fully Unicode aware, but might be better than nothing.
  // http://stackoverflow.com/a/228060/2055163
  override def strReverse(s: expr): String =
    s"new string(Array.Reverse(${translate(s)}.ToCharArray()))"

  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"${translate(s)}.Substring(${translate(from)}, ${translate(to)} - ${translate(from)})"

  override def arrayFirst(a: expr): String =
    s"${translate(a)}[0]"
  override def arrayLast(a: expr): String = {
    val v = translate(a)
    s"$v[$v.Count - 1]"
  }
  override def arraySize(a: expr): String =
    s"${translate(a)}.Count"
  override def arrayMin(a: Ast.expr): String =
    s"${translate(a)}.Min()"
  override def arrayMax(a: Ast.expr): String =
    s"${translate(a)}.Max()"
}
