package io.kaitai.struct.translators

import io.kaitai.struct.{ImportList, Utils}
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast._
import io.kaitai.struct.format.{EnumSpec, Identifier}
import io.kaitai.struct.languages.CSharpCompiler

class CSharpTranslator(provider: TypeProvider, importList: ImportList) extends BaseTranslator(provider) {
  override def translate(v: Ast.expr, extPrec: Int): String = {
    val expr = super.translate(v, extPrec)
    v match {
      case Ast.expr.UnaryOp(op: Ast.unaryop, inner: Ast.expr) =>
        if (extPrec == METHOD_PRECEDENCE) {
          // This is needed so that `(-2).to_s` and `(~12).to_s` are not incorrectly
          // translated as `-2.ToString()` and `~12.ToString()`.
          s"($expr)"
        } else {
          expr
        }
      case _ =>
        expr
    }
  }

  override def doArrayLiteral(t: DataType, value: Seq[expr]): String = {
    val nativeType = CSharpCompiler.kaitaiType2NativeType(importList, t)
    val commaStr = value.map((v) => translate(v)).mkString(", ")

    importList.add("System.Collections.Generic")
    s"new List<$nativeType> { $commaStr }"
  }

  override def doByteArrayLiteral(arr: Seq[Byte]): String =
    s"new byte[] { ${arr.map(_ & 0xff).mkString(", ")} }"
  override def doByteArrayNonLiteral(elts: Seq[Ast.expr]): String =
    s"new byte[] { ${elts.map(translate).mkString(", ")} }"

  override val asciiCharQuoteMap: Map[Char, String] = Map(
    '\t' -> "\\t",
    '\n' -> "\\n",
    '\r' -> "\\r",
    '"' -> "\\\"",
    '\\' -> "\\\\",

    '\u0000' -> "\\0",
    '\u0007' -> "\\a",
    '\f' -> "\\f",
    '\u000b' -> "\\v",
    '\b' -> "\\b"
  )

  override def strLiteralGenericCC(code: Char): String = strLiteralUnicode(code)

  override def genericBinOp(left: Ast.expr, op: Ast.binaryop, right: Ast.expr, extPrec: Int) = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Mod) =>
        s"${CSharpCompiler.kstreamName}.Mod(${translate(left)}, ${translate(right)})"
      case _ =>
        super.genericBinOp(left, op, right, extPrec)
    }
  }

  override def doName(s: String) =
    if (s.startsWith("_")) {
      s match {
        case Identifier.SWITCH_ON => "on"
        case Identifier.INDEX => "i"
        case _ => s"M${Utils.upperCamelCase(s)}"
      }
    } else {
      s"${Utils.upperCamelCase(s)}"
    }

  override def doInternalName(id: Identifier): String =
    CSharpCompiler.privateMemberName(id)

  override def doEnumVariant(enumSpec: EnumSpec, variant: String): String =
    s"${enumClass(enumSpec.name)}.${Utils.upperCamelCase(variant)}"
  override def doEnumCast(enumSpec: EnumSpec, value: String): String =
    s"((${enumClass(enumSpec.name)}) $value)"

  def enumClass(enumTypeAbs: List[String]): String = {
    val enumTypeRel = Utils.relClass(enumTypeAbs, provider.nowClass.name)
    CSharpCompiler.types2class(enumTypeRel)
  }

  override def doStrCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr, extPrec: Int) = {
    if (op == Ast.cmpop.Eq || op == Ast.cmpop.NotEq) {
      super.doStrCompareOp(left, op, right, extPrec)
    } else {
      s"(${translate(left)}.CompareTo(${translate(right)}) ${cmpOp(op)} 0)"
    }
  }

  override def doBytesCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr, extPrec: Int): String =
    s"(${CSharpCompiler.kstreamName}.ByteArrayCompare(${translate(left)}, ${translate(right)}) ${cmpOp(op)} 0)"

  override def arraySubscript(container: expr, idx: expr): String =
    s"${translate(container, METHOD_PRECEDENCE)}[${translate(idx)}]"
  override def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String =
    s"(${translate(condition)} ? ${translate(ifTrue)} : ${translate(ifFalse)})"
  override def doCast(value: Ast.expr, typeName: DataType): String =
    s"((${CSharpCompiler.kaitaiType2NativeType(importList, typeName)}) (${translate(value)}))"

  // Predefined methods of various types
  override def strToInt(s: expr, base: expr): String = {
    importList.add("System")
    s"Convert.ToInt64(${translate(s)}, ${translate(base)})"
  }
  override def enumToInt(v: expr, et: EnumType): String =
    // Always casting to `int` works fine at the time of writing this, because the
    // enums we generate for C# are `int`-based (we generate `public enum $enumClass
    // { ... }`, see `CSharpCompiler.enumDeclaration`, and according to
    // <https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/enum>:
    // "By default, the associated constant values of enum members are of type
    // `int`").
    //
    // However, once we start generating enums with underlying types other than
    // `int`, we will have to change this.
    s"((int) ${translate(v, METHOD_PRECEDENCE)})"
  override def floatToInt(v: expr): String =
    s"(long) (${translate(v)})"
  override def intToStr(i: expr): String =
    s"${translate(i, METHOD_PRECEDENCE)}.ToString()"
  override def bytesToStr(bytesExpr: String, encoding: String): String =
    s"""System.Text.Encoding.GetEncoding(${doStringLiteral(encoding)}).GetString($bytesExpr)"""
  override def strLength(s: expr): String =
    s"${translate(s, METHOD_PRECEDENCE)}.Length"

  override def strReverse(s: expr): String =
    s"${CSharpCompiler.kstreamName}.StringReverse(${translate(s)})"

  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"${translate(s, METHOD_PRECEDENCE)}.Substring(${translate(from)}, ${genericBinOp(to, Ast.operator.Sub, from, 0)})"

  override def bytesLength(b: Ast.expr): String =
    s"${translate(b, METHOD_PRECEDENCE)}.Length"
  override def bytesLast(b: Ast.expr): String = {
    val v = translate(b, METHOD_PRECEDENCE)
    s"$v[$v.Length - 1]"
  }

  override def arrayFirst(a: expr): String =
    s"${translate(a, METHOD_PRECEDENCE)}[0]"
  override def arrayLast(a: expr): String = {
    val v = translate(a, METHOD_PRECEDENCE)
    s"$v[$v.Count - 1]"
  }
  override def arraySize(a: expr): String =
    s"${translate(a, METHOD_PRECEDENCE)}.Count"
  override def arrayMin(a: Ast.expr): String = {
    importList.add("System.Linq")
    s"${translate(a, METHOD_PRECEDENCE)}.Min()"
  }
  override def arrayMax(a: Ast.expr): String = {
    importList.add("System.Linq")
    s"${translate(a, METHOD_PRECEDENCE)}.Max()"
  }
}
