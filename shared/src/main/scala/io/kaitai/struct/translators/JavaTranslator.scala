package io.kaitai.struct.translators

import io.kaitai.struct.{ClassTypeProvider, ImportList, RuntimeConfig, Utils}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast._
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.format.Identifier
import io.kaitai.struct.languages.JavaCompiler

class JavaTranslator(provider: TypeProvider, importList: ImportList, config: RuntimeConfig) extends BaseTranslator(provider) {
  override def doIntLiteral(n: BigInt): String = {
    // Java's integer parsing behaves differently depending on whether you use decimal or hex syntax.
    // With decimal syntax, the parser/compiler rejects any number that cannot be stored in a long
    // (signed 64-bit integer) without overflow. With hexadecimal syntax, it allows anything that
    // would fit in an unsigned 64-bit integer. Java's `long` is always signed, so if you use this
    // trick to enter a number that is too large for a signed 64-bit integer, it will overflow into
    // negative. But this can still be useful if you don't perform any arithmetic that cares about
    // the sign (e. g. you pass the value unmodified to something else, or you use only bit operations
    // or Long's "unsigned" methods).
    //
    // Of course, if `n > Utils.MAX_UINT64` we'll still get out of range error
    // TODO: Convert real big numbers to BigInteger
    val literal = if (n > Long.MaxValue && n <= Utils.MAX_UINT64) {
      "0x" + n.toString(16)
    } else {
      n.toString
    }
    val suffix = if (n < Int.MinValue || n > Int.MaxValue) "L" else ""

    s"$literal$suffix"
  }

  override def doArrayLiteral(t: DataType, value: Seq[expr]): String = {
    // FIXME
    val compiler = new JavaCompiler(provider.asInstanceOf[ClassTypeProvider], config)

    val javaType = compiler.kaitaiType2JavaTypeBoxed(t)
    val commaStr = value.map((v) => translate(v)).mkString(", ")

    importList.add("java.util.ArrayList")
    importList.add("java.util.Arrays")
    s"new ArrayList<$javaType>(Arrays.asList($commaStr))"
  }

  override def doByteArrayLiteral(arr: Seq[Byte]): String =
    s"new byte[] { ${arr.mkString(", ")} }"
  override def doByteArrayNonLiteral(elts: Seq[expr]): String =
    s"new byte[] { ${elts.map(translate).mkString(", ")} }"

  override def numericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Mod) =>
        s"${JavaCompiler.kstreamName}.mod(${translate(left)}, ${translate(right)})"
      case _ =>
        super.numericBinOp(left, op, right)
    }
  }

  override def doNumericCompareOp(left: expr, op: cmpop, right: expr): String =
    s"(${super.doNumericCompareOp(left, op, right)})"

  override def doEnumCompareOp(left: expr, op: cmpop, right: expr): String =
    s"(${super.doEnumCompareOp(left, op, right)})"

  override def doName(s: String) =
    s match {
      case Identifier.ITERATOR => "_it"
      case Identifier.ITERATOR2 => "_buf"
      case Identifier.SWITCH_ON => "on"
      case Identifier.INDEX => "i"
      case _ => s"${Utils.lowerCamelCase(s)}()"
    }

  override def doInternalName(id: Identifier): String =
    s"${JavaCompiler.publicMemberName(id)}()"

  override def doEnumByLabel(enumTypeAbs: List[String], label: String): String =
    s"${enumClass(enumTypeAbs)}.${Utils.upperUnderscoreCase(label)}"
  override def doEnumById(enumTypeAbs: List[String], id: String): String =
    s"${enumClass(enumTypeAbs)}.byId($id)"

  def enumClass(enumTypeAbs: List[String]): String = {
    val enumTypeRel = Utils.relClass(enumTypeAbs, provider.nowClass.name)
    enumTypeRel.map((x) => Utils.upperCamelCase(x)).mkString(".")
  }

  override def doStrCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): String = op match {
    case Ast.cmpop.Eq =>
      s"${translate(left)}.equals(${translate(right)})"
    case Ast.cmpop.NotEq =>
      s"!(${translate(left)}).equals(${translate(right)})"
    case _ =>
      s"(${translate(left)}.compareTo(${translate(right)}) ${cmpOp(op)} 0)"
  }

  override def doBytesCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): String = {
    op match {
      case Ast.cmpop.Eq =>
        importList.add("java.util.Arrays")
        s"Arrays.equals(${translate(left)}, ${translate(right)})"
      case Ast.cmpop.NotEq =>
        importList.add("java.util.Arrays")
        s"!Arrays.equals(${translate(left)}, ${translate(right)})"
      case _ =>
        s"(${JavaCompiler.kstreamName}.byteArrayCompare(${translate(left)}, ${translate(right)}) ${cmpOp(op)} 0)"
    }
  }

  override def arraySubscript(container: expr, idx: expr): String =
    s"${translate(container)}.get(${doCast(idx, CalcIntType)})"
  override def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String =
    s"(${translate(condition)} ? ${translate(ifTrue)} : ${translate(ifFalse)})"
  override def doCast(value: Ast.expr, typeName: DataType): String = {
    // FIXME
    val compiler = new JavaCompiler(provider.asInstanceOf[ClassTypeProvider], config)
    if (value.isInstanceOf[Ast.expr.IntNum] || value.isInstanceOf[Ast.expr.FloatNum])
      // this branch is not really needed, but makes the code a bit cleaner -
      // we can simplify casting to just this for numeric constants
      s"((${compiler.kaitaiType2JavaType(typeName)}) ${translate(value)})"
    else
      compiler.castIfNeeded(translate(value), AnyType, typeName)
  }

  // Predefined methods of various types
  override def strToInt(s: expr, base: expr): String =
    s"Long.parseLong(${translate(s)}, ${translate(base)})"
  override def enumToInt(v: expr, et: EnumType): String =
    s"${translate(v)}.id()"
  override def floatToInt(v: expr): String =
    doCast(v, CalcIntType)
  override def intToStr(i: expr, base: expr): String =
    s"Long.toString(${translate(i)}, ${translate(base)})"
  override def bytesToStr(bytesExpr: String, encoding: String): String = {
    // Java has a small number of standard charsets preloaded. Accessing them as constants is more
    // efficient than looking them up by string in a map, so we utilize this when as possible.
    // See https://docs.oracle.com/javase/7/docs/api/java/nio/charset/StandardCharsets.html
    val standardCharsetsMap = Map(
      "ISO-8859-1" -> "ISO_8859_1",
      "ASCII" -> "US_ASCII",
      "UTF-16BE" -> "UTF_16BE",
      "UTF-16LE" -> "UTF_16LE",
      "UTF-8" -> "UTF_8",
    )

    val charsetExpr = standardCharsetsMap.get(encoding) match {
      case Some(charsetConst) =>
        importList.add("java.nio.charset.StandardCharsets")
        s"StandardCharsets.${charsetConst}"
      case None =>
        importList.add("java.nio.charset.Charset")
        s"""Charset.forName("$encoding")"""
    }
    s"new String($bytesExpr, $charsetExpr)"
  }
  override def bytesIndexOf(b: expr, byte: expr): String =
    s"${JavaCompiler.kstreamName}.byteArrayIndexOf(${translate(b)}, ${doCast(byte, Int1Type(true))})"

  override def bytesLength(b: Ast.expr): String =
    s"${translate(b)}.length"
  override def bytesSubscript(container: Ast.expr, idx: Ast.expr): String =
    s"(${translate(container)}[${doCast(idx, CalcIntType)}] & 0xff)"
  override def bytesFirst(b: Ast.expr): String =
    bytesSubscript(b, Ast.expr.IntNum(0))
  override def bytesLast(b: Ast.expr): String =
    bytesSubscript(b, Ast.expr.BinOp(
      Ast.expr.Attribute(
        b,
        Ast.identifier("length")
      ),
      Ast.operator.Sub,
      Ast.expr.IntNum(1)
    ))
  override def bytesMin(b: Ast.expr): String =
    s"${JavaCompiler.kstreamName}.byteArrayMin(${translate(b)})"
  override def bytesMax(b: Ast.expr): String =
    s"${JavaCompiler.kstreamName}.byteArrayMax(${translate(b)})"

  override def strLength(s: expr): String =
    s"(${translate(s)}).length()"
  override def strReverse(s: expr): String =
    s"new StringBuilder(${translate(s)}).reverse().toString()"
  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"(${translate(s)}).substring(${translate(from)}, ${translate(to)})"
  override def strToBytes(s: expr, encoding: expr): String = {
    importList.add("java.nio.charset.Charset")
    s"(${translate(s)}).getBytes(Charset.forName(${translate(encoding)}))"
  }

  override def arrayFirst(a: expr): String =
    s"${translate(a)}.get(0)"
  override def arrayLast(a: expr): String = {
    val v = translate(a)
    s"$v.get($v.size() - 1)"
  }
  override def arraySize(a: expr): String =
    s"${translate(a)}.size()"
  override def arrayMin(a: Ast.expr): String = {
    importList.add("java.util.Collections")
    s"Collections.min(${translate(a)})"
  }
  override def arrayMax(a: Ast.expr): String = {
    importList.add("java.util.Collections")
    s"Collections.max(${translate(a)})"
  }
}
