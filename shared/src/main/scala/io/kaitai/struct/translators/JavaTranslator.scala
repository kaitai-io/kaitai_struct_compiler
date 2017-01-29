package io.kaitai.struct.translators

import io.kaitai.struct.Utils
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast._
import io.kaitai.struct.exprlang.DataType.{BaseType, CalcIntType, IntType}
import io.kaitai.struct.languages.JavaCompiler

class JavaTranslator(provider: TypeProvider) extends BaseTranslator(provider) {
  override def doIntLiteral(n: BigInt): String = {
    val literal = n.toString
    val suffix = if (n > Int.MaxValue) "L" else ""

    s"${literal}${suffix}"
  }

  /**
   * Wrapper for {@link #doIntLiteral(BigInt)} if {@code CalcIntType} is known to be needed.
   * <p>
   * {@link #doIntLiteral(BigInt)} doesn't work for statements like {@code new ArrayList<Long>(Arrays.asList(0, 1, 100500))}
   * because it doesn't know that a {@code long} is always needed, even if the value of the number
   * wouldn't need it. Java by default assumes {@code int} for numeric literals and would create an
   * array with a different type than required.
   * </p>
   */
  def doIntLiteralCalcIntType(n: BigInt): String = {
    val literal = doIntLiteral(n)
    val isLong = JavaCompiler.kaitaiType2JavaTypePrim(CalcIntType) == "long"
    val suffixNeeded = if (isLong && !literal.endsWith("L")) true else false
    val suffix = if (suffixNeeded) "L" else ""

    s"${literal}${suffix}"
  }

  override def doArrayLiteral(t: BaseType, value: Seq[expr]): String = {
    val javaType = JavaCompiler.kaitaiType2JavaTypeBoxed(t)
    val values = t match {
      case CalcIntType => value.map((v) => doIntLiteralCalcIntType(v match { case Ast.expr.IntNum(n) => n }))
      case _ => value.map((v) => translate(v))
    }
    val commaStr = values.mkString(", ")

    s"new ArrayList<$javaType>(Arrays.asList($commaStr))"
  }

  override def doByteArrayLiteral(arr: Seq[Byte]): String =
    s"new byte[] { ${arr.mkString(", ")} }"

  override def numericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Mod) =>
        s"${JavaCompiler.kstreamName}.mod(${translate(left)}, ${translate(right)})"
      case _ =>
        super.numericBinOp(left, op, right)
    }
  }

  override def doName(s: String) =
    s match {
      case "_root" => s
      case "_parent" => "_parent()"
      case "_io" => "_io()"
      case "_" => "_it"
      case _ => s"${Utils.lowerCamelCase(s)}()"
    }

  override def doEnumByLabel(enumTypeAbs: List[String], label: String): String =
    s"${enumClass(enumTypeAbs)}.${label.toUpperCase}"
  override def doEnumById(enumTypeAbs: List[String], id: String): String =
    s"${enumClass(enumTypeAbs)}.byId($id)"

  def enumClass(enumTypeAbs: List[String]): String = {
    val enumTypeRel = Utils.relClass(enumTypeAbs, provider.nowClass.name)
    enumTypeRel.map((x) => Utils.upperCamelCase(x)).mkString(".")
  }

  override def doStrCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr) = {
    if (op == Ast.cmpop.Eq) {
      s"${translate(left)}.equals(${translate(right)})"
    } else if (op == Ast.cmpop.NotEq) {
      s"!(${translate(left)}).equals(${translate(right)})"
    } else {
      s"(${translate(left)}.compareTo(${translate(right)}) ${cmpOp(op)} 0)"
    }
  }

  override def doSubscript(container: expr, idx: expr): String = {
    val idxStr = translate(idx);
    val contStr = translate(container);
    val idxArgStr = idx match {
      case Ast.expr.IntNum(_) => idxStr
      case _ => s"Long.valueOf(${idxStr}).intValue()"
    }

    s"${contStr}.get(${idxArgStr})"
  }
  override def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String =
    s"(${translate(condition)} ? ${translate(ifTrue)} : ${translate(ifFalse)})"

  // Predefined methods of various types
  override def strToInt(s: expr, base: expr): String =
    s"Long.parseLong(${translate(s)}, ${translate(base)})"
  override def intToStr(i: expr, base: expr): String =
    s"Long.toString(${translate(i)}, ${translate(base)})"
  override def strLength(s: expr): String =
    s"${translate(s)}.length()"
  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"${translate(s)}.substring(${translate(from)}, ${translate(to)})"

  override def arrayFirst(a: expr): String =
    s"${translate(a)}.get(0)"
  override def arrayLast(a: expr): String = {
    val v = translate(a)
    s"$v.get($v.size() - 1)"
  }
}
