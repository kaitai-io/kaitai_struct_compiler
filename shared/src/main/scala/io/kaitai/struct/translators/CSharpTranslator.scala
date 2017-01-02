package io.kaitai.struct.translators

import io.kaitai.struct.Utils
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast._
import io.kaitai.struct.exprlang.DataType.{BaseType, IntType}
import io.kaitai.struct.languages.CSharpCompiler
import io.kaitai.struct.translators.components.CTernaryOperator

class CSharpTranslator(provider: TypeProvider) extends BaseTranslator(provider) with CTernaryOperator {
  override def doArrayLiteral(t: BaseType, value: Seq[expr]): (String, Int) = {
    val nativeType = CSharpCompiler.kaitaiType2NativeType(t)
    val commaStr = value.map((v) => translate(v)).mkString(", ")
    (s"new List<$nativeType> { $commaStr }", 0)
  }

  override def doByteArrayLiteral(arr: Seq[Byte]): (String, Int) =
    (s"new byte[] { ${arr.map(_ & 0xff).mkString(", ")} }", 0)

  override def numericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Mod) =>
        (s"${CSharpCompiler.kstreamName}.Mod(${translate(left)}, ${translate(right)})", 0)
      case _ =>
        super.numericBinOp(left, op, right)
    }
  }

  override def doName(s: String) =
    if (s.startsWith("_"))
      s"M${Utils.upperCamelCase(s)}"
    else
      s"${Utils.upperCamelCase(s)}"

  override def doEnumByLabel(enumTypeAbs: List[String], label: String): (String, Int) =
    (s"${enumClass(enumTypeAbs)}.${Utils.upperCamelCase(label)}", 0)
  override def doEnumById(enumTypeAbs: List[String], id: String): (String, Int) =
    (s"((${enumClass(enumTypeAbs)}) $id)", 0)

  def enumClass(enumTypeAbs: List[String]): String = {
    val enumTypeRel = Utils.relClass(enumTypeAbs, provider.nowClass.name)
    CSharpCompiler.types2class(enumTypeRel)
  }

  override def doStrCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr) = {
    op match {
      case Ast.cmpop.Eq | Ast.cmpop.NotEq =>
        super.doStrCompareOp(left, op, right)
      case _ =>
        (
          s"${translate(left, 0)}.CompareTo(${translate(right, 0)}) ${cmpOp(op)} 0",
          cmpOpPriority(op)
        )
    }
  }

  override def doSubscript(container: expr, idx: expr): String =
    s"${translate(container)}[${translate(idx)}]"

  // Predefined methods of various types
  override def strToInt(s: expr, base: expr): String =
    s"Convert.ToInt64(${translate(s)}, ${translate(base)})"
  override def strLength(s: expr): String =
    s"${translate(s)}.Length"
  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"${translate(s)}.Substring(${translate(from)}, ${translate(to)} - ${translate(from)})"

  override def arrayFirst(a: expr): String =
    s"${translate(a)}[0]"
  override def arrayLast(a: expr): String = {
    val v = translate(a)
    s"$v[$v.Length - 1]"
  }
}
