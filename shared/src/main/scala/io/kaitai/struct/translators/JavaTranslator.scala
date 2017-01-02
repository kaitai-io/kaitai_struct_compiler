package io.kaitai.struct.translators

import io.kaitai.struct.Utils
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast._
import io.kaitai.struct.exprlang.DataType.{BaseType, IntType}
import io.kaitai.struct.languages.JavaCompiler
import io.kaitai.struct.translators.components.CTernaryOperator

class JavaTranslator(provider: TypeProvider) extends BaseTranslator(provider) with CTernaryOperator {
  override def doArrayLiteral(t: BaseType, value: Seq[expr]): (String, Int) = {
    val javaType = JavaCompiler.kaitaiType2JavaTypeBoxed(t)
    val commaStr = value.map((v) => translate(v)).mkString(", ")
    (s"new ArrayList<$javaType>(Arrays.asList($commaStr))", 0)
  }

  override def doByteArrayLiteral(arr: Seq[Byte]): (String, Int) =
    (s"new byte[] { ${arr.mkString(", ")} }", 0)

  override def numericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Mod) =>
        (s"${JavaCompiler.kstreamName}.mod(${translate(left, 0)}, ${translate(right, 0)})", 0)
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

  override def doEnumByLabel(enumTypeAbs: List[String], label: String): (String, Int) =
    (s"${enumClass(enumTypeAbs)}.${label.toUpperCase}", 0)
  override def doEnumById(enumTypeAbs: List[String], id: String): (String, Int) =
    (s"${enumClass(enumTypeAbs)}.byId($id)", 0)

  def enumClass(enumTypeAbs: List[String]): String = {
    val enumTypeRel = Utils.relClass(enumTypeAbs, provider.nowClass.name)
    enumTypeRel.map((x) => Utils.upperCamelCase(x)).mkString(".")
  }

  override def doStrCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr) = {
    op match {
      case Ast.cmpop.Eq =>
        (s"${translate(left, 5)}.equals(${translate(right, 0)})", 0)
      case Ast.cmpop.NotEq =>
        (s"!${translate(left, 15)}.equals(${translate(right, 0)})", 0)
      case _ =>
        (
          s"${translate(left, 0)}.compareTo(${translate(right, 0)}) ${cmpOp(op)} 0",
          cmpOpPriority(op)
        )
    }
  }

  override def doSubscript(container: expr, idx: expr): String =
    s"${translate(container)}.get((int) ${translate(idx)})"

  // Predefined methods of various types
  override def strToInt(s: expr, base: expr): String =
    s"Long.parseLong(${translate(s)}, ${translate(base)})"
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
