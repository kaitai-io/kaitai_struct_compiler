package io.kaitai.struct.translators

import io.kaitai.struct.Utils
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast._
import io.kaitai.struct.exprlang.DataType.{BaseType, IntType}
import io.kaitai.struct.languages.JavaCompiler

class JavaTranslator(provider: TypeProvider) extends BaseTranslator(provider) {
  override def doArrayLiteral(t: BaseType, value: Seq[expr]): String = {
    val javaType = JavaCompiler.kaitaiType2JavaTypeBoxed(t)
    val commaStr = value.map((v) => translate(v)).mkString(", ")
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

  override def doSubscript(container: expr, idx: expr): String =
    s"${translate(container)}.get((int) ${translate(idx)})"
  override def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String =
    s"(${translate(condition)} ? ${translate(ifTrue)} : ${translate(ifFalse)})"

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
