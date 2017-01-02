package io.kaitai.struct.translators

import io.kaitai.struct.Utils
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType.IntType
import io.kaitai.struct.languages.PHPCompiler
import io.kaitai.struct.translators.components.CTernaryOperator

class PHPTranslator(provider: TypeProvider, lang: PHPCompiler) extends BaseTranslator(provider) with CTernaryOperator {
  override def doByteArrayLiteral(arr: Seq[Byte]): (String, Int) =
    ("\"" + Utils.hexEscapeByteArray(arr) + "\"", 0)

  override def numericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Div) =>
        val prio = binOpPriority(Ast.operator.Div)
        (s"intval(${translate(left, prio)} / ${translate(right, prio)})", 0)
      case (_: IntType, _: IntType, Ast.operator.Mod) =>
        (s"${PHPCompiler.kstreamName}::mod(${translate(left, 0)}, ${translate(right, 0)})", 0)
      case _ =>
        super.numericBinOp(left, op, right)
    }
  }

  override def userTypeField(value: expr, attrName: String): String =
    s"${translate(value)}->${doName(attrName)}"

  override def doLocalName(s: String) = {
    s match {
      case "_" => "$_"
      case _ => s"$$this->${doName(s)}"
    }
  }

  override def doName(s: String) = s"${Utils.lowerCamelCase(s)}()"

  override def doEnumByLabel(enumTypeAbs: List[String], label: String): (String, Int) = {
    val enumClass = lang.types2classAbs(enumTypeAbs)
    (s"$enumClass::${label.toUpperCase}", 0)
  }
  override def doEnumById(enumTypeAbs: List[String], id: String) =
    // Just an integer, without any casts / resolutions - one would have to look up constants manually
    (id, 0)

  override def doSubscript(container: expr, idx: expr): String =
    s"${translate(container)}[${translate(idx)}]"

  // Predefined methods of various types
  override def strConcat(left: Ast.expr, right: Ast.expr): (String, Int) = {
    val prio = binOpPriority(Ast.operator.Add)
    (s"${translate(left, prio)} . ${translate(right, prio)}", prio)
  }

  override def strToInt(s: expr, base: expr): String =
    s"intval(${translate(s, 0)}, ${translate(base, 0)})"

  override def strLength(s: expr): String =
    s"strlen(${translate(s)})"

  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"${translate(s)}.substring(${translate(from)}, ${translate(to)})"

  override def arrayFirst(a: expr): String =
    s"${translate(a)}[0]"
  override def arrayLast(a: expr): String = {
    val v = translate(a)
    s"$v[$v.length - 1]"
  }
}
