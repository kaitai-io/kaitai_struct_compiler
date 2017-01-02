package io.kaitai.struct.translators

import io.kaitai.struct.Utils
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType.IntType
import io.kaitai.struct.languages.JavaScriptCompiler
import io.kaitai.struct.translators.components.CTernaryOperator

class JavaScriptTranslator(provider: TypeProvider) extends BaseTranslator(provider) with CTernaryOperator {
  override def numericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Div) =>
        val divPrio = binOpPriority(Ast.operator.Div)
        (s"Math.floor(${translate(left, divPrio)} / ${translate(right, divPrio)})", 0)
      case (_: IntType, _: IntType, Ast.operator.Mod) =>
        (s"${JavaScriptCompiler.kstreamName}.mod(${translate(left, 0)}, ${translate(right, 0)})", 0)
      case _ =>
        super.numericBinOp(left, op, right)
    }
  }

  override def doLocalName(s: String) = {
    s match {
      case "_" => s
      case _ => s"this.${doName(s)}"
    }
  }

  override def doName(s: String) = {
    s match {
      case "_root" | "_parent" | "_io" => s
      case _ => Utils.lowerCamelCase(s)
    }
  }

  override def doEnumByLabel(enumType: List[String], label: String): (String, Int) =
    (s"${JavaScriptCompiler.types2class(enumType)}.${label.toUpperCase}", 0)
  override def doEnumById(enumTypeAbs: List[String], id: String): (String, Int) =
    // Just an integer, without any casts / resolutions - one would have to look up constants manually
    (id, 0)

  override def doSubscript(container: expr, idx: expr): String =
    s"${translate(container)}[${translate(idx)}]"

  // Predefined methods of various types
  override def strToInt(s: expr, base: expr): String =
    s"Number.parseInt(${translate(s)}, ${translate(base)})"

  override def strLength(s: expr): String =
    s"${translate(s)}.length"

  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"${translate(s)}.substring(${translate(from)}, ${translate(to)})"

  override def arrayFirst(a: expr): String =
    s"${translate(a)}[0]"
  override def arrayLast(a: expr): String = {
    val v = translate(a)
    s"$v[$v.length - 1]"
  }

  override def kaitaiStreamEof(value: Ast.expr): String =
    s"${translate(value)}.isEof()"
}
