package io.kaitai.struct.translators

import io.kaitai.struct.Utils
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType.IntType
import io.kaitai.struct.languages.JavaScriptCompiler

class JavaScriptTranslator(provider: TypeProvider) extends BaseTranslator(provider) {
  override def numericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Div) =>
        s"Math.floor(${translate(left)} / ${translate(right)})"
      case (_: IntType, _: IntType, Ast.operator.Mod) =>
        s"${JavaScriptCompiler.kstreamName}.mod(${translate(left)}, ${translate(right)})"
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

  override def doEnumByLabel(enumType: List[String], label: String): String =
    s"${JavaScriptCompiler.types2class(enumType)}.${label.toUpperCase}"
  override def doEnumById(enumTypeAbs: List[String], label: String): String =
    // Just an integer, without any casts / resolutions - one would have to look up constants manually
    label

  override def doSubscript(container: expr, idx: expr): String =
    s"${translate(container)}[${translate(idx)}]"
  override def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String =
    s"(${translate(condition)} ? ${translate(ifTrue)} : ${translate(ifFalse)})"

  // Predefined methods of various types
  override def strToInt(s: expr, base: expr): String =
    s"Number.parseInt(${translate(s)}, ${translate(base)})"

  override def intToStr(i: expr, base: expr): String =
    s"(${translate(i)}).toString(${translate(base)})"

  override def strLength(s: expr): String =
    s"${translate(s)}.length"

  // http://stackoverflow.com/a/36525647/2055163
  override def strReverse(s: expr): String =
    s"Array.from(${translate(s)}).reverse().join('')"

  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"${translate(s)}.substring(${translate(from)}, ${translate(to)})"

  override def arrayFirst(a: expr): String =
    s"${translate(a)}[0]"
  override def arrayLast(a: expr): String = {
    val v = translate(a)
    s"$v[$v.length - 1]"
  }
  override def arraySize(a: expr): String =
    s"${translate(a)}.length"

  override def kaitaiStreamEof(value: Ast.expr): String =
    s"${translate(value)}.isEof()"
}
