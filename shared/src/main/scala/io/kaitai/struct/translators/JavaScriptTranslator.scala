package io.kaitai.struct.translators

import io.kaitai.struct.{ImportList, Utils}
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format.{EnumSpec, Identifier}
import io.kaitai.struct.languages.JavaScriptCompiler

class JavaScriptTranslator(provider: TypeProvider, importList: ImportList) extends BaseTranslator(provider) {
  override def doByteArrayLiteral(arr: Seq[Byte]): String =
    s"new Uint8Array([${arr.map(_ & 0xff).mkString(", ")}])"
  override def doByteArrayNonLiteral(elts: Seq[Ast.expr]): String =
    s"new Uint8Array([${elts.map(translate).mkString(", ")}])"

  /**
    * JavaScript rendition of common control character that would use hex form,
    * not octal. "Octal" control character string literals might be accepted
    * in non-strict JS mode, but in strict mode only hex or unicode are ok.
    * Here we'll use hex, as they are shorter.
    *
    * @see https://github.com/kaitai-io/kaitai_struct/issues/279
    * @param code character code to represent
    * @return string literal representation of given code
    */
  override def strLiteralGenericCC(code: Char): String =
    "\\x%02x".format(code.toInt)

  override def genericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr, extPrec: Int) = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Div) =>
        s"Math.floor(${super.genericBinOp(left, op, right, 0)})"
      case (_: IntType, _: IntType, Ast.operator.Mod) =>
        s"${JavaScriptCompiler.kstreamName}.mod(${translate(left)}, ${translate(right)})"
      case (_: IntType, _: IntType, Ast.operator.RShift) =>
        genericBinOpStr(left, op, ">>>", right, extPrec)
      case _ =>
        super.genericBinOp(left, op, right, extPrec)
    }
  }

  override def doLocalName(s: String) = {
    s match {
      case "_" => s
      case Identifier.SWITCH_ON => "on"
      case Identifier.INDEX => "i"
      case _ => s"this.${doName(s)}"
    }
  }

  override def doName(s: String) = {
    s match {
      case "_root" | "_parent" | "_io" => s
      case _ => Utils.lowerCamelCase(s)
    }
  }

  override def doInternalName(id: Identifier): String =
    JavaScriptCompiler.privateMemberName(id)

  override def doEnumByLabel(enumSpec: EnumSpec, label: String): String = {
    val isExternal = enumSpec.isExternal(provider.nowClass)
    if (isExternal) {
      val className = JavaScriptCompiler.type2class(enumSpec.name.head)
      importList.add(s"./$className")
    }
    s"${JavaScriptCompiler.types2class(enumSpec.name, isExternal)}.${Utils.upperUnderscoreCase(label)}"
  }
  override def doEnumById(enumSpec: EnumSpec, id: String): String =
    // Just an integer, without any casts / resolutions - one would have to look up constants manually
    id

  override def doBytesCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): String =
    s"(${JavaScriptCompiler.kstreamName}.byteArrayCompare(${translate(left)}, ${translate(right)}) ${cmpOp(op)} 0)"

  override def arraySubscript(container: expr, idx: expr): String =
    s"${translate(container)}[${translate(idx)}]"
  override def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String =
    s"(${translate(condition)} ? ${translate(ifTrue)} : ${translate(ifFalse)})"

  // Predefined methods of various types
  override def strToInt(s: expr, base: expr): String =
    s"Number.parseInt(${translate(s)}, ${translate(base)})"

  override def enumToInt(v: expr, et: EnumType): String =
    translate(v)

  /**
    * Converts a boolean (true or false) to integer (1 or 0, respectively) in
    * JavaScript. There are quite a few methods to so, this one is generally
    * accepted as one of the fastest (other top methods are +-0.3%), and it's
    * pretty concise and readable.
    *
    * @see http://stackoverflow.com/questions/7820683/convert-boolean-result-into-number-integer
    * @param v boolean expression to convert
    * @return string rendition of conversion
    */
  override def boolToInt(v: expr): String =
    s"(${translate(v)} | 0)"

  /**
    * Converts a float to an integer in JavaScript. There are many methods to
    * do so, here we use the fastest one, but it requires ES6+. OTOH, it is
    * relatively easy to add compatibility polyfill for non-supporting environments
    * (see MDN page).
    *
    * @see http://stackoverflow.com/a/596503/487064
    * @see https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Global_Objects/Math/trunc
    * @param v float expression to convert
    * @return string rendition of conversion
    */
  override def floatToInt(v: expr): String =
    s"Math.trunc(${translate(v)})"

  override def intToStr(i: expr): String =
    s"(${translate(i)}).toString()"

  override def bytesToStr(bytesExpr: String, encoding: String): String =
    s"""${JavaScriptCompiler.kstreamName}.bytesToStr($bytesExpr, ${doStringLiteral(encoding)})"""

  override def strLength(s: expr): String =
    s"${translate(s, METHOD_PRECEDENCE)}.length"

  // http://stackoverflow.com/a/36525647/2055163
  override def strReverse(s: expr): String =
    s"Array.from(${translate(s)}).reverse().join('')"

  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"${translate(s, METHOD_PRECEDENCE)}.substring(${translate(from)}, ${translate(to)})"

  override def arrayFirst(a: expr): String =
    s"${translate(a)}[0]"
  override def arrayLast(a: expr): String = {
    val v = translate(a)
    s"$v[$v.length - 1]"
  }
  override def arraySize(a: expr): String =
    s"${translate(a, METHOD_PRECEDENCE)}.length"
  override def arrayMin(a: expr): String =
    s"${JavaScriptCompiler.kstreamName}.arrayMin(${translate(a)})"
  override def arrayMax(a: expr): String =
    s"${JavaScriptCompiler.kstreamName}.arrayMax(${translate(a)})"

  override def kaitaiStreamEof(value: Ast.expr): String =
    s"${translate(value, METHOD_PRECEDENCE)}.isEof()"
}
