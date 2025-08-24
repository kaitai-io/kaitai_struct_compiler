package io.kaitai.struct.translators

import io.kaitai.struct.Utils
import io.kaitai.struct.datatype.DataType.EnumType
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{EnumSpec, Identifier}
import io.kaitai.struct.languages.RubyCompiler

class RubyTranslator(provider: TypeProvider) extends BaseTranslator(provider)
  with ByteArraysAsTrueArrays[String] {
  /**
  * @see https://ruby-doc.org/core-2.6.2/doc/syntax/precedence_rdoc.html
  */
  override val OPERATOR_PRECEDENCE = Map[Ast.binaryop, Int](
    Ast.operator.Mult -> 130,
    Ast.operator.Div -> 130,
    Ast.operator.Mod -> 130,
    Ast.operator.Add -> 120,
    Ast.operator.Sub -> 120,
    Ast.operator.LShift -> 110,
    Ast.operator.RShift -> 110,
    Ast.operator.BitAnd -> 100,
    Ast.operator.BitXor -> 90,
    Ast.operator.BitOr -> 90,
    Ast.cmpop.Lt -> 70,
    Ast.cmpop.LtE -> 70,
    Ast.cmpop.Gt -> 70,
    Ast.cmpop.GtE -> 70,
    Ast.cmpop.Eq -> 60,
    Ast.cmpop.NotEq -> 60
  )

  override def translate(v: Ast.expr, extPrec: Int): String = {
    val expr = super.translate(v, extPrec)
    v match {
      case Ast.expr.UnaryOp(op: Ast.unaryop, inner: Ast.expr) =>
        // This is needed so that `(~12).to_s` is not incorrectly translated as `~12.to_s`
        // in the generated Ruby code, which would cause the following error:
        //
        // ```
        // undefined method '~' for an instance of String (NoMethodError)
        // ```
        if (extPrec == METHOD_PRECEDENCE) {
          s"($expr)"
        } else {
          expr
        }
      case _ =>
        expr
    }
  }

  override def doByteArrayLiteral(arr: Seq[Byte]): String =
    s"[${arr.map(_ & 0xff).mkString(", ")}].pack('C*')"
  override def doByteArrayNonLiteral(elts: Seq[Ast.expr]): String =
    s"[${elts.map(translate).mkString(", ")}].pack('C*')"

  // https://docs.ruby-lang.org/en/3.4/syntax/literals_rdoc.html#label-Strings
  // https://github.com/ruby/ruby/blob/a38531fd3f617bf734ef7d6c595325f69985ea1d/string.c#L7201
  override val asciiCharQuoteMap: Map[Char, String] = Map(
    '\t' -> "\\t",
    '\n' -> "\\n",
    '\r' -> "\\r",
    '"' -> "\\\"",
    '\\' -> "\\\\",

    '#' -> "\\#",
    '\u0007' -> "\\a",
    '\f' -> "\\f",
    '\u000b' -> "\\v",
    '\u001b' -> "\\e",
    '\b' -> "\\b"
  )

  override def doName(s: String) = {
    s match {
      case Identifier.INDEX => "i" // FIXME: probably would clash with attribute named "i"
      case _ => s
    }
  }

  override def doInternalName(id: Identifier): String =
    RubyCompiler.privateMemberName(id)

  override def doEnumVariant(enumSpec: EnumSpec, variant: String): String =
    RubyCompiler.enumValue(enumSpec.name.last, variant)
  override def doEnumCast(enumSpec: EnumSpec, value: String): String =
    s"${RubyCompiler.kstreamName}::resolve_enum(${enumDirectMap(enumSpec.name)}, $value)"

  def enumDirectMap(enumTypeAndName: List[String]): String = {
    val enumTypeAbs = enumTypeAndName.dropRight(1)
    val enumTypeName = Utils.upperUnderscoreCase(enumTypeAndName.last)

    val enumTypeRel = Utils.relClass(enumTypeAbs, provider.nowClass.name)

    if (enumTypeRel.nonEmpty) {
      (enumTypeRel.map((x) => Utils.upperCamelCase(x)) ++ List(enumTypeName)).mkString("::")
    } else {
      enumTypeName
    }
  }

  def enumInverseMap(et: EnumType): String = {
    val enumTypeAndName = et.enumSpec.get.name
    val enumDirectMap = this.enumDirectMap(enumTypeAndName)
    val enumNameDirect = Utils.upperUnderscoreCase(enumTypeAndName.last)
    val enumNameInverse = RubyCompiler.inverseEnumName(enumNameDirect)

    enumDirectMap.replace(enumNameDirect, enumNameInverse)
  }

  override def arraySubscript(container: Ast.expr, idx: Ast.expr): String =
    s"${translate(container, METHOD_PRECEDENCE)}[${translate(idx)}]"
  override def doIfExp(condition: Ast.expr, ifTrue: Ast.expr, ifFalse: Ast.expr): String =
    s"(${translate(condition)} ? ${translate(ifTrue)} : ${translate(ifFalse)})"

  // Predefined methods of various types
  override def strToInt(s: Ast.expr, base: Ast.expr): String = {
    val baseStr = translate(base)
    s"${translate(s, METHOD_PRECEDENCE)}.to_i" + (baseStr match {
      case "10" => ""
      case _ => s"($baseStr)"
    })
  }
  override def enumToInt(v: Ast.expr, et: EnumType): String = {
    val value = translate(v)
    s"(${enumInverseMap(et)}[$value] || $value)"
  }
  override def floatToInt(v: Ast.expr): String =
    s"${translate(v, METHOD_PRECEDENCE)}.to_i"
  override def intToStr(i: Ast.expr): String =
    s"${translate(i, METHOD_PRECEDENCE)}.to_s"

  override def bytesToStr(bytesExpr: String, encoding: String): String = {
    // We can skip "encode to UTF8" if we're 100% sure that the string we're handling is already
    // in UTF8.
    s"""($bytesExpr).force_encoding(${doStringLiteral(encoding)})""" + (if (encoding != "UTF-8") {
      ".encode('UTF-8')"
    } else {
      ""
    })
  }

  override def bytesLength(b: Ast.expr): String =
    s"${translate(b, METHOD_PRECEDENCE)}.size"
  /**
    * Alternatives considered:
    *
    * * value[0].ord => 6341 => winner by performance
    * * value.bytes[0] => 8303
    */
  override def bytesSubscript(container: Ast.expr, idx: Ast.expr): String =
    s"${translate(container, METHOD_PRECEDENCE)}[${translate(idx)}].ord"
  override def bytesFirst(b: Ast.expr): String =
    s"${translate(b, METHOD_PRECEDENCE)}[0].ord"
  override def bytesLast(b: Ast.expr): String =
    s"${translate(b, METHOD_PRECEDENCE)}[-1].ord"
  override def bytesMin(b: Ast.expr): String =
    s"${translate(b, METHOD_PRECEDENCE)}.bytes.min"
  override def bytesMax(b: Ast.expr): String =
    s"${translate(b, METHOD_PRECEDENCE)}.bytes.max"

  override def strLength(s: Ast.expr): String =
    s"${translate(s, METHOD_PRECEDENCE)}.size"
  override def strReverse(s: Ast.expr): String =
    s"${translate(s, METHOD_PRECEDENCE)}.reverse"
  override def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): String =
    s"${translate(s, METHOD_PRECEDENCE)}[${translate(from)}...${translate(to)}]"

  override def arrayFirst(a: Ast.expr): String =
    s"${translate(a, METHOD_PRECEDENCE)}.first"
  override def arrayLast(a: Ast.expr): String =
    s"${translate(a, METHOD_PRECEDENCE)}.last"
  override def arraySize(a: Ast.expr): String =
    s"${translate(a, METHOD_PRECEDENCE)}.length"
  override def arrayMin(a: Ast.expr): String =
    s"${translate(a, METHOD_PRECEDENCE)}.min"
  override def arrayMax(a: Ast.expr): String =
    s"${translate(a, METHOD_PRECEDENCE)}.max"

  override def kaitaiStreamEof(value: Ast.expr): String =
    s"${translate(value, METHOD_PRECEDENCE)}.eof?"
}
