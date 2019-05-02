package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.Ast.expr.Name
import io.kaitai.struct.format.{Identifier, ParentIdentifier, RootIdentifier}
import io.kaitai.struct.languages.RustCompiler
import io.kaitai.struct.{RuntimeConfig, Utils}

class RustTranslator(provider: TypeProvider, config: RuntimeConfig) extends BaseTranslator(provider) {
  var castAsType: Option[DataType] = None

  override def doByteArrayLiteral(arr: Seq[Byte]): String =
    "vec![" + arr.map(x => "%0#2x".format(x & 0xff)).mkString(", ") + "]"
  override def doByteArrayNonLiteral(elts: Seq[Ast.expr]): String =
    s"pack('C*', ${elts.map(translate).mkString(", ")})"

  override val asciiCharQuoteMap: Map[Char, String] = Map(
    '\t' -> "\\t",
    '\n' -> "\\n",
    '\r' -> "\\r",
    '"' -> "\\\"",
    '\\' -> "\\\\"
  )

  override def strLiteralUnicode(code: Char): String =
    "\\u{%x}".format(code.toInt)

  override def numericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr): String = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Div) =>
        s"${translate(left)} / ${translate(right)}"
      case (_: IntType, _: IntType, Ast.operator.Mod) =>
        s"${translate(left)} % ${translate(right)}"
      case _ =>
        super.numericBinOp(left, op, right)
    }
  }

  override def doLocalName(s: String): String = {
    s match {
      case Identifier.ITERATOR => "tmpa"
      case Identifier.ITERATOR2 => "tmpb"
      case Identifier.INDEX => "i"
      case Identifier.ROOT => s"${RustCompiler.privateMemberName(RootIdentifier)}.ok_or(KError::MissingRoot)?"
      case Identifier.PARENT => s"${RustCompiler.privateMemberName(ParentIdentifier)}.ok_or(KError::MissingParent)?"
      case _ =>
        val suffix = castAsType match {
          case Some(d) => s"as ${RustCompiler.kaitaiTypeToNativeType(d)}"
          case None => ""
        }
        s"self.${doName(s)} $suffix".trim
    }
  }

  override def doName(s: String): String = s


  override def userTypeField(userType: UserType, value: expr, attrName: String): String = {
    // Like `doLocalName`, but called when we are attempting to access user type fields.
    // Important because we need to handle unwrapping the Option type
    value match {
      // TODO: Clean up this guard
      case Name(id) if id.name == Identifier.ROOT
        || id.name == Identifier.PARENT
        || id.name == Identifier.ITERATOR
        || id.name == Identifier.IO => super.userTypeField(userType, value, attrName)
      case _ => s"${translate(value)}.as_ref().unwrap().${doName(attrName)}"
    }
  }

  override def doEnumByLabel(enumTypeAbs: List[String], label: String): String = {
    s"${RustCompiler.normalizeClassName(enumTypeAbs)}::${Utils.upperCamelCase(label)}"
  }
  override def doEnumById(enumTypeAbs: List[String], id: String): String =
    // Just an integer, without any casts / resolutions - one would have to look up constants manually
    id

  override def doSubscript(container: expr, idx: expr): String =
    s"${translate(container)}[${translate(idx)}]"
  override def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String =
    "if " + translate(condition) +
    	" { " + translate(ifTrue) + " } else { " +
	translate(ifFalse) + "}"

  // Predefined methods of various types
  override def strConcat(left: Ast.expr, right: Ast.expr): String =
    "format!(\"{}{}\", " + translate(left) + ", " + translate(right) + ")"

  override def strToInt(s: expr, base: expr): String =
    translate(base) match {
      case "10" =>
        s"${translate(s)}.parse().unwrap()"
      case _ =>
        "panic!(\"Converting from string to int in base {} is unimplemented\"" + translate(base) + ")"
    }

  override def enumToInt(v: expr, et: EnumType): String =
    translate(v)

  override def boolToInt(v: expr): String =
    s"${translate(v)} as i32"

  override def floatToInt(v: expr): String =
    s"${translate(v)} as i32"

  override def intToStr(i: expr, base: expr): String = {
    val baseStr = translate(base)
    baseStr match {
      case "10" =>
        s"${translate(i)}.to_string()"
      case _ =>
        s"base_convert(strval(${translate(i)}), 10, $baseStr)"
    }
  }

  override def bytesToStr(bytesExpr: String, encoding: Ast.expr): String =
    translate(encoding) match {
      // Because ASCII is a subset of UTF-8,
      case "\"ASCII\"" | "\"UTF-8\"" => s"str::from_utf8($bytesExpr).or(Err(KError::Encoding { expected: ${translate(encoding)} }))?"
      case _ => "panic!(\"Unimplemented encoding for bytesToStr: {}\", " + translate(encoding) + ")"
    }

  override def bytesLength(b: Ast.expr): String =
    s"${translate(b)}.len()"
  override def strLength(s: expr): String =
    s"${translate(s)}.len()"
  override def strReverse(s: expr): String =
    s"${translate(s)}.graphemes(true).rev().flat_map(|g| g.chars()).collect()"
  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"${translate(s)}.substring(${translate(from)}, ${translate(to)})"

  override def arrayFirst(a: expr): String =
    s"${translate(a)}.first()"
  override def arrayLast(a: expr): String =
    s"${translate(a)}.last()"
  override def arraySize(a: expr): String =
    s"${translate(a)}.len()"
  override def arrayMin(a: Ast.expr): String =
    s"${translate(a)}.iter().min()"
  override def arrayMax(a: Ast.expr): String =
    s"${translate(a)}.iter().max()"

  override def doEnumCompareOp(left: expr, op: Ast.cmpop, right: expr): String =
  // TODO: This probably isn't legal - no guarantees that the enum value is on the right
    s"${translate(left)} ${cmpOp(op)} Some(${translate(right)})"
}
