package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.Ast.expr.Name
import io.kaitai.struct.format.{Identifier, ParentIdentifier, RootIdentifier}
import io.kaitai.struct.languages.RustCompiler
import io.kaitai.struct.{RuntimeConfig, Utils}

import scala.collection.mutable

class RustTranslator(provider: TypeProvider, config: RuntimeConfig) extends BaseTranslator(provider) {
  var castAsType: mutable.Stack[Option[DataType]] = mutable.Stack()

  def translateAsType(v: Ast.expr, t: Option[DataType]): String = {
    castAsType.push(t)
    val ret = translate(v)
    castAsType.pop()

    ret
  }

  override def doByteArrayLiteral(arr: Seq[Byte]): String =
    "[" + arr.map(x => "%0#2x".format(x & 0xff)).mkString(", ") + "]"
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
        castAsType.headOption match {
          case Some(Some(d)) => s"(self.${doName(s)} as ${RustCompiler.kaitaiPrimitiveToNativeType(d)})"
          case _ => s"self.${doName(s)}"
        }
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
    s"(${translate(v)} as ${RustCompiler.kaitaiPrimitiveToNativeType(CalcIntType)})"

  override def floatToInt(v: expr): String =
    s"(${translate(v)} as ${RustCompiler.kaitaiPrimitiveToNativeType(CalcIntType)})"

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

  override def bytesLength(b: Ast.expr): String = s"${translateAsType(b, None)}.len()"

  override def strLength(s: expr): String = s"${translateAsType(s, None)}.len()"

  override def strReverse(s: expr): String =
    s"${translate(s)}.graphemes(true).rev().flat_map(|g| g.chars()).collect()"
  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"${translate(s)}.substring(${translate(from)}, ${translate(to)})"

  override def arrayFirst(a: expr): String =
    s"${iterDeref()}${translateAsType(a, None)}.first().ok_or(KError::EmptyIterator)?"

  override def arrayLast(a: expr): String =
    s"${iterDeref()}${translateAsType(a, None)}.last().ok_or(KError::EmptyIterator)?"

  override def arraySize(a: expr): String = {
    val cast = castAsType.headOption match {
      case Some(Some(d)) => s" as ${RustCompiler.kaitaiPrimitiveToNativeType(d)}"
      case _ => ""
    }
    s"${translateAsType(a, None)}.len()$cast"
  }

  override def arrayMin(a: Ast.expr): String = {
    val iterMethod = castAsType.headOption match {
      case Some(Some(FloatMultiType(Width4, _))) => ".fold(None, kf32_min)"
      case Some(Some(FloatMultiType(Width8, _))) => ".fold(None, kf64_min)"
      case _ => ".min()"
    }
    s"${iterDeref()}${translateAsType(a, None)}.iter()$iterMethod.ok_or(KError::EmptyIterator)?"
  }

  override def arrayMax(a: Ast.expr): String = {
    val iterMethod = castAsType.headOption match {
      case Some(Some(FloatMultiType(Width4, _))) => ".fold(None, kf32_max)"
      case Some(Some(FloatMultiType(Width8, _))) => ".fold(None, kf64_max)"
      case _ => ".max()"
    }

    s"${iterDeref()}${translateAsType(a, None)}.iter()$iterMethod.ok_or(KError::EmptyIterator)?"
  }

  def iterDeref(): String = {
    val needsDeref = castAsType.headOption match {
      case Some(Some(dt: DataType)) => dt.isInstanceOf[NumericType] || dt.isInstanceOf[BooleanType]
      case _ => false
    }

    if (needsDeref) "*" else ""
  }

  override def doEnumCompareOp(left: expr, op: Ast.cmpop, right: expr): String =
  // TODO: This probably isn't legal - no guarantees that the enum value is on the right
    s"${translate(left)} ${cmpOp(op)} Some(${translate(right)})"

  override def doNumericCompareOp(left: expr, op: Ast.cmpop, right: expr): String =
  // TODO: No guarantees that the ident is on the left
    s"${translateAsType(left, None)} ${cmpOp(op)} ${translate(right)}"
}
