package io.kaitai.struct.translators

import java.nio.charset.Charset

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format.Identifier
import io.kaitai.struct.languages.ObjcCompiler
import io.kaitai.struct.{ImportList, Utils}

class ObjcTranslator(provider: TypeProvider, importListSrc: ImportList) extends BaseTranslator(provider) {
  val CHARSET_UTF8 = Charset.forName("UTF-8")

  // Members declared in io.kaitai.struct.translators.BaseTranslator
  override def bytesToStr(value: String, expr: io.kaitai.struct.exprlang.Ast.expr): String =
    s"[${value} KSBytesToStringWithEncoding:${translate(expr)}]"
  override def doEnumById(enumTypeAbs: List[String], id: String): String = s"doEnumById"
  override def doEnumByLabel(enumTypeAbs: List[String], label: String): String = s"doEnumByLabel"
  override def doIfExp(condition: io.kaitai.struct.exprlang.Ast.expr, ifTrue: io.kaitai.struct.exprlang.Ast.expr, ifFalse: io.kaitai.struct.exprlang.Ast.expr): String = s"doIfExp"

  override def doStrCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr) = {
    op match {
      case Ast.cmpop.Eq =>
        s"[${translate(left)} isEqualToString:${translate(right)}]"
      case Ast.cmpop.NotEq =>
        s"!([${translate(left)} isEqualToString:${translate(right)}])"
      case Ast.cmpop.Lt =>
        s"([${translate(left)} compare:${translate(right)}] == NSOrderedAscending)"
      case Ast.cmpop.Gt =>
        s"([${translate(left)} compare:${translate(right)}] == NSOrderedDescending)"
      case Ast.cmpop.GtE =>
        s"([${translate(left)} compare:${translate(right)}] >= NSOrderedSame)"
      case Ast.cmpop.LtE =>
        s"([${translate(left)} compare:${translate(right)}] <= NSOrderedSame)"
    }
  }

  override def doLocalName(s: String, t: Option[DataType]) = {
    s match {
      case Identifier.ITERATOR => "_"
      case Identifier.INDEX => "i"
      case _ => s"self.${doName(s, t)}"
    }
  }

  override def doName(s: String, t: Option[DataType] = None) = s match {
    case Identifier.ITERATOR => "_"
    case Identifier.ITERATOR2 => "_buf"
    case Identifier.INDEX => "i"
    case _ => t match {
      case Some(CalcBooleanType) => s"$s.boolValue"
      case Some(Int1Type(false)) => s"$s.unsignedCharValue"
      case Some(Int1Type(true)) => s"$s.charValue"
      case Some(IntMultiType(true,Width2,_)) => s"$s.shortValue"
      case Some(IntMultiType(false,Width2,_)) => s"$s.unsignedShortValue"
      case Some(IntMultiType(true,Width4,_)) => s"$s.intValue"
      case Some(IntMultiType(false,Width4,_)) => s"$s.unsignedIntValue"
      case Some(IntMultiType(true,Width8,_)) => s"$s.longLongValue"
      case Some(IntMultiType(false,Width8,_)) => s"$s.unsignedLongLongValue"
      case Some(CalcIntType) => s"$s.unsignedLongLongValue"
      case Some(CalcFloatType) => s"$s.doubleValue"
      case _ => s"$s"
    }
  }

  override def userTypeField(userType: UserType, value: Ast.expr, attrName: String): String =
    translate(value) + "." + doName(attrName, Some(provider.determineType(userType.classSpec.get, attrName)))

  override def anyField(value: Ast.expr, attrName: String): String =
    s"${translate(value)}.${doName(attrName, Some(detectType(value)))}"

  override def doSubscript(container: io.kaitai.struct.exprlang.Ast.expr, idx: io.kaitai.struct.exprlang.Ast.expr): String = s"doSubscript"

  // Members declared in io.kaitai.struct.translators.CommonMethods
  override def arrayFirst(a: io.kaitai.struct.exprlang.Ast.expr): String = s"arrayFirst"
  override def arrayLast(a: io.kaitai.struct.exprlang.Ast.expr): String = s"arrayLast"
  override def arrayMax(a: io.kaitai.struct.exprlang.Ast.expr): String = s"arrayMax"
  override def arrayMin(a: io.kaitai.struct.exprlang.Ast.expr): String = s"arrayMin"
  override def arraySize(a: io.kaitai.struct.exprlang.Ast.expr): String = s"arraySize"
  override def enumToInt(value: io.kaitai.struct.exprlang.Ast.expr, et: io.kaitai.struct.datatype.DataType.EnumType): String = s"enumToInt"
  override def floatToInt(value: io.kaitai.struct.exprlang.Ast.expr): String = s"floatToInt"
  override def intToStr(value: io.kaitai.struct.exprlang.Ast.expr, num: io.kaitai.struct.exprlang.Ast.expr): String = s"intToStr"
  override def strLength(s: io.kaitai.struct.exprlang.Ast.expr): String =
        s"[${translate(s)} length]"
  override def strReverse(s: io.kaitai.struct.exprlang.Ast.expr): String = s"strReverse"
  override def strSubstring(s: io.kaitai.struct.exprlang.Ast.expr, from: io.kaitai.struct.exprlang.Ast.expr, to: io.kaitai.struct.exprlang.Ast.expr): String =
    s"[${translate(s)} substringWithRange:NSMakeRange(${translate(from)}, ${translate(to)} - ${translate(from)})]"
  override def strToInt(s: io.kaitai.struct.exprlang.Ast.expr, base: io.kaitai.struct.exprlang.Ast.expr): String =
    s"${translate(s)}.KSToNumberWithBase:${translate(base)}"

  override def doStringLiteral(s: String): String = "@" + super.doStringLiteral(s)
  override def doBoolLiteral(n: Boolean): String = if (n) "YES" else "NO"

  override def strConcat(left: Ast.expr, right: Ast.expr): String =
    s"[${translate(left)} stringByAppendingString:${translate(right)}]"

  override def numericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Mod) =>
        s"[kstream modA(${translate(left)} b:${translate(right)}]"
      case _ =>
        super.numericBinOp(left, op, right)
    }
  }
}
