package io.kaitai.struct.translators

import java.nio.charset.Charset

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format.Identifier
import io.kaitai.struct.languages.ObjcCompiler
import io.kaitai.struct.{ImportList, Utils}
import io.kaitai.struct.precompile.TypeMismatchError

class ObjcTranslator(provider: TypeProvider, importListSrc: ImportList) extends BaseTranslator(provider) {
  val CHARSET_UTF8 = Charset.forName("UTF-8")

  /**
    * Handles integer literals for Objective-C by appending relevant suffix to
    * decimal notation.
    *
    * Note that suffixes essentially mean "long", "unsigned long",
    * and "unsigned long long", which are not really guaranteed to match
    * `int32_t`, `uint32_t` and `uint64_t`, but it would work for majority
    * of current compilers.
    *
    * For reference, ranges of integers that are used in this conversion are:
    *
    * * int32_t (no suffix): –2147483648..2147483647
    * * uint32_t (UL): 0..4294967295
    * * int64_t (LL): -9223372036854775808..9223372036854775807
    * * uint64_t (ULL): 0..18446744073709551615
    *
    * Merging all these ranges, we get the following decision tree:
    *
    * * -9223372036854775808..-2147483649 => LL
    * * -2147483648..2147483647 => no suffix
    * * 2147483648..4294967295 => UL
    * * 4294967296..9223372036854775807 => LL
    * * 9223372036854775808..18446744073709551615 => ULL
    *
    * Beyond these boundaries, Objective-C is unlikely to be able to represent
    * these anyway, so we just drop the suffix and hope for the miracle.
    *
    * @param n integer to render
    * @return rendered integer literal in Objective-C syntax as string
    */
  override def doIntLiteral(n: BigInt): String = {
    val suffix = if (n < -9223372036854775808L) {
      "" // too low, no suffix would help anyway
    } else if (n <= -2147483649L) {
      "LL" // -9223372036854775808..–2147483649
    } else if (n <= 2147483647L) {
      "" // -2147483648..2147483647
    } else if (n <= 4294967295L) {
      "UL" // 2147483648..4294967295
    } else if (n <= 9223372036854775807L) {
      "LL" // 4294967296..9223372036854775807
    } else if (n <= Utils.MAX_UINT64) {
      "ULL" // 9223372036854775808..18446744073709551615
    } else {
      "" // too high, no suffix would help anyway
    }

    s"$n$suffix"
  }

  // Members declared in io.kaitai.struct.translators.BaseTranslator
  override def bytesToStr(value: String, expr: io.kaitai.struct.exprlang.Ast.expr): String =
    s"[${value} KSBytesToStringWithEncoding:${translate(expr)}]"
  override def doEnumById(enumTypeAbs: List[String], id: String): String =
    if(id.startsWith("(self._io).")) {
        s"[$id KSENUMWithDictionary:${ObjcCompiler.types2class(enumTypeAbs.dropRight(1))}._${enumTypeAbs.last}]"
    } else {
        s"[@($id) KSENUMWithDictionary:${ObjcCompiler.types2class(enumTypeAbs.dropRight(1))}._${enumTypeAbs.last}]"
    }

  override def doEnumByLabel(enumTypeAbs: List[String], label: String): String =
    "[@\"" + s"$label" + "\" " + s"KSENUMWithDictionary:${ObjcCompiler.types2class(enumTypeAbs.dropRight(1))}._${enumTypeAbs.last}]"

  override def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String =
    s"((${translate(condition)}) ? (${translate(ifTrue)}) : (${translate(ifFalse)}))"

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

  override def doBytesCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): String = {
    op match {
      case Ast.cmpop.Eq =>
        s"[${translate(left)} isEqualToData:${translate(right)}]"
      case Ast.cmpop.NotEq =>
        s"!([${translate(left)} isEqualToData:${translate(right)}])"
      case Ast.cmpop.Lt =>
        s"([${translate(left)} KSCompare:${translate(right)}] < NSOrderedSame)"
      case Ast.cmpop.Gt =>
        s"([${translate(left)} KSCompare:${translate(right)}] > NSOrderedSame)"
      case Ast.cmpop.GtE =>
        s"([${translate(left)} KSCompare:${translate(right)}] >= NSOrderedSame)"
      case Ast.cmpop.LtE =>
        s"([${translate(left)} KSCompare:${translate(right)}] <= NSOrderedSame)"
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
      case Some(FloatMultiType(Width8,_)) => s"$s.doubleValue"
      case Some(FloatMultiType(Width4,_)) => s"$s.floatValue"
      case _ => s"$s"
    }
  }

  override def doArrayLiteral(t: DataType, value: Seq[Ast.expr]): String =
    "@[" + value.map((v) => boxNSNumber(translate(v))).mkString(", ") + "]"

  override def doByteArrayLiteral(arr: Seq[Byte]): String =
    "[NSData dataWithBytes:\"" + Utils.hexEscapeByteArray(arr) + "\" length:" + arr.length + "]"

  override def userTypeField(userType: UserType, value: Ast.expr, attrName: String): String =
    translate(value) + "." + doName(attrName, Some(provider.determineType(userType.classSpec.get, attrName)))

  override def anyField(value: Ast.expr, attrName: String): String =
    s"${translate(value)}.${doName(attrName, Some(detectType(value)))}"

  override def doSubscript(container: expr, idx: expr): String =
    doName(s"${translate(container)}[${translate(idx)}]", Some(detectType(container).asInstanceOf[ArrayType].elType))
  def boxNSNumber(s: String): String = s"@($s)"

  // Members declared in io.kaitai.struct.translators.CommonMethods
  override def arrayFirst(a: io.kaitai.struct.exprlang.Ast.expr): String =
    doName(s"${translate(a)}.firstObject", Some(detectType(a).asInstanceOf[ArrayType].elType))
  override def arrayLast(a: io.kaitai.struct.exprlang.Ast.expr): String =
    doName(s"${translate(a)}.lastObject", Some(detectType(a).asInstanceOf[ArrayType].elType))
  override def arrayMax(a: io.kaitai.struct.exprlang.Ast.expr): String =
    doName(s"((${ObjcCompiler.kaitaiType2NativeType(detectType(a).asInstanceOf[ArrayType].elType)})[" + translate(a) + " valueForKeyPath: @\"@max.self\"])", Some(detectType(a).asInstanceOf[ArrayType].elType))
  override def arrayMin(a: io.kaitai.struct.exprlang.Ast.expr): String =
    doName(s"((${ObjcCompiler.kaitaiType2NativeType(detectType(a).asInstanceOf[ArrayType].elType)})[" + translate(a) + " valueForKeyPath: @\"@min.self\"])", Some(detectType(a).asInstanceOf[ArrayType].elType))
  override def arraySize(a: io.kaitai.struct.exprlang.Ast.expr): String = s"${translate(a)}.count"
  override def enumToInt(value: io.kaitai.struct.exprlang.Ast.expr, et: io.kaitai.struct.datatype.DataType.EnumType): String =
    doName(s"((${ObjcCompiler.kaitaiType2NativeType(et.asInstanceOf[EnumType].basedOn)})" + translate(value) + "[@\"value\"])", Some(et.asInstanceOf[EnumType].basedOn))
  override def floatToInt(v: expr): String = s"((int)${translate(v)})"
  override def intToStr(value: io.kaitai.struct.exprlang.Ast.expr, num: io.kaitai.struct.exprlang.Ast.expr): String =
    "[NSString stringWithFormat:@\"%d\", " + s"${translate(value)}]"
  override def strLength(s: io.kaitai.struct.exprlang.Ast.expr): String = s"[${translate(s)} length]"
  override def strReverse(s: io.kaitai.struct.exprlang.Ast.expr): String = s"[${translate(s)} KSReverse]"
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
        s"[kstream modA:${translate(left)} b:${translate(right)}]"
      case _ =>
        super.numericBinOp(left, op, right)
    }
  }
  override def doEnumCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr) = {
    (detectType(left), detectType(right), op) match {
      case (_: EnumType, _: EnumType, Ast.cmpop.Eq) =>
        s"[${translate(left)} KSIsEqualToENUM:${translate(right)}]"
      case _ =>
        throw new TypeMismatchError(s"can't use comparison operator $op on enums")
    }
  }
}
