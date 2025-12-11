package io.kaitai.struct.translators

import io.kaitai.struct.{ClassTypeProvider, ExternalEnum, ImportList, RuntimeConfig, Utils}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast._
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.format.{EnumSpec, Identifier}
import io.kaitai.struct.languages.ZigCompiler

class ZigTranslator(provider: TypeProvider, importList: ImportList, config: RuntimeConfig) extends BaseTranslator(provider) {
  /**
  * @see https://ziglang.org/documentation/0.15.2/#Precedence
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
    Ast.operator.BitXor -> 100,
    Ast.operator.BitOr -> 100,
    Ast.cmpop.Lt -> 90,
    Ast.cmpop.LtE -> 90,
    Ast.cmpop.Gt -> 90,
    Ast.cmpop.GtE -> 90,
    Ast.cmpop.Eq -> 90,
    Ast.cmpop.NotEq -> 90
  )

  override def doArrayLiteral(t: DataType, value: Seq[expr]): String = {
    val nativeType = ZigCompiler.kaitaiType2NativeType(t, importList, provider.nowClass)
    val commaStr = value.map((v) => translate(v)).mkString(", ")

    s"_imp_std.ArrayList($nativeType){ .items = @constCast(@as([]const $nativeType, &.{ $commaStr })), .capacity = ${value.length} }"
  }

  override def doByteArrayLiteral(arr: Seq[Byte]): String =
    s"&[_]u8{ ${arr.map(_ & 0xff).mkString(", ")} }"
  override def doByteArrayNonLiteral(elts: Seq[expr]): String =
    s"&[_]u8{ ${elts.map(translate).mkString(", ")} }"

  // https://ziglang.org/documentation/0.15.2/#Escape-Sequences
  override val asciiCharQuoteMap: Map[Char,String] = Map(
    '\n' -> "\\n",
    '\r' -> "\\r",
    '\t' -> "\\t",
    '\\' -> "\\\\",
    '"' -> "\\\"",
  )

  override def doStringLiteralBody(s: String): String =
    s.codePointStepper.iterator.map { code =>
      if (code <= 0xff) {
        strLiteralAsciiChar(code.toChar)
      } else {
        "\\u{%04x}".format(code)
      }
    }.mkString

  override def strLiteralGenericCC(code: Char): String =
    "\\u{%04x}".format(code.toInt)

  override def genericBinOp(left: Ast.expr, op: Ast.binaryop, right: Ast.expr, extPrec: Int) = {
    (detectType(left), detectType(right), op) match {
      case (_: NumericType, _: NumericType, Ast.operator.Mod) =>
        s"@mod(${translate(left)}, ${translate(right)})"
      case _ =>
        super.genericBinOp(left, op, right, extPrec)
    }
  }

  override def doLocalName(s: String) =
    s match {
      case Identifier.ITERATOR => "_it"
      case Identifier.ITERATOR2 => "_buf"
      case Identifier.SWITCH_ON => "_on"
      case Identifier.INDEX => "i"
      case Identifier.ROOT | Identifier.PARENT =>
        s"self.$s.?"
      case Identifier.IO =>
        s"self.$s"
      case _ =>
        if (provider.isLazy(s)) {
          s"(try self.${Utils.lowerCamelCase(s)}())"
        } else {
          s"self.$s"
        }
    }

  override def doName(s: String) =
    s

  override def doInternalName(id: Identifier): String =
    ZigCompiler.privateMemberName(id)

  override def userTypeField(ut: UserType, value: Ast.expr, name: String): String = {
    val valueStr = translate(value)

    name match {
      case Identifier.ROOT | Identifier.PARENT =>
        s"$valueStr.$name.?"
      case Identifier.IO =>
        s"$valueStr.$name"
      case _ =>
        val isLazy = provider.isLazy(ut.classSpec.get, name)
        if (isLazy) {
          s"(try $valueStr.${Utils.lowerCamelCase(name)}())"
        } else {
          s"$valueStr.$name"
        }
    }
  }

  override def doEnumByLabel(enumSpec: EnumSpec, label: String): String = {
    val et = EnumType(enumSpec.name, CalcIntType)
    et.enumSpec = Some(enumSpec)
    s"${ZigCompiler.kaitaiType2NativeType(et, importList, provider.nowClass)}.$label"
  }
  override def doEnumById(enumSpec: EnumSpec, id: String): String = {
    val et = EnumType(enumSpec.name, CalcIntType)
    et.enumSpec = Some(enumSpec)
    s"@as(${ZigCompiler.kaitaiType2NativeType(et, importList, provider.nowClass)}, @enumFromInt($id))"
  }

  override def doStrCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr, extPrec: Int): String =
    doBytesCompareOp(left, op, right, extPrec)

  override def doBytesCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr, extPrec: Int): String = {
    op match {
      case Ast.cmpop.Eq =>
        s"_imp_std.mem.eql(u8, ${translate(left)}, ${translate(right)})"
      case Ast.cmpop.NotEq =>
        s"!_imp_std.mem.eql(u8, ${translate(left)}, ${translate(right)})"
      case Ast.cmpop.Lt =>
        s"(_imp_std.mem.order(u8, ${translate(left)}, ${translate(right)}) == .lt)"
      case Ast.cmpop.Gt =>
        s"(_imp_std.mem.order(u8, ${translate(left)}, ${translate(right)}) == .gt)"
      case Ast.cmpop.LtE =>
        s"_imp_std.mem.order(u8, ${translate(left)}, ${translate(right)}).compare(.lte)"
      case Ast.cmpop.GtE =>
        s"_imp_std.mem.order(u8, ${translate(left)}, ${translate(right)}).compare(.gte)"
    }
  }

  override def booleanOp(op: Ast.boolop) = op match {
    case Ast.boolop.Or => "or"
    case Ast.boolop.And => "and"
  }

  override def arraySubscript(container: expr, idx: expr): String =
    s"${translate(container)}.items[${translate(idx)}]"
  override def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String =
    s"(if (${translate(condition)}) ${translate(ifTrue)} else ${translate(ifFalse)})"
  override def doCast(value: Ast.expr, typeName: DataType): String = {
    val valueType = detectTypeRaw(value)
    valueType match {
      case st: SwitchType if (ZigCompiler.switchUsesTaggedUnion(st)) =>
        val unionFieldName = ZigCompiler.dataTypeToUnionFieldName(typeName)
        s"${translate(value, METHOD_PRECEDENCE)}.$unionFieldName"
      case _ =>
        s"@as(${ZigCompiler.kaitaiType2NativeType(typeName, importList, provider.nowClass)}, ${translate(value)})"
    }
  }

  // Predefined methods of various types
  override def strConcat(left: Ast.expr, right: Ast.expr, extPrec: Int): String =
    s"(try _imp_std.mem.concat(self._allocator(), u8, &[_][]const u8{ ${translate(left)}, ${translate(right)} }))"
  override def strToInt(s: expr, base: expr): String =
    s"(try _imp_std.fmt.parseInt(i32, ${translate(s)}, ${translate(base)}))"
  override def enumToInt(v: expr, et: EnumType): String =
    s"@intFromEnum(${translate(v)})"
  override def floatToInt(v: expr): String =
    s"@intFromFloat(${translate(v)})"
  override def boolToInt(value: Ast.expr): String =
    s"@intFromBool(${translate(value)})"

  override def intToStr(i: expr): String =
    s"""(try _imp_std.fmt.allocPrint(self._allocator(), "{d}", .{ ${translate(i)} }))"""
  override def bytesToStr(bytesExpr: String, encoding: String): String =
    s"(try ${ZigCompiler.kstreamName}.bytesToStr(self._allocator(), $bytesExpr, ${doStringLiteral(encoding)}))"
  override def bytesLength(b: Ast.expr): String =
    s"${translate(b, METHOD_PRECEDENCE)}.len"
  override def bytesSubscript(container: Ast.expr, idx: Ast.expr): String =
    s"${translate(container, METHOD_PRECEDENCE)}[${translate(idx)}]"
  override def bytesFirst(b: Ast.expr): String =
    bytesSubscript(b, Ast.expr.IntNum(0))
  override def bytesLast(b: Ast.expr): String =
    bytesSubscript(b, Ast.expr.BinOp(
      Ast.expr.Attribute(
        b,
        Ast.identifier("length")
      ),
      Ast.operator.Sub,
      Ast.expr.IntNum(1)
    ))
  override def bytesMin(b: Ast.expr): String =
    s"_imp_std.mem.min(u8, ${translate(b)})"
  override def bytesMax(b: Ast.expr): String =
    s"_imp_std.mem.max(u8, ${translate(b)})"

  override def strLength(s: expr): String =
    bytesLength(s)
  override def strReverse(s: expr): String =
    s"new StringBuilder(${translate(s)}).reverse().toString()"
  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"${translate(s, METHOD_PRECEDENCE)}[${translate(from)}..${translate(to)}]"

  override def arrayFirst(a: expr): String =
    s"${translate(a, METHOD_PRECEDENCE)}.items[0]"
  override def arrayLast(a: expr): String = {
    val v = translate(a, METHOD_PRECEDENCE)
    s"$v.items[$v.items.len - 1]"
  }
  override def arraySize(a: expr): String =
    s"${translate(a, METHOD_PRECEDENCE)}.items.len"
  override def arrayMin(a: Ast.expr): String = {
    val t = detectType(a)
    val elType = t.asInstanceOf[ArrayType].elType
    s"_imp_std.mem.min(${ZigCompiler.kaitaiType2NativeType(elType, importList, provider.nowClass)}, ${translate(a, METHOD_PRECEDENCE)}.items)"
  }
  override def arrayMax(a: Ast.expr): String = {
    val t = detectType(a)
    val elType = t.asInstanceOf[ArrayType].elType
    s"_imp_std.mem.max(${ZigCompiler.kaitaiType2NativeType(elType, importList, provider.nowClass)}, ${translate(a, METHOD_PRECEDENCE)}.items)"
  }

  override def kaitaiStreamSize(value: Ast.expr): String =
    s"(try ${translate(value, METHOD_PRECEDENCE)}.size())"
  override def kaitaiStreamEof(value: Ast.expr): String =
    s"(try ${translate(value, METHOD_PRECEDENCE)}.isEof())"
  override def kaitaiStreamPos(value: Ast.expr): String =
    s"${translate(value, METHOD_PRECEDENCE)}.pos()"

  override def doInterpolatedStringLiteral(exprs: Seq[Ast.expr]): String =
    if (exprs.isEmpty) {
      doStringLiteral("")
    } else {
      val fmtString =
        exprs.map { expr =>
          detectType(expr) match {
            case _: IntType =>
              "{d}"
            case _: StrType =>
              "{s}"
            case otherType =>
              throw new UnsupportedOperationException(s"unable to convert $otherType to string in format string (only integers and strings are supported)")
          }
        }.mkString
      s"(try _imp_std.fmt.allocPrint(self._allocator(), \"$fmtString\", .{ ${exprs.map(translate).mkString(", ")} }))"
    }
}
