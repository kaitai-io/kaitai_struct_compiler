package io.kaitai.struct.translators

import io.kaitai.struct.{ImportList, Utils}
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format.{EnumSpec, Identifier}
import io.kaitai.struct.languages.JavaScriptCompiler

class JavaScriptTranslator(provider: TypeProvider, importList: ImportList) extends BaseTranslator(provider) {
  /**
    * Integer literals outside the `int32` and `uint32` ranges have a 64-bit
    * type (see [[TypeDetector.detectType]]), so they're translated as
    * `bigint` literals (e.g. `0x1_0000_0000` becomes `4294967296n`).
    */
  override def doIntLiteral(n: BigInt): String =
    if (n >= Int.MinValue && n <= Utils.MAX_UINT32) {
      n.toString
    } else {
      s"${n}n"
    }

  /**
    * Translates the integer expression `v` as a `bigint` if `asBigInt` is
    * true, and as a `number` otherwise. JavaScript doesn't allow mixing
    * `bigint` and `number` operands in arithmetic and bitwise operators (e.g.
    * `1n + 1` throws a `TypeError`), so operands must be converted.
    */
  def translateIntAs(v: Ast.expr, asBigInt: Boolean, extPrec: Int): String = {
    val isBigInt = detectType(v) match {
      case it: IntType => JavaScriptTranslator.isBigIntType(it)
      case _ => false
    }
    (isBigInt, asBigInt) match {
      case (false, true) =>
        TypeDetector.intLiteralValue(v) match {
          case Some(n) => s"${n}n"
          case None => s"BigInt(${translate(v)})"
        }
      case (true, false) =>
        TypeDetector.intLiteralValue(v) match {
          case Some(n) => n.toString
          case None => s"Number(${translate(v)})"
        }
      case _ =>
        translate(v, extPrec)
    }
  }

  override def translateBinaryOperand(operand: Ast.expr, binExpr: Ast.expr.BinOp, extPrec: Int): String =
    detectType(binExpr) match {
      case it: IntType => translateIntAs(operand, JavaScriptTranslator.isBigIntType(it), extPrec)
      // floats are `number`, so integer operands must be `number` too
      case _: FloatType => translateIntAs(operand, false, extPrec)
      case _ => translate(operand, extPrec)
    }

  /**
    * A `bigint` can't be shifted by a `number` or vice versa (e.g. `1n << 3`
    * throws a `TypeError`), so the shift distance is converted like the
    * other operators' operands.
    */
  override def translateShiftDistance(distance: Ast.expr, binExpr: Ast.expr.BinOp, extPrec: Int): String =
    translateBinaryOperand(distance, binExpr, extPrec)

  /**
    * `<<` and `~` give a signed 32-bit result for `number` operands (e.g.
    * `~0` is `-1`), and a signed result of unlimited width for `bigint`
    * operands (e.g. `~0n` is `-1n`), so unsigned results need to be
    * truncated.
    */
  override def truncateToUnsigned(expr: String, t: IntType): String =
    if (JavaScriptTranslator.isBigIntType(t)) {
      s"BigInt.asUintN(${t.bitWidth}, $expr)"
    } else {
      s"($expr >>> 0)"
    }

  // e.g. `u4_val.as<u8>` must give a `bigint`
  override def doCast(value: Ast.expr, typeName: DataType): String =
    translateAsType(value, typeName, METHOD_PRECEDENCE)

  /**
    * Translates `v` in a place where a value of type `t` is expected (e.g. an
    * argument for a parameter of type `t`). An integer is converted to a
    * `bigint` or a `number` if values of `t` are represented differently.
    * Other values are translated without a conversion.
    */
  def translateAsType(v: Ast.expr, t: DataType, extPrec: Int = 0): String =
    (detectType(v), t) match {
      case (_: IntType, it: IntType) => translateIntAs(v, JavaScriptTranslator.isBigIntType(it), extPrec)
      // floats are `number`
      case (_: IntType, _: FloatType) => translateIntAs(v, false, extPrec)
      case _ => translate(v, extPrec)
    }

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

  override def genericBinOp(left: Ast.expr, op: Ast.binaryop, right: Ast.expr, extPrec: Int) = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Div) =>
        val binExpr = Ast.expr.BinOp(left, Ast.operator.Div, right)
        detectType(binExpr) match {
          case it: IntType if JavaScriptTranslator.isBigIntType(it) =>
            // `/` on `bigint` operands rounds toward zero, but KS rounds down
            val leftStr = translateBinaryOperand(left, binExpr, 0)
            val rightStr = translateBinaryOperand(right, binExpr, 0)
            s"${JavaScriptCompiler.kstreamName}.divBigInt($leftStr, $rightStr)"
          case _ =>
            s"Math.floor(${super.genericBinOp(left, op, right, 0)})"
        }
      case (_: IntType, _: IntType, Ast.operator.Mod) =>
        val binExpr = Ast.expr.BinOp(left, Ast.operator.Mod, right)
        val leftStr = translateBinaryOperand(left, binExpr, 0)
        val rightStr = translateBinaryOperand(right, binExpr, 0)
        s"${JavaScriptCompiler.kstreamName}.mod($leftStr, $rightStr)"
      case (_: IntType, _: IntType, Ast.operator.RShift) =>
        detectType(Ast.expr.BinOp(left, Ast.operator.RShift, right)) match {
          // `>>>` is a logical shift (for `number` only, `bigint` throws a
          // `TypeError`). `CalcIntType` uses it too, because `>>` would make
          // values from `0x8000_0000` to `0xffff_ffff` negative.
          case CalcIntType | IntMultiType(false, Width4, _) =>
            genericBinOpStr(left, op, ">>>", right, extPrec)
          // `>>` is an arithmetic shift, and since `bigint` values of unsigned
          // types are never negative, it behaves like a logical shift for them
          case _ =>
            super.genericBinOp(left, op, right, extPrec)
        }
      case (_: IntType, _: IntType, bitOp: Ast.operator)
          if bitOp == Ast.operator.BitAnd || bitOp == Ast.operator.BitOr || bitOp == Ast.operator.BitXor =>
        detectType(Ast.expr.BinOp(left, bitOp, right)) match {
          // `&`, `|` and `^` give a signed 32-bit result for `number` operands
          // (e.g. `0x8000_0000 | 0` is `-0x8000_0000`). The operation must be
          // parenthesized, because these operators have a lower precedence
          // than `>>>` used by `truncateToUnsigned()`.
          case it @ IntMultiType(false, Width4, _) =>
            truncateToUnsigned(super.genericBinOp(left, op, right, OPERATOR_PRECEDENCE(Ast.operator.RShift)), it)
          case _ =>
            super.genericBinOp(left, op, right, extPrec)
        }
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
    // Just an integer, not resolved to an enum constant. The IDs of 64-bit
    // enums are `bigint` (`BigInt()` returns a `bigint` argument as is).
    if (JavaScriptTranslator.isBigIntType(enumSpec.intType)) s"BigInt($id)" else id

  override def doBytesCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr, extPrec: Int): String =
    s"(${JavaScriptCompiler.kstreamName}.byteArrayCompare(${translate(left)}, ${translate(right)}) ${cmpOp(op)} 0)"

  override def arraySubscript(container: expr, idx: expr): String =
    s"${translate(container)}[${translate(idx)}]"
  override def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String =
    detectType(Ast.expr.IfExp(condition, ifTrue, ifFalse)) match {
      // both branches must be converted to the result type, otherwise the
      // result could be a `number` or a `bigint` depending on the condition
      case it: IntType =>
        val asBigInt = JavaScriptTranslator.isBigIntType(it)
        s"(${translate(condition)} ? ${translateIntAs(ifTrue, asBigInt, 0)} : ${translateIntAs(ifFalse, asBigInt, 0)})"
      // integer branches of a float result must be `number`
      case _: FloatType =>
        s"(${translate(condition)} ? ${translateIntAs(ifTrue, false, 0)} : ${translateIntAs(ifFalse, false, 0)})"
      case _ =>
        s"(${translate(condition)} ? ${translate(ifTrue)} : ${translate(ifFalse)})"
    }

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
    * @see https://stackoverflow.com/questions/7820683/convert-boolean-result-into-number-integer
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
    * @see https://stackoverflow.com/a/596503/487064
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

  // https://stackoverflow.com/a/36525647/2055163
  override def strReverse(s: expr): String =
    s"Array.from(${translate(s)}).reverse().join('')"

  // `substring()` throws a `TypeError` for `bigint` arguments
  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"${translate(s, METHOD_PRECEDENCE)}.substring(${translateIntAs(from, false, 0)}, ${translateIntAs(to, false, 0)})"

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

object JavaScriptTranslator {
  /**
    * Checks if values of the integer type `t` are represented as `bigint`
    * instead of `number`. That's the case for 64-bit integers (`s8`, `u8`
    * and `b33`..`b64`), because `number` can't represent all of their values
    * exactly (and bitwise operators truncate `number` operands to 32-bit
    * integers).
    */
  def isBigIntType(t: IntType): Boolean =
    // `CalcIntType` is checked first, because its `bitWidth` isn't defined
    t != CalcIntType && t.bitWidth > 32
}
