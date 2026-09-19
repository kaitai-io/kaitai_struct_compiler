package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{ClassSpec, Identifier}
import io.kaitai.struct.languages.GoCompiler
import io.kaitai.struct.precompile.TypeMismatchError
import io.kaitai.struct.{ImportList, StringLanguageOutputWriter, Utils}
import io.kaitai.struct.format.SpecialIdentifier
import io.kaitai.struct.format.NamedIdentifier
import io.kaitai.struct.format.InstanceIdentifier

sealed trait TranslatorResult
case class ResultString(s: String) extends TranslatorResult
case class ResultLocalVar(n: Int) extends TranslatorResult

class GoTranslator(out: StringLanguageOutputWriter, provider: TypeProvider, importList: ImportList)
  extends TypeDetector(provider)
  with AbstractTranslator
  with CommonLiterals
  with CommonOps
  with CommonArraysAndCast[TranslatorResult]
  with CommonMethods[TranslatorResult]
  with ByteArraysAsTrueArrays[TranslatorResult] {

  import io.kaitai.struct.languages.GoCompiler._

  var returnRes: Option[String] = None

  /**
  * @see https://go.dev/ref/spec#Operator_precedence
  */
  override val OPERATOR_PRECEDENCE = Map[Ast.binaryop, Int](
    Ast.operator.Mult -> 130,
    Ast.operator.Div -> 130,
    Ast.operator.Mod -> 130,
    Ast.operator.LShift -> 130,
    Ast.operator.RShift -> 130,
    Ast.operator.BitAnd -> 130,
    Ast.operator.Add -> 120,
    Ast.operator.Sub -> 120,
    Ast.operator.BitXor -> 120,
    Ast.operator.BitOr -> 120,
    Ast.cmpop.Lt -> 110,
    Ast.cmpop.LtE -> 110,
    Ast.cmpop.Gt -> 110,
    Ast.cmpop.GtE -> 110,
    Ast.cmpop.Eq -> 110,
    Ast.cmpop.NotEq -> 110
  )

  override def translate(v: Ast.expr, extPrec: Int): String = resToStr(translateExpr(v, extPrec))

  def resToStr(r: TranslatorResult): String = r match {
    case ResultString(s) => s
    case ResultLocalVar(n) => localVarName(n)
  }

  def translateExpr(v: Ast.expr, extPrec: Int = 0): TranslatorResult = {
    v match {
      case Ast.expr.IntNum(n) =>
        trIntLiteral(n)
      case Ast.expr.FloatNum(n) =>
        trFloatLiteral(n)
      case Ast.expr.Str(s) =>
        trStringLiteral(s)
      case Ast.expr.InterpolatedStr(s) =>
        trInterpolatedStringLiteral(s)
      case Ast.expr.Bool(n) =>
        trBoolLiteral(n)
      case Ast.expr.EnumById(enumType, id, inType) =>
        val enumSpec = provider.resolveEnum(inType, enumType.name)
        trEnumById(enumSpec.name, translate(id))
      case Ast.expr.EnumByLabel(enumType, label, inType) =>
        val enumSpec = provider.resolveEnum(inType, enumType.name)
        trEnumByLabel(enumSpec.name, label.name)
      case Ast.expr.Name(name: Ast.identifier) =>
        if (name.name == Identifier.SIZEOF) {
          byteSizeOfClassSpec(provider.nowClass)
        } else {
          trLocalName(name.name)
        }
      case Ast.expr.InternalName(id: Identifier) =>
        trInternalName(id)
      case Ast.expr.UnaryOp(op: Ast.unaryop, inner: Ast.expr) =>
        val opStr = unaryOp(op)
        ResultString((op, inner) match {
          /** [[doIntLiteral]] has to know when a negative number is being translated - if it
           * doesn't, the result is things like `-uint32(2147483648)` that will not compile in Go
           * (the error is "constant -2147483648 overflows uint32") */
          case (Ast.unaryop.Minus, Ast.expr.IntNum(n)) => translate(Ast.expr.IntNum(-n))
          case (Ast.unaryop.Invert, Ast.expr.IntNum(_)) if !isUntypedNumeric(inner) && numericType(inner) != numericType(v) =>
            s"$opStr${numericType(v)}(${translateNumericAs(inner, numericType(v))})"
          case (_, Ast.expr.IntNum(_) | Ast.expr.FloatNum(_)) =>
            s"$opStr${translate(inner)}"
          case (Ast.unaryop.Minus | Ast.unaryop.Invert, _) if detectType(inner).isInstanceOf[IntType] =>
            s"$opStr(${translateNumericAs(inner, numericType(v))})"
          case _ =>
            s"$opStr(${translate(inner)})"
        })
      case Ast.expr.Compare(left, op, right) =>
        (detectType(left), detectType(right)) match {
          case (_: NumericType, _: NumericType) =>
            trNumericCompareOp(left, op, right, extPrec)
          case (_: StrType, _: StrType) =>
            trStrCompareOp(left, op, right, extPrec)
          case (_: BytesType, _: BytesType) =>
            trBytesCompareOp(left, op, right)
          case (_: BooleanType, _: BooleanType) =>
            trNumericCompareOp(left, op, right, extPrec)
          case (_: EnumType, _: EnumType) =>
            trNumericCompareOp(left, op, right, extPrec)
          case (ltype, rtype) =>
            throw new TypeMismatchError(s"can't do $ltype $op $rtype")
        }
      case Ast.expr.BinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) =>
        (detectType(left), detectType(right), op) match {
          case (_: NumericType, _: NumericType, _) =>
            trNumericBinOp(left, op, right, extPrec)
          case (_: StrType, _: StrType, Ast.operator.Add) =>
            trStrConcat(left, right, extPrec)
          case (ltype, rtype, _) =>
            throw new TypeMismatchError(s"can't do $ltype $op $rtype")
        }
      case Ast.expr.BoolOp(op, values) =>
        trBooleanOp(op, values)
      case Ast.expr.IfExp(condition, ifTrue, ifFalse) =>
        trIfExp(condition, ifTrue, ifFalse)
      case Ast.expr.Subscript(container, idx) =>
        arraySubscript(container, idx)
      case call: Ast.expr.Attribute =>
        translateAttribute(call)
      case call: Ast.expr.Call =>
        translateCall(call)
      case Ast.expr.List(elts) =>
        doGuessArrayLiteral(elts)
      case ctt: Ast.expr.CastToType =>
        doCastOrArray(ctt)
      case Ast.expr.ByteSizeOfType(typeName) =>
        doByteSizeOfType(typeName)
      case Ast.expr.BitSizeOfType(typeName) =>
        doBitSizeOfType(typeName)
    }
  }

  def trIntLiteral(n: BigInt): TranslatorResult = ResultString(doIntLiteral(n))
  def trFloatLiteral(n: BigDecimal): TranslatorResult = ResultString(doFloatLiteral(n))
  def trStringLiteral(s: String): TranslatorResult = ResultString(doStringLiteral(s))
  def trBoolLiteral(n: Boolean): TranslatorResult = ResultString(doBoolLiteral(n))

  def doByteSizeOfType(typeName: Ast.typeId): TranslatorResult = trIntLiteral(
    CommonSizeOf.bitToByteSize(
      CommonSizeOf.getBitsSizeOfType(
        typeName.nameAsStr, detectCastType(typeName)
      )
    )
  )
  def doBitSizeOfType(typeName: Ast.typeId): TranslatorResult = trIntLiteral(
    CommonSizeOf.getBitsSizeOfType(
      typeName.nameAsStr, detectCastType(typeName)
    )
  )
  def byteSizeOfClassSpec(cs: ClassSpec): TranslatorResult =
    trIntLiteral(CommonSizeOf.getByteSizeOfClassSpec(cs))

  def trBooleanOp(op: Ast.boolop, values: Seq[Ast.expr]): TranslatorResult = {
    // Translating an operand can emit statements (including error returns).
    // Keep those statements inside the operand's short-circuit branch, not
    // before the logical expression. Capture through the original writer so
    // compiler overrides and nested boolean expressions use the same scope.
    val operands = values.zipWithIndex.map { case (value, index) =>
      out.capture {
        if (index > 0) out.inc
        translate(value)
      }
    }
    if (operands.tail.forall(_._2.result.isEmpty)) {
      // No guarded prerequisites: retain the ordinary Go expression verbatim.
      out.add(operands.head._2)
      val expression = operands.map(_._1).mkString("(", s") ${booleanOp(op)} (", ")")
      ResultString(s" ($expression) ")
    } else {
      val result = allocateLocalVar()
      val name = localVarName(result)
      out.add(operands.head._2)
      out.puts(s"$name := ${operands.head._1}")
      operands.tail.foreach { case (expression, statements) =>
        val guard = if (op == Ast.boolop.And) name else s"!$name"
        out.puts(s"if $guard {")
        out.inc
        out.add(statements)
        out.puts(s"$name = $expression")
        out.dec
        out.puts("}")
      }
      ResultLocalVar(result)
    }
  }

  def trNumericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr, extPrec: Int): TranslatorResult = {
    if ((op == Ast.operator.Div || op == Ast.operator.Mod) && mixedSignedUnsigned(left, right))
      return trMixedIntegerDivision(left, op, right)
    val native = numericType(Ast.expr.BinOp(left, op, right))
    val precedence = OPERATOR_PRECEDENCE(op)
    val l = translateNumericAs(left, native, precedence)
    // Unlike the other binary operators, Go shifts do not require matching
    // operand types. The left operand determines the result's width.
    val r = if (op == Ast.operator.LShift || op == Ast.operator.RShift)
      translate(right, precedence) else translateNumericAs(right, native, precedence)
    val expr = s"$l ${binOp(op)} $r"
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Mod) =>
        val v1 = allocateLocalVar()
        out.puts(s"${localVarName(v1)} := $expr")
        if (!native.startsWith("uint")) {
          out.puts(s"if ${localVarName(v1)} < 0 {")
          out.inc
          out.puts(s"${localVarName(v1)} += $r")
          out.dec
          out.puts("}")
        }
        ResultLocalVar(v1)
      case _ =>
        ResultString(if (precedence <= extPrec) s"($expr)" else expr)
    }
  }

  private def mayBeNegative(value: Ast.expr): Boolean =
    numericType(value).startsWith("int") && integerLiteral(value).forall(_ < 0)

  private def mixedSignedUnsigned(left: Ast.expr, right: Ast.expr): Boolean =
    detectType(left).isInstanceOf[IntType] && detectType(right).isInstanceOf[IntType] &&
      commonNumericType(left, right) == "uint64" && (mayBeNegative(left) || mayBeNegative(right))

  private def trMixedIntegerDivision(left: Ast.expr, op: Ast.operator, right: Ast.expr): ResultLocalVar = {
    importList.add("errors")
    val l = translate(left)
    val r = translate(right)
    val signedLeft = numericType(left).startsWith("int")
    val signedRight = numericType(right).startsWith("int")
    // -(MinInt64) is not representable as int64. Negate one less than its
    // magnitude, then add the last unit in the unsigned domain instead.
    def magnitude(name: String, signed: Boolean): String =
      s"${name}Magnitude := uint64($name)\n" +
        (if (signed) s"if $name < 0 { ${name}Magnitude = uint64(-(int64($name) + 1)) + 1 }\n" else "")
    val resultType = if (op == Ast.operator.Div) "int64" else "uint64"
    val body = if (op == Ast.operator.Div) {
      val negative = if (signedLeft) "a < 0" else "b < 0"
      magnitude("a", signedLeft) + magnitude("b", signedRight) +
        s"""if bMagnitude == 0 { return 0, errors.New("integer division by zero") }
           |quotient := aMagnitude / bMagnitude
           |if $negative {
           |  if aMagnitude % bMagnitude != 0 { quotient++ }
           |  if quotient > uint64(1) << 63 { return 0, errors.New("integer division result is outside int64 range") }
           |  if quotient == uint64(1) << 63 { return -9223372036854775808, nil }
           |  return -int64(quotient), nil
           |}
           |if quotient > uint64(1) << 63 - 1 { return 0, errors.New("integer division result is outside int64 range") }
           |return int64(quotient), nil
           |""".stripMargin
    } else {
      val divisorCheck = if (signedRight) "b <= 0" else "b == 0"
      s"if $divisorCheck { return 0, errors.New(\"mod divisor <= 0\") }\n" +
        magnitude("a", signedLeft) +
        "remainder := aMagnitude % uint64(b)\n" +
        (if (signedLeft) "if a < 0 && remainder != 0 { remainder = uint64(b) - remainder }\n" else "") +
        "return remainder, nil\n"
    }
    // Parameters make side-effecting source operands execute exactly once.
    // Kaitai division rounds down (including negative non-exact quotients).
    // Mixed division can be negative, so its concrete result is int64. A
    // quotient outside that range returns an error, never a wrapped value;
    // an explicitly unsigned KSY division can instead retain uint64 results.
    outVarCheckRes(s"func(a ${numericType(left)}, b ${numericType(right)}) ($resultType, error) {\n$body}($l, $r)")
  }

  private def integerLiteral(value: Ast.expr): Option[BigInt] = value match {
    case Ast.expr.IntNum(n) => Some(n)
    case Ast.expr.UnaryOp(Ast.unaryop.Minus, Ast.expr.IntNum(n)) => Some(-n)
    case _ => None
  }

  private def integerRange(native: String): (BigInt, BigInt) = {
    val unsigned = native.startsWith("uint")
    val width = if (native == "int") 64 else native.drop(if (unsigned) 4 else 3).toInt
    if (unsigned) (BigInt(0), (BigInt(1) << width) - 1)
    else (-(BigInt(1) << (width - 1)), (BigInt(1) << (width - 1)) - 1)
  }

  /** The abstract CalcIntType does not retain the widths of operands. Track
    * the concrete Go expression type separately, without changing KSY types
    * or public value-instance signatures. Constants can adopt the other
    * operand's type when representable; typed values require conversions.
    */
  private def commonNumericType(left: Ast.expr, right: Ast.expr): String = {
    val l = numericType(left)
    val r = numericType(right)
    if (l == r) return l
    if (l.startsWith("float") || r.startsWith("float")) return "float64"
    def fits(value: Ast.expr, native: String): Boolean = integerLiteral(value).exists { n =>
      val (min, max) = integerRange(native)
      min <= n && n <= max
    }
    if (fits(left, r)) return r
    if (fits(right, l)) return l
    val (lmin, lmax) = integerRange(l)
    val (rmin, rmax) = integerRange(r)
    val min = lmin.min(rmin)
    val max = lmax.max(rmax)
    Seq("int8", "uint8", "int16", "uint16", "int32", "uint32", "int64", "uint64")
      .find { n => val (lo, hi) = integerRange(n); lo <= min && max <= hi }
      // No signed primitive can contain uint64's full range. Ring operations
      // use modulo 2^64; division, modulo and comparisons handle signs below.
      .getOrElse("uint64")
  }

  private def arithmeticType(native: String): String = native match {
    case "int8" | "uint8" | "int16" | "uint16" => "int"
    case "int32" | "uint32" => "int64"
    case "float32" => "float64"
    case other => other
  }

  def numericType(value: Ast.expr): String = value match {
    // The positive magnitude of MinInt64 needs uint64, but its negated result
    // is signed. This must agree with the whole-literal translation above.
    case Ast.expr.UnaryOp(Ast.unaryop.Minus, Ast.expr.IntNum(n)) => numericType(Ast.expr.IntNum(-n))
    case Ast.expr.IntNum(n) if n > Long.MaxValue => "uint64"
    case Ast.expr.IntNum(n) if n > Utils.MAX_UINT32 || n < Int.MinValue => "int64"
    case Ast.expr.IntNum(n) if n > Int.MaxValue => "uint32"
    case Ast.expr.Attribute(owner, Ast.identifier("pos" | "size"))
      if detectType(owner) == KaitaiStreamType || detectType(owner) == OwnedKaitaiStreamType => "int64"
    case Ast.expr.UnaryOp(_, inner) if detectType(inner).isInstanceOf[IntType] => arithmeticType(numericType(inner))
    case Ast.expr.BinOp(left, Ast.operator.Div, right) if mixedSignedUnsigned(left, right) => "int64"
    case Ast.expr.BinOp(left, Ast.operator.LShift | Ast.operator.RShift, _) => arithmeticType(numericType(left))
    case Ast.expr.BinOp(left, _, right) => arithmeticType(commonNumericType(left, right))
    case Ast.expr.IfExp(_, ifTrue, ifFalse) => commonNumericType(ifTrue, ifFalse)
    case _ => kaitaiType2NativeType(detectType(value))
  }

  private def isUntypedNumeric(value: Ast.expr): Boolean = value match {
    case Ast.expr.IntNum(n) => n >= Int.MinValue && n <= Int.MaxValue
    case Ast.expr.FloatNum(_) => true
    case Ast.expr.UnaryOp(_, inner) => isUntypedNumeric(inner)
    case Ast.expr.BinOp(left, op, right) if op != Ast.operator.Mod => isUntypedNumeric(left) && isUntypedNumeric(right)
    case _ => false
  }

  def translateNumericAs(value: Ast.expr, native: String, precedence: Int = 0): String = {
    if (!detectType(value).isInstanceOf[NumericType]) return translate(value, precedence)
    integerLiteral(value) match {
      case Some(n) => n.toString
      case None if isUntypedNumeric(value) => translate(value, precedence)
      case None if numericType(value) == native => translate(value, precedence)
      case None => s"$native(${translate(value)})"
    }
  }

  def translateAs(value: Ast.expr, target: DataType): String = target.asCombined match {
    case t: NumericType => translateNumericAs(value, kaitaiType2NativeType(t))
    case _ => translate(value)
  }

  def trStrConcat(left: Ast.expr, right: Ast.expr, extPrec: Int): TranslatorResult =
    ResultString(genericBinOp(left, Ast.operator.Add, right, extPrec))

  def trNumericCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr, extPrec: Int): TranslatorResult = {
    if (!detectType(left).isInstanceOf[NumericType] || !detectType(right).isInstanceOf[NumericType])
      return ResultString(doNumericCompareOp(left, op, right, extPrec))
    val native = commonNumericType(left, right)
    val signedLeft = mayBeNegative(left)
    val signedRight = mayBeNegative(right)
    if (native == "uint64" && (signedLeft || signedRight)) {
      // Casting a negative signed value to uint64 before comparison reverses
      // its order. Function parameters evaluate each operand exactly once.
      val l = translate(left)
      val r = translate(right)
      val signed = if (signedLeft) "a" else "b"
      val negativeResult = op match {
        case Ast.cmpop.NotEq => true
        case Ast.cmpop.Eq => false
        case Ast.cmpop.Lt | Ast.cmpop.LtE => signedLeft
        case Ast.cmpop.Gt | Ast.cmpop.GtE => signedRight
      }
      return ResultString(s"func(a ${numericType(left)}, b ${numericType(right)}) bool { if $signed < 0 { return $negativeResult }; return uint64(a) ${cmpOp(op)} uint64(b) }($l, $r)")
    }
    val precedence = OPERATOR_PRECEDENCE(op)
    val l = translateNumericAs(left, native, precedence)
    val r = translateNumericAs(right, native, precedence)
    val expr = s"$l ${cmpOp(op)} $r"
    ResultString(if (precedence <= extPrec) s"($expr)" else expr)
  }

  def trStrCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr, extPrec: Int): TranslatorResult =
    ResultString(doStrCompareOp(left, op, right, extPrec))

  def trBytesCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): TranslatorResult = {
    importList.add("bytes")
    op match {
      case Ast.cmpop.Eq =>
        ResultString(s"bytes.Equal(${translate(left)}, ${translate(right)})")
      case _ =>
        ResultString(s"(bytes.Compare(${translate(left)}, ${translate(right)}) ${cmpOp(op)} 0)")
    }
  }

  override def doIntLiteral(n: BigInt): String = {
    if (n < -9223372036854775808L) {
      s"$n" // too low, no type conversion would help anyway
    } else if (n <= -2147483649L) {
      s"int64($n)" // -9223372036854775808..-2147483649
    } else if (n <= 2147483647L) {
      s"$n" // -2147483648..2147483647
    } else if (n <= 4294967295L) {
      s"uint32($n)" // 2147483648..4294967295
    } else if (n <= 9223372036854775807L) {
      s"int64($n)" // 4294967296..9223372036854775807
    } else if (n <= Utils.MAX_UINT64) {
      s"uint64($n)" // 9223372036854775808..18446744073709551615
    } else {
      s"$n" // too high, no type conversion would help anyway
    }
  }

  override def unaryOp(op: Ast.unaryop): String = op match {
    case Ast.unaryop.Invert => "^"
    case Ast.unaryop.Minus => "-"
    case Ast.unaryop.Not => "!"
  }

  def trLocalName(s: String): TranslatorResult = {
    s match {
      case Identifier.ROOT |
           Identifier.PARENT |
           Identifier.IO =>
        ResultString(s"this.${specialName(s)}")

      // These can be local only
      case Identifier.ITERATOR |
           Identifier.ITERATOR2 =>
        ResultString(specialName(s))
      case Identifier.INDEX => ResultString("i")

      case _ =>
        if (provider.isLazy(s)) {
          outVarCheckRes(s"this.${Utils.upperCamelCase(s)}()")
        } else {
          ResultString(s"this.${Utils.upperCamelCase(s)}")
        }
    }
  }

  def trInternalName(id: Identifier): TranslatorResult =
    ResultString(GoCompiler.privateMemberName(id))

  def specialName(id: String): String = id match {
    case Identifier.ROOT | Identifier.PARENT | Identifier.IO =>
      id
    case Identifier.ITERATOR =>
      "_it"
    case Identifier.ITERATOR2 =>
      "_buf"
  }

  def arraySubscript(container: Ast.expr, idx: Ast.expr) =
    ResultString(s"${translate(container)}[${translate(idx)}]")

  def trIfExp(condition: Ast.expr, ifTrue: Ast.expr, ifFalse: Ast.expr): ResultLocalVar = {
    val v1 = allocateLocalVar()
    val typ = detectType(ifTrue)
    val native = typ match {
      case _: NumericType => commonNumericType(ifTrue, ifFalse)
      case _ => GoCompiler.kaitaiType2NativeType(typ)
    }
    out.puts(s"var ${localVarName(v1)} $native;")
    out.puts(s"if (${translate(condition)}) {")
    out.inc
    out.puts(s"${localVarName(v1)} = ${translateNumericAs(ifTrue, native)}")
    out.dec
    out.puts("} else {")
    out.inc
    out.puts(s"${localVarName(v1)} = ${translateNumericAs(ifFalse, native)}")
    out.dec
    out.puts("}")
    ResultLocalVar(v1)
  }

  def trEnumByLabel(enumTypeAbs: List[String], label: String) =
    ResultString(GoCompiler.enumToStr(enumTypeAbs, label))
  def trEnumById(enumTypeAbs: List[String], id: String) =
    ResultString(s"${types2class(enumTypeAbs)}($id)")

  override def doBytesCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr, extPrec: Int): String = {
    op match {
      case Ast.cmpop.Eq =>
        s"Arrays.equals(${translate(left)}, ${translate(right)})"
      case Ast.cmpop.NotEq =>
        s"!Arrays.equals(${translate(left)}, ${translate(right)})"
      case _ =>
        s"(${GoCompiler.kstreamName}.byteArrayCompare(${translate(left)}, ${translate(right)}) ${cmpOp(op)} 0)"
    }
  }

  override def doCast(value: Ast.expr, typeName: DataType): TranslatorResult = {
    val valueType = detectType(value)
    valueType match {
      case KaitaiStructType | CalcKaitaiStructType(_) | AnyType =>
        // Type assertion - only works on interfaces, otherwise a syntax error would occur
        ResultString(s"${translate(value, METHOD_PRECEDENCE)}.(${GoCompiler.kaitaiType2NativeType(typeName)})")
      // These data types are represented as pointer types (`*T`) in the generated Go
      // code, so they have to be wrapped into parentheses, otherwise Go will attempt to
      // apply `*` to the result of `T(...)` instead of the desired `(*T)(...)`.
      case KaitaiStreamType | OwnedKaitaiStreamType | _: UserType =>
        ResultString(s"(${GoCompiler.kaitaiType2NativeType(typeName)})(${translate(value)})")
      case _ =>
        // Type conversion - for everything else that is not an interface
        ResultString(s"${GoCompiler.kaitaiType2NativeType(typeName)}(${translate(value)})")
    }
  }

  override def doArrayLiteral(t: DataType, value: Seq[Ast.expr]) =
    ResultString(s"[]${GoCompiler.kaitaiType2NativeType(t)}{${value.map(translate).mkString(", ")}}")

  override def doByteArrayLiteral(arr: Seq[Byte]): TranslatorResult =
    ResultString("[]uint8{" + arr.map(_ & 0xff).mkString(", ") + "}")

  override def doByteArrayNonLiteral(elts: Seq[Ast.expr]): TranslatorResult =
    ResultString("[]uint8{" + elts.map(translateNumericAs(_, "uint8")).mkString(", ") + "}")

  // Predefined methods of various types

  val IMPORT_CHARMAP = "golang.org/x/text/encoding/charmap"

  val ENCODINGS = Map(
    "UTF-16BE" -> ("unicode.UTF16(unicode.BigEndian, unicode.IgnoreBOM)", "golang.org/x/text/encoding/unicode"),
    "UTF-16LE" -> ("unicode.UTF16(unicode.LittleEndian, unicode.IgnoreBOM)", "golang.org/x/text/encoding/unicode"),
    "UTF-32BE" -> ("utf32.UTF32(utf32.BigEndian, utf32.IgnoreBOM)", "golang.org/x/text/encoding/unicode/utf32"),
    "UTF-32LE" -> ("utf32.UTF32(utf32.LittleEndian, utf32.IgnoreBOM)", "golang.org/x/text/encoding/unicode/utf32"),
    "ISO-8859-1"  -> ("charmap.ISO8859_1", IMPORT_CHARMAP),
    "ISO-8859-2"  -> ("charmap.ISO8859_2", IMPORT_CHARMAP),
    "ISO-8859-3"  -> ("charmap.ISO8859_3", IMPORT_CHARMAP),
    "ISO-8859-4"  -> ("charmap.ISO8859_4", IMPORT_CHARMAP),
    "ISO-8859-5"  -> ("charmap.ISO8859_5", IMPORT_CHARMAP),
    "ISO-8859-6"  -> ("charmap.ISO8859_6", IMPORT_CHARMAP),
    "ISO-8859-7"  -> ("charmap.ISO8859_7", IMPORT_CHARMAP),
    "ISO-8859-8"  -> ("charmap.ISO8859_8", IMPORT_CHARMAP),
    "ISO-8859-9"  -> ("charmap.ISO8859_9", IMPORT_CHARMAP),
    "ISO-8859-10" -> ("charmap.ISO8859_10", IMPORT_CHARMAP),
    // The same note as in https://github.com/kaitai-io/kaitai_struct_cpp_stl_runtime/blob/07ff9cf91e8bdf3515c0efdda0a879c0021b5edb/kaitai/kaitaistream.cpp#L918-L922
    // applies here
    "ISO-8859-11" -> ("charmap.Windows874", IMPORT_CHARMAP),
    "ISO-8859-13" -> ("charmap.ISO8859_13", IMPORT_CHARMAP),
    "ISO-8859-14" -> ("charmap.ISO8859_14", IMPORT_CHARMAP),
    "ISO-8859-15" -> ("charmap.ISO8859_15", IMPORT_CHARMAP),
    "ISO-8859-16" -> ("charmap.ISO8859_16", IMPORT_CHARMAP),
    "windows-1250" -> ("charmap.Windows1250", IMPORT_CHARMAP),
    "windows-1251" -> ("charmap.Windows1251", IMPORT_CHARMAP),
    "windows-1252" -> ("charmap.Windows1252", IMPORT_CHARMAP),
    "windows-1253" -> ("charmap.Windows1253", IMPORT_CHARMAP),
    "windows-1254" -> ("charmap.Windows1254", IMPORT_CHARMAP),
    "windows-1255" -> ("charmap.Windows1255", IMPORT_CHARMAP),
    "windows-1256" -> ("charmap.Windows1256", IMPORT_CHARMAP),
    "windows-1257" -> ("charmap.Windows1257", IMPORT_CHARMAP),
    "windows-1258" -> ("charmap.Windows1258", IMPORT_CHARMAP),
    "IBM437" -> ("charmap.CodePage437", IMPORT_CHARMAP),
    "IBM866" -> ("charmap.CodePage866", IMPORT_CHARMAP),
    "Shift_JIS" -> ("japanese.ShiftJIS", "golang.org/x/text/encoding/japanese"),
    "Big5" -> ("traditionalchinese.Big5", "golang.org/x/text/encoding/traditionalchinese"),
    "EUC-KR" -> ("korean.EUCKR", "golang.org/x/text/encoding/korean"),
  )

  override def bytesToStr(value: Ast.expr, encoding: String): TranslatorResult =
    bytesToStr(translate(value), encoding)

  override def strToBytes(value: Ast.expr, encoding: Ast.expr): TranslatorResult = encoding match {
    case Ast.expr.Str("UTF-8") => ResultString(s"[]byte(${translate(value)})")
    case Ast.expr.Str("ASCII") =>
      importList.add("fmt")
      def fieldName(expr: Ast.expr): String = expr match {
        case Ast.expr.InternalName(id) => id.humanReadable
        case Ast.expr.Name(id) => id.name
        case Ast.expr.Subscript(container, _) => fieldName(container)
        case Ast.expr.CastToType(inner, _) => fieldName(inner)
        case _ => "string"
      }
      val context = doStringLiteral(fieldName(value))
      // A Go string contains UTF-8 bytes, not necessarily ASCII. Validate before
      // returning bytes so both consistency lengths and writes see the error.
      outVarCheckRes(
        s"""func(value string) ([]byte, error) {
           |\tfor offset, codePoint := range value {
           |\t\tif codePoint > 127 {
           |\t\t\treturn nil, fmt.Errorf("encode ASCII field %s at byte offset %d: %w", $context, offset, kaitai.NewConsistencyError("ASCII", rune(127), codePoint))
           |\t\t}
           |\t}
           |\treturn []byte(value), nil
           |}(${translate(value)})""".stripMargin)
    case Ast.expr.Str(enc) =>
      ENCODINGS.get(enc) match {
        case Some((encoder, importName)) =>
          importList.add(importName)
          outVarCheckRes(s"$encoder.NewEncoder().Bytes([]byte(${translate(value)}))")
        case None => throw new RuntimeException(s"encoding '$enc' is not supported in Go")
      }
    case _ => throw new RuntimeException("Go string serialization requires a literal encoding")
  }

  override def bytesIndexOf(value: Ast.expr, byte: Ast.expr): TranslatorResult = {
    importList.add("bytes")
    ResultString(s"bytes.IndexByte(${translate(value)}, byte(${translate(byte)}))")
  }

  def bytesToStr(bytesExpr: String, encoding: String): TranslatorResult = {
    encoding match {
      case "ASCII" | "UTF-8" =>
        // no conversion
        // FIXME: may be add some checks for valid ASCII/UTF-8
        ResultString(s"string($bytesExpr)")
      case encStr =>
        ENCODINGS.get(encStr) match {
          case Some((decoderSrc, importName)) =>
            importList.add(importName)
            outVarCheckRes(s"kaitai.BytesToStr($bytesExpr, $decoderSrc.NewDecoder())")
          case None =>
            throw new RuntimeException(s"encoding '$encStr' in not supported in Go")
        }
    }
  }

//  override def strReverse(s: Ast.expr): String =
//    s"new StringBuilder(${translate(s)}).reverse().toString()"
//  override def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): String =
//    s"${translate(s)}.substring(${translate(from)}, ${translate(to)})"

  override def arrayFirst(a: Ast.expr): TranslatorResult =
    ResultString(s"${translate(a)}[0]")
  override def arrayLast(a: Ast.expr): ResultString = {
    val v = allocateLocalVar()
    out.puts(s"${localVarName(v)} := ${translate(a)}")
    ResultString(s"${localVarName(v)}[len(${localVarName(v)}) - 1]")
  }
  override def arraySize(a: Ast.expr): TranslatorResult =
    ResultString(s"len(${translate(a)})")
//  override def arrayMin(a: Ast.expr): String =
//    s"Collections.min(${translate(a)})"
//  override def arrayMax(a: Ast.expr): String =
//    s"Collections.max(${translate(a)})"

  override def userTypeField(ut: UserType, value: Ast.expr, name: String): TranslatorResult = {
    val valueStr = translate(value)

    val (call, twoOuts) = name match {
      case Identifier.ROOT |
           Identifier.PARENT |
           Identifier.IO =>
        (specialName(name), false)
      case _ =>
        (Utils.upperCamelCase(name), provider.isLazy(ut.classSpec.get, name))
    }

    if (twoOuts) {
      outVarCheckRes(s"$valueStr.$call()")
    } else {
      ResultString(s"$valueStr.$call")
    }
  }
  def kaitaiStructField(value: Ast.expr, name: String): TranslatorResult = {
    val valueStr = translate(value)

    val (call, twoOuts) = name match {
      case Identifier.ROOT |
           Identifier.PARENT |
           Identifier.IO =>
        (specialName(name), false)
    }

    if (twoOuts) {
      outVarCheckRes(s"$valueStr.$call()")
    } else {
      ResultString(s"$valueStr.$call")
    }
  }

  override def strLength(s: Ast.expr): TranslatorResult = {
    importList.add("unicode/utf8")
    ResultString(s"utf8.RuneCountInString(${translate(s)})")
  }

  override def strReverse(s: Ast.expr): TranslatorResult = {
    ResultString(s"kaitai.StringReverse(${translate(s)})")
  }

  override def strToInt(s: Ast.expr, base: Ast.expr): TranslatorResult = {
    importList.add("strconv")
    outVarCheckRes(s"strconv.ParseInt(${translate(s)}, ${translate(base)}, 0)")
  }

  override def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): TranslatorResult =
    ResultString(s"${translate(s, METHOD_PRECEDENCE)}[${translate(from)}:${translate(to)}]")

  override def intToStr(value: Ast.expr): TranslatorResult = {
    importList.add("strconv")
    ResultString(s"strconv.FormatInt(int64(${translate(value)}), 10)")
  }

  override def floatToInt(value: Ast.expr) =
    ResultString(s"int(${translate(value)})")

  override def kaitaiStreamSize(value: Ast.expr): TranslatorResult =
    outVarCheckRes(s"${translate(value, METHOD_PRECEDENCE)}.Size()")

  override def kaitaiStreamEof(value: Ast.expr): TranslatorResult =
    outVarCheckRes(s"${translate(value, METHOD_PRECEDENCE)}.EOF()")

  override def kaitaiStreamPos(value: Ast.expr) =
    outVarCheckRes(s"${translate(value, METHOD_PRECEDENCE)}.Pos()")

  override def arrayMin(a: Ast.expr): ResultLocalVar = {
    val min = allocateLocalVar()
    val value = allocateLocalVar()
    out.puts(s"${localVarName(min)} := ${translate(a)}[0]")
    out.puts(s"for _, ${localVarName(value)} := range ${translate(a)} {")
    out.inc
    out.puts(s"if ${localVarName(min)} > ${localVarName(value)} {")
    out.inc
    out.puts(s"${localVarName(min)} = ${localVarName(value)}")
    out.dec
    out.puts("}")
    out.dec
    out.puts("}")
    ResultLocalVar(min)
  }

  override def arrayMax(a: Ast.expr): ResultLocalVar = {
    val max = allocateLocalVar()
    val value = allocateLocalVar()
    out.puts(s"${localVarName(max)} := ${translate(a)}[0]")
    out.puts(s"for _, ${localVarName(value)} := range ${translate(a)} {")
    out.inc
    out.puts(s"if ${localVarName(max)} < ${localVarName(value)} {")
    out.inc
    out.puts(s"${localVarName(max)} = ${localVarName(value)}")
    out.dec
    out.puts("}")
    out.dec
    out.puts("}")
    ResultLocalVar(max)
  }

  override def enumToInt(value: Ast.expr, et: EnumType) =
    translateExpr(value)

  override def boolToInt(value: Ast.expr): ResultLocalVar = {
    val v = allocateLocalVar()
    out.puts(s"${localVarName(v)} := 0")
    out.puts(s"if ${translate(value)} {")
    out.inc
    out.puts(s"${localVarName(v)} = 1")
    out.dec
    out.puts("}")
    ResultLocalVar(v)
  }

  def trInterpolatedStringLiteral(exprs: Seq[Ast.expr]): TranslatorResult = {
    exprs match {
      case Seq(Ast.expr.Str(s)) =>
        // exactly one string literal, no need for printf at all
        trStringLiteral(s)

      case _ =>
        importList.add("fmt")

        val piecesAndArgs: Seq[(String, Option[String])] = exprs.map {
          case Ast.expr.Str(s) =>
            // This string will be used as format string, so we need to escape all `%` as `%%`
            val escapedFmtStr = s.replace("%", "%%")
            (doStringLiteralBody(escapedFmtStr), None)
          case e =>
            ("%v", Some(translate(e)))
        }

        val fmtString = piecesAndArgs.map(x => x._1).mkString
        val fmtArgs = piecesAndArgs.flatMap(x => x._2)

        ResultString("fmt.Sprintf(\"" + fmtString + "\", " + fmtArgs.mkString(", ") + ")")
    }
  }

  def outVarCheckRes(expr: String): ResultLocalVar = {
    val v1 = allocateLocalVar()
    out.puts(s"${localVarName(v1)}, err := $expr")
    outAddErrCheck()
    ResultLocalVar(v1)
  }

  def outTransform(id: ResultLocalVar, expr: String): ResultLocalVar = {
    out.puts(s"${resToStr(id)} = $expr")
    id
  }

  private
  var localVarNum = 0

  def allocateLocalVar(): Int = {
    localVarNum += 1
    localVarNum
  }

  def localVarName(n: Int) = s"tmp$n"

  def outAddErrCheck(): Unit = {
    out.puts("if err != nil {")
    out.inc

    val noValueAndErr = returnRes match {
      case None => "err"
      case Some(r) => s"$r, err"
    }

    out.puts(s"return $noValueAndErr")
    out.dec
    out.puts("}")
  }

  override def byteSizeOfValue(attrName: String, valType: DataType): TranslatorResult =
    trIntLiteral(CommonSizeOf.bitToByteSize(CommonSizeOf.getBitsSizeOfType(attrName, valType)))
}
