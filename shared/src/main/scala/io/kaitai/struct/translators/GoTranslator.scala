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
          case (_, Ast.expr.IntNum(_) | Ast.expr.FloatNum(_)) =>
            s"$opStr${translate(inner)}"
          case _ =>
            s"$opStr(${translate(inner)})"
        })
      case Ast.expr.Compare(left, op, right) =>
        (detectType(left), detectType(right)) match {
          case (_: NumericType, _: NumericType) =>
            trNumericCompareOp(left, op, right)
          case (_: StrType, _: StrType) =>
            trStrCompareOp(left, op, right)
          case (_: BytesType, _: BytesType) =>
            trBytesCompareOp(left, op, right)
          case (_: BooleanType, _: BooleanType) =>
            trNumericCompareOp(left, op, right)
          case (_: EnumType, _: EnumType) =>
            trNumericCompareOp(left, op, right)
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

  def trBooleanOp(op: Ast.boolop, values: Seq[Ast.expr]) =
    ResultString(doBooleanOp(op, values))

  def trNumericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr, extPrec: Int): TranslatorResult = {
    (detectType(left), detectType(right), op) match {
      case (t1: IntType, t2: IntType, Ast.operator.Mod) =>
        val v1 = allocateLocalVar()
        out.puts(s"${localVarName(v1)} := ${translate(left)} % ${translate(right)}")
        out.puts(s"if ${localVarName(v1)} < 0 {")
        out.inc
        out.puts(s"${localVarName(v1)} += ${translate(right)}")
        out.dec
        out.puts("}")
        ResultLocalVar(v1)
      case _ =>
        ResultString(genericBinOp(left, op, right, extPrec))
    }
  }

  def trStrConcat(left: Ast.expr, right: Ast.expr, extPrec: Int): TranslatorResult =
    ResultString(genericBinOp(left, Ast.operator.Add, right, extPrec))

  def trNumericCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): TranslatorResult =
    ResultString(doNumericCompareOp(left, op, right))

  def trStrCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): TranslatorResult =
    ResultString(doStrCompareOp(left, op, right))

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
    out.puts(s"var ${localVarName(v1)} ${GoCompiler.kaitaiType2NativeType(typ)};")
    out.puts(s"if (${translate(condition)}) {")
    out.inc
    out.puts(s"${localVarName(v1)} = ${translate(ifTrue)}")
    out.dec
    out.puts("} else {")
    out.inc
    out.puts(s"${localVarName(v1)} = ${translate(ifFalse)}")
    out.dec
    out.puts("}")
    ResultLocalVar(v1)
  }

  def trEnumByLabel(enumTypeAbs: List[String], label: String) =
    ResultString(GoCompiler.enumToStr(enumTypeAbs, label))
  def trEnumById(enumTypeAbs: List[String], id: String) =
    ResultString(s"${types2class(enumTypeAbs)}($id)")

  override def doBytesCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): String = {
    op match {
      case Ast.cmpop.Eq =>
        s"Arrays.equals(${translate(left)}, ${translate(right)})"
      case Ast.cmpop.NotEq =>
        s"!Arrays.equals(${translate(left)}, ${translate(right)})"
      case _ =>
        s"(${GoCompiler.kstreamName}.byteArrayCompare(${translate(left)}, ${translate(right)}) ${cmpOp(op)} 0)"
    }
  }

  override def doCast(value: Ast.expr, typeName: DataType): TranslatorResult = ???

  override def doArrayLiteral(t: DataType, value: Seq[Ast.expr]) =
    ResultString(s"[]${GoCompiler.kaitaiType2NativeType(t)}{${value.map(translate).mkString(", ")}}")

  override def doByteArrayLiteral(arr: Seq[Byte]): TranslatorResult =
    ResultString("[]uint8{" + arr.map(_ & 0xff).mkString(", ") + "}")

  override def doByteArrayNonLiteral(elts: Seq[Ast.expr]): TranslatorResult =
    ResultString("[]uint8{" + elts.map(translate).mkString(", ") + "}")

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

  override def kaitaiStreamSize(value: Ast.expr) =
    outVarCheckRes(s"${translate(value, METHOD_PRECEDENCE)}.Size()")

  override def kaitaiStreamEof(value: Ast.expr) =
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
