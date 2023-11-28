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


class GoTranslator(out: StringLanguageOutputWriter, provider: TypeProvider, importList: ImportList) extends BaseTranslator(provider) {
  import io.kaitai.struct.languages.GoCompiler._

  var returnRes: Option[String] = None

  override def doByteSizeOfType(typeName: Ast.typeId): String = doIntLiteral(
    CommonSizeOf.bitToByteSize(
      CommonSizeOf.getBitsSizeOfType(
        typeName.nameAsStr, detectCastType(typeName)
      )
    )
  )
  override def doBitSizeOfType(typeName: Ast.typeId): String = doIntLiteral(
    CommonSizeOf.getBitsSizeOfType(
      typeName.nameAsStr, detectCastType(typeName)
    )
  )
  override def byteSizeOfClassSpec(cs: ClassSpec): String =
    doIntLiteral(CommonSizeOf.getByteSizeOfClassSpec(cs))

  override def bytesIndexOf(b: Ast.expr, byte: Ast.expr): String = {
    importList.add("bytes")
    s"bytes.IndexByte(${translate(b)}, ${translate(byte)})"
  }

  override def numericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr): String = {
    (detectType(left), detectType(right), op) match {
      case (t1: IntType, t2: IntType, Ast.operator.Mod) =>
        s"(${translate(left)} % ${translate(right)} + ${translate(right)}) % ${translate(right)}"
      case _ =>
        s"(${translate(left)} ${binOp(op)} ${translate(right)})"
    }
  }

  override def strConcat(left: Ast.expr, right: Ast.expr): String = translate(left) + " + " + translate(right)

  override def doBytesCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): String = {
    importList.add("bytes")
    op match {
      case Ast.cmpop.Eq =>
        s"bytes.Equal(${translate(left)}, ${translate(right)})"
      case _ =>
        "(bytes.Compare(${translate(left)}, ${translate(right)}) ${cmpOp(op)} 0)"
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

  override def doName(s: String): String = Utils.upperCamelCase(s)

  override def doLocalName(s: String): String = {
    s match {
      case Identifier.ROOT |
           Identifier.PARENT |
           Identifier.IO =>
        s"this.${specialName(s)}"

      // These can be local only
      case Identifier.ITERATOR |
           Identifier.ITERATOR2 =>
        specialName(s)
      case Identifier.INDEX => "i"

      case _ =>
        if (provider.isLazy(s)) {
          outVarCheckRes(s"this.${doName(s)}()")
        } else {
          s"this.${doName(s)}"
        }
    }
  }

  override def doInternalName(id: Identifier): String  =
    id match {
      case SpecialIdentifier(name) => doLocalName(name)
      case NamedIdentifier(name) => doLocalName(name)
      case InstanceIdentifier(name) => doLocalName(name)
      case _ => s"this.${GoCompiler.publicMemberName(id)}"
    }

  def specialName(id: String): String = id match {
    case Identifier.ROOT | Identifier.PARENT | Identifier.IO =>
      id
    case Identifier.ITERATOR =>
      "_it"
    case Identifier.ITERATOR2 =>
      "_buf"
  }

  override def arraySubscript(container: Ast.expr, idx: Ast.expr) = s"${translate(container)}[${translate(idx)}]"

  override def doIfExp(condition: Ast.expr, ifTrue: Ast.expr, ifFalse: Ast.expr): String = {
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
    localVarName(v1)
  }

  override def doEnumByLabel(enumTypeAbs: List[String], label: String) = GoCompiler.enumToStr(enumTypeAbs, label)
  override def doEnumById(enumTypeAbs: List[String], id: String) = s"${types2class(enumTypeAbs)}($id)"

  override def doCast(value: Ast.expr, typeName: DataType): String = ???

  override def doArrayLiteral(t: DataType, value: Seq[Ast.expr]) =
    s"[]${GoCompiler.kaitaiType2NativeType(t)}{${value.map(translate).mkString(", ")}}"

  override def doByteArrayLiteral(arr: Seq[Byte]): String =
    "[]uint8{" + arr.map(_ & 0xff).mkString(", ") + "}"

  override def doByteArrayNonLiteral(elts: Seq[Ast.expr]): String =
    "[]uint8{" + elts.map(translate).mkString(", ") + "}"

  // Predefined methods of various types

  val IMPORT_CHARMAP = "golang.org/x/text/encoding/charmap"

  val ENCODINGS = Map(
    "IBM437" -> ("charmap.CodePage437", IMPORT_CHARMAP),
    "ISO-8859-1" -> ("charmap.ISO8859_1", IMPORT_CHARMAP),
    "ISO-8859-2" -> ("charmap.ISO8859_2", IMPORT_CHARMAP),
    "ISO-8859-3" -> ("charmap.ISO8859_3", IMPORT_CHARMAP),
    "ISO-8859-4" -> ("charmap.ISO8859_4", IMPORT_CHARMAP),
    "SJIS" -> ("japanese.ShiftJIS", "golang.org/x/text/encoding/japanese"),
    "BIG5" -> ("traditionalchinese.Big5", "golang.org/x/text/encoding/traditionalchinese"),
    "UTF-16LE" -> ("unicode.UTF16(unicode.LittleEndian, unicode.IgnoreBOM)", "golang.org/x/text/encoding/unicode"),
    "UTF-16BE" -> ("unicode.UTF16(unicode.BigEndian, unicode.IgnoreBOM)", "golang.org/x/text/encoding/unicode")
  )

  override def bytesToStr(value: String, encoding: String): String = {
    encoding match {
      case "ASCII" | "UTF-8" =>
        // no conversion
        // FIXME: may be add some checks for valid ASCII/UTF-8
        s"string($value)"
      case encStr =>
        ENCODINGS.get(encStr) match {
          case Some((decoderSrc, importName)) =>
            importList.add(importName)
            outVarCheckRes(s"kaitai.BytesToStr($value, $decoderSrc.NewDecoder())")
          case None =>
            throw new RuntimeException(s"encoding '$encStr' in not supported in Go")
        }
    }
  }

  override def strToBytes(value: Ast.expr, encoding: Ast.expr): String = {
    val strExpr = translate(value)
    val encodingExpr = translate(encoding)
    encodingExpr match {
      case "ASCII" | "UTF-8" =>
        // no conversion
        s"[]byte($strExpr)"
      case encStr =>
        ENCODINGS.get(encStr) match {
          case Some((encoderSrc, importName)) =>
            importList.add(importName)
            outVarCheckRes(s"kaitai.StrToBytes($strExpr, $encoderSrc.NewEncoder())")
          case None =>
            throw new RuntimeException(s"encoding '$encStr' in not supported in Go")
        }
    }
  }

  override def strReverse(s: Ast.expr): String = {
    s"kaitai.StringReverse(${translate(s)})"
  }

  override def strToInt(s: Ast.expr, base: Ast.expr): String = {
    importList.add("strconv")
    outVarCheckRes(s"strconv.ParseInt(${translate(s)}, ${translate(base)}, 0)")
  }

  override def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): String = {
    s"${translate(s)}[${translate(from)}:${translate(to)}]"
  }

  override def arrayFirst(a: Ast.expr): String = s"${translate(a)}[0]"

  override def arrayLast(a: Ast.expr): String = s"${translate(a)}[len(${translate(a)}) - 1]"

  override def arraySize(a: Ast.expr): String = s"len(${translate(a)})"

  override def arrayMin(a: Ast.expr): String = {
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
    localVarName(min)
  }

  override def arrayMax(a: Ast.expr): String = {
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
    localVarName(max)
  }


  override def userTypeField(ut: UserType, value: Ast.expr, name: String): String = {
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
      s"$valueStr.$call"
    }
  }

  override def kaitaiStructField(value: Ast.expr, name: String): String = {
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
      s"$valueStr.$call"
    }
  }

  override def strLength(s: Ast.expr): String = {
    importList.add("unicode/utf8")
    s"utf8.RuneCountInString(${translate(s)})"
  }

  override def intToStr(value: Ast.expr, base: Ast.expr): String = {
    importList.add("strconv")
    s"strconv.FormatInt(int64(${translate(value)}), ${translate(base)})"
  }

  override def floatToInt(value: Ast.expr) = s"int(${translate(value)})"

  override def kaitaiStreamSize(value: Ast.expr) =
    outVarCheckRes(s"${translate(value)}.Size()")

  override def kaitaiStreamEof(value: Ast.expr) =
    outVarCheckRes(s"${translate(value)}.EOF()")

  override def kaitaiStreamPos(value: Ast.expr) =
    outVarCheckRes(s"${translate(value)}.Pos()")


  override def enumToInt(value: Ast.expr, et: EnumType) = translate(value)

  override def boolToInt(value: Ast.expr): String = {
    val v = allocateLocalVar()
    out.puts(s"${localVarName(v)} := 0")
    out.puts(s"if ${translate(value)} {")
    out.inc
    out.puts(s"${localVarName(v)} = 1")
    out.dec
    out.puts("}")
    localVarName(v)
  }

  def userType(t: UserType, io: String) = {
    val v = allocateLocalVar()
    val parent = t.forcedParent match {
      case Some(USER_TYPE_NO_PARENT) => "nil"
      case Some(fp) => translate(fp)
      case None => "this"
    }
    val root = if (t.isOpaque) "nil" else "this._root"
    val addParams = t.args.map((a) => translate(a)).mkString(", ")
    out.puts(s"${localVarName(v)} := New${GoCompiler.types2class(t.classSpec.get.name)}($addParams)")
    out.puts(s"err = ${localVarName(v)}.Read($io, $parent, $root)")
    outAddErrCheck()
    localVarName(v)
  }

  def outVarCheckRes(expr: String): String = {
    val v1 = allocateLocalVar()
    out.puts(s"${localVarName(v1)}, err := $expr")
    outAddErrCheck()
    localVarName(v1)
  }

  def outTransform(id: String, expr: String): String = {
    out.puts(s"${id} = $expr")
    id
  }

  private
  var localVarNum = 0

  def allocateLocalVar(): Int = {
    localVarNum += 1
    localVarNum
  }

  def localVarName(n: Int) = s"tmp$n"

  def outAddErrCheck() {
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

  override def byteSizeOfValue(attrName: String, valType: DataType): String =
    doIntLiteral(CommonSizeOf.bitToByteSize(CommonSizeOf.getBitsSizeOfType(attrName, valType)))
}
