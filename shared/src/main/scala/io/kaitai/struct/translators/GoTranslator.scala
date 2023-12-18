package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{ClassSpec, Identifier}
import io.kaitai.struct.languages.GoCompiler
import io.kaitai.struct.{ClassTypeProvider, ImportList, RuntimeConfig, StringLanguageOutputWriter, Utils}
import io.kaitai.struct.format.SpecialIdentifier
import io.kaitai.struct.format.NamedIdentifier
import io.kaitai.struct.format.InstanceIdentifier
import io.kaitai.struct.precompile.TypeMismatchError

class GoTranslator(out: StringLanguageOutputWriter, provider: TypeProvider, importList: ImportList, config: RuntimeConfig) extends BaseTranslator(provider) {
  import io.kaitai.struct.languages.GoCompiler._

  var returnRes: Option[String] = None

  override def doNumericCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): String = {
    val castedType = doCast(right, detectType(left))
    val rawLeft = translate(left)
    // TODO: very ugly
    var oops = op
    if (rawLeft.contains("newBytes") || (rawLeft.contains("wirte") && rawLeft.contains("Byte")) && op == Ast.cmpop.NotEq) {
        oops = Ast.cmpop.Eq
    }
    // TODO: resolve so many brackets
    detectType(right) match {
      case _: IntMultiType | _: FloatMultiType | CalcIntType => s"${translate(left)} ${cmpOp(oops)} $castedType"
      // case _: IntType | _: FloatType => s"(${translate(left)}) ${cmpOp(op)} ${translate(right)}"
      case _ => s"(($rawLeft) ${cmpOp(oops)} ${if (rawLeft.startsWith("len(")) "int(" + s"${translate(right)})" else s"(${translate(right)})"})"
    }
  }

  override def doStrCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): String = {
    val fn = (side: Ast.expr) => {
      detectType(side) match {
        case bytesType: StrFromBytesType if bytesType.bytes.isInstanceOf[BytesTerminatedType] =>
          val btt = bytesType.bytes.asInstanceOf[BytesTerminatedType]
          if (btt != null) {
            ".Write()"
          } else {
            ""
          }
        case _ =>
          ""
      }
    }

    s"${translate(left)}${fn(left)} ${cmpOp(op)} ${translate(right)}${fn(right)}"
  }

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

    val t = detectType(b)
    s"bytes.IndexByte(${terminatedTypeConvert(b, t)}, ${translate(byte)})"
  }

  override def numericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr): String = {
    val compiler = new GoCompiler(provider.asInstanceOf[ClassTypeProvider], config)
    (detectType(left), detectType(right), op) match {
      case (t1: IntType, t2: IntType, Ast.operator.Mod) =>
        s"(${translate(left)} % ${translate(right)} + ${translate(right)}) % ${translate(right)}"
      // TODO: for go, this is special
      case (t1: IntType, t2: IntType, _) => {
        val t1T = compiler.kaitaiType2NativeType(t1)
        val t1Size = t1 match {
          case CalcIntType => 32
          case it :IntMultiType => it.width.width * 8
          case _: Int1Type => 8
          case _ => 32
        }
        val t2T = compiler.kaitaiType2NativeType(t2)
        val t2Size = t2 match {
          case CalcIntType => 32
          case it: IntMultiType => it.width.width * 8
          case _: Int1Type => 8
          case _ => 32
        }
        val res = if (t2T != t1T) {
          if (t1Size > t2Size) {
            s"(${t1T}(${translate(left)}) ${binOp(op)} ${t1T}(${translate(right)}))"
          } else {
            s"(${t2T}(${translate(left)}) ${binOp(op)} ${t2T}(${translate(right)}))"
          }

        } else {
          s"(${translate(left)} ${binOp(op)} ${translate(right)})"
        }
        res
      }
      case _ =>
        s"(${translate(left)} ${binOp(op)} ${translate(right)})"
    }
  }

  override def strConcat(left: Ast.expr, right: Ast.expr): String = {
    translate(left) + " + " + translate(right)
  }

  override def doBytesCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): String = {
    importList.add("bytes")
    op match {
      case Ast.cmpop.Eq =>
        s"bytes.Equal(${translate(left)}, ${translate(right)})"
      case _ =>
        s"(bytes.Compare(${translate(left)}, ${translate(right)}) ${cmpOp(op)} 0)"
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

  override def doName(s: String): String = {
    s match {
      case Identifier.ROOT |
           Identifier.PARENT |
           Identifier.IO |
           Identifier.ITERATOR | Identifier.ITERATOR2 =>
        specialName(s)
      case _ =>
        Utils.upperCamelCase(s)
    }
  }

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
    val compiler = new GoCompiler(provider.asInstanceOf[ClassTypeProvider], config)
    val v1 = allocateLocalVar()
    val typ = detectType(ifTrue)

    var emuType = "int64"
    val convertedType = compiler.kaitaiType2NativeType(typ)
    if (convertedType.startsWith("uint")) {
      emuType = "uint64"
    } else {
      emuType = convertedType
    }

    out.puts(s"var ${localVarName(v1)} $emuType")
    out.puts(s"if ${translate(condition)} {")
    out.inc
    out.puts(s"${localVarName(v1)} = ${if (typ.toString.contains("UserType")) translate(ifTrue) else s"$emuType(${translate(ifTrue)})"}")
    out.dec
    out.puts("} else {")
    out.inc
    out.puts(s"${localVarName(v1)} = ${if (typ.toString.contains("UserType")) translate(ifFalse) else s"$emuType(${translate(ifFalse)})"}")
    out.dec
    out.puts("}")
    localVarName(v1)
  }

  override def doEnumByLabel(enumTypeAbs: List[String], label: String) = GoCompiler.enumToStr(enumTypeAbs, label)
  override def doEnumById(enumTypeAbs: List[String], id: String) = s"${types2class(enumTypeAbs)}($id)"

  override def doCast(value: Ast.expr, typeName: DataType): String = {
    val compiler = new GoCompiler(provider.asInstanceOf[ClassTypeProvider], config)
    if (value.isInstanceOf[Ast.expr.IntNum] || value.isInstanceOf[Ast.expr.FloatNum])
      s"${compiler.kaitaiType2NativeType(typeName)}(${translate(value)})"
    else
      compiler.castIfNeeded(translate(value), detectType(value), typeName)
  }

  override def doArrayLiteral(t: DataType, value: Seq[Ast.expr]) = {
    val compiler = new GoCompiler(provider.asInstanceOf[ClassTypeProvider], config)
    s"[]${compiler.kaitaiType2NativeType(t)}{${value.map(translate).mkString(", ")}}"
  }


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
    "UTF-16BE" -> ("unicode.UTF16(unicode.BigEndian, unicode.IgnoreBOM)", "golang.org/x/text/encoding/unicode"),
    "UTF-8" -> ("unicode.UTF8", "golang.org/x/text/encoding/unicode"),
    "ASCII" -> ("nil", "")
  )

  var terminatedTypesStack: List[() => String] = List()

  override def bytesToStr(value: String, encoding: String): String = {
    val enc = encoding.stripPrefix("\"").stripSuffix("\"")
    enc match {
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

  def rawTerminatedBytesToStr(value: String, rt: StrFromBytesType, id: Identifier): String = {
    val enc = rt.encoding.stripPrefix("\"").stripSuffix("\"")
    rt.bytes match {
      case bt: BytesTerminatedType => {
        if (bt.terminator == None) {
          return bytesToStr(value, rt.encoding)
        }
        ENCODINGS.get(enc) match {
          case Some((decoderSrc, importName)) =>
            if (importName != "") importList.add(importName)
            terminatedConstructorFact(id, rt, Map(
              "encoding"   -> decoderSrc,
              "terminator" -> bt.terminator,
              "include"    -> bt.include,
              "consume"    -> bt.consume,
              "eosError"   -> bt.eosError
            ))
            s"this.New_${idToStr(id)}TerminatedType(string($value))"
          case None =>
            throw new RuntimeException(s"encoding '$enc' in not supported in Go")
        }
      }
      case _ => bytesToStr(value, rt.encoding)
    }
  }

  def terminatedConstructorFact(id: Identifier, dt: DataType, params: Map[String, Any]) = {
    val compiler = new GoCompiler(provider.asInstanceOf[ClassTypeProvider], config)
    val idStr = idToStr(id)
    terminatedTypesStack = terminatedTypesStack.:+(() => {
      compiler.terminatorConstructor(idStr, dt, params)
    })
    null
  }

  override def strToBytes(value: Ast.expr, encoding: Ast.expr): String = {
    val strExpr = translate(value)
    val encodingExpr = translate(encoding)
    val valueType = detectType(value)

    var strId = strExpr
    if (strId.contains(".")) {
      strId = strId.split("\\.").last
      if (strId.contains("[") && strId.contains("]")) {
        strId = strId.replace("[", "_").replace("]", "")
      }
    } else if (strId.contains("[") && strId.contains("]")) {
      strId = strId.replace("[", "_").replace("]", "")
    }
    valueType match {
      case sbt if (sbt.isInstanceOf[StrFromBytesType] && sbt.asInstanceOf[StrFromBytesType].bytes.isInstanceOf[BytesTerminatedType]) => {
        val v = allocateLocalVar()
        s"wirte${strId}${v}Byte, err := $strExpr.Write();[]byte(wirte${strId}${v}Byte)"
      }
      case _ => {
        encodingExpr.stripPrefix("\"").stripSuffix("\"") match {
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
    val t = detectType(s)
    s"${terminatedTypeConvert(s, t)}[${translate(from)}:${translate(to)}]"
  }

  override def arrayFirst(a: Ast.expr): String = {
    val t = detectType(a)
    s"${terminatedTypeConvert(a, t)}[0]"
  }

  override def arrayLast(a: Ast.expr): String = {
    val t = detectType(a)
    s"${terminatedTypeConvert(a, t)}[len(${translate(a)}) - 1]"
  }
  override def arraySize(a: Ast.expr): String = {
    val t = detectType(a)
    s"len(${terminatedTypeConvert(a, t)})"
  }

  def terminatedTypeConvert(a: Ast.expr, t: DataType) = {
    val exprRaw = translate(a)
    if (t.isInstanceOf[BytesTerminatedType]) {
      val allocatedTempVar = allocateLocalVar()
      out.puts(s"newBytes${allocatedTempVar}, err := $exprRaw.Write()")
      returnRes = None
      outAddErrCheck()
      out.puts(s"newBytes${allocatedTempVar} = newBytes${allocatedTempVar}")
      s"newBytes${allocatedTempVar}"
    } else {
      if (exprRaw.contains(";")) {
        val newedExprRaw = exprRaw.split(";")
        out.puts(newedExprRaw(0))
        returnRes = None
        outAddErrCheck()
        out.puts(s"${newedExprRaw(0).split(",")(0)} = ${newedExprRaw(0).split(",")(0)}")
        newedExprRaw(1)
      } else {
        exprRaw
      }
    }
  }

  override def arrayMin(a: Ast.expr): String = {
    val min = allocateLocalVar()
    val value = allocateLocalVar()

    var elemIsTerminatedType = false

    val t = detectType(a)
    val translatedA = t match {
      case _: BytesTerminatedType => {
        s"${translate(a)}).Bytes()"
      }
      case sbt: StrFromBytesType if (sbt.bytes.isInstanceOf[BytesTerminatedType]) => {
        s"${translate(a)}).String()"
      }
      case cat: ArrayType if (cat.elType.isInstanceOf[BytesTerminatedType] || (cat.isInstanceOf[StrFromBytesType] && cat.asInstanceOf[StrFromBytesType].bytes.isInstanceOf[BytesTerminatedType])) => {
        elemIsTerminatedType = true
        translate(a)
      }
      case _ => translate(a)
    }

    out.puts(s"${localVarName(min)} := $translatedA[0]")
    out.puts(s"for _, ${localVarName(value)} := range $translatedA {")
    out.inc
    out.puts(s"if ${localVarName(min)}${if (elemIsTerminatedType) ".String()" else "" } > ${localVarName(value)}${if (elemIsTerminatedType) ".String()" else ""} {")
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
      val name = outVarCheckRes(s"$valueStr.$call()")
      out.puts(s"$name = $name")
      name
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
    out.puts(s"var ${localVarName(v)} uint64 = 0")
    out.puts(s"if ${translate(value)} {")
    out.inc
    out.puts(s"${localVarName(v)} = 1")
    out.dec
    out.puts("}")
    localVarName(v)
  }

  def userType(t: UserType, io: String): String = {
    val v = allocateLocalVar()
    val parent = t.forcedParent match {
      case Some(USER_TYPE_NO_PARENT) => "nil"
      case Some(fp) => translate(fp)
      case None => {
        val content = t.classSpec.get.parentClass match {
          case _: ClassSpec => {
            if (t.classSpec.get.isTopLevel) {
              "&this.Stream"
            } else {
              "this"
            }
          }
          case _ => "&this.Stream"
        }
        content
      }
    }

    val root = if (t.isOpaque) "nil" else "this._root"
    val addParams = t.args.map((a) => translate(a)).mkString(", ")
    out.puts(s"${localVarName(v)} := New${GoCompiler.types2class(t.classSpec.get.name)}($io, $parent, $root)")
    out.puts(s"err = ${localVarName(v)}.Read()")
    outAddErrCheck()
    localVarName(v)
  }

  def outVarCheckRes(expr: String): String = {
    val v1 = allocateLocalVar()
    out.puts(s"${localVarName(v1)}, err := $expr")
    outAddErrCheck()
    out.puts(s"${localVarName(v1)} = ${localVarName(v1)}")
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

  def interfaceTypeOfSwitchCase(transStr: String, targetType: String): String = {
    val transVar = allocateLocalVar()
    val tmpVarName = localVarName(transVar)
    out.puts(s"${tmpVarName}, ok := $transStr.($targetType)")
    out.puts("if !ok {")
    out.inc
    out.puts("return nil")
    out.dec
    out.puts("}")

    tmpVarName
  }

  def interfaceTypeOfSwitchCaseInExpr(expr: Ast.expr, dataType: Option[DataType], typeTransCallBack: (DataType) => String): (Ast.expr, String) = {
    var changedExpr = expr
    var tmpName = ""
    expr match {
      case call: Ast.expr.Attribute => {
        val value = call.value

        value match {
          case ct: Ast.expr.CastToType => {
            val dt = detectType(value)
            val nativeType = typeTransCallBack(dt)
            val transVar = allocateLocalVar()
            val tmpVarName = localVarName(transVar)
            out.puts(s"${tmpVarName}, ok := ${translate(value)}.($nativeType)")
            out.puts("if !ok {")
            out.inc
            out.puts("return nil")
            out.dec
            out.puts("}")

            changedExpr = value
            tmpName = tmpVarName
          }
          case _ => {}
        }
      }
      case name: Ast.expr.InternalName => {
        val nativeType = typeTransCallBack(dataType.get)
        val transVar = allocateLocalVar()
        val tmpVarName = localVarName(transVar)
        out.puts(s"${tmpVarName}, ok := ${translate(name)}.($nativeType)")
        out.puts("if !ok {")
        out.inc
        out.puts("return nil")
        out.dec
        out.puts("}")

        changedExpr = name
        tmpName = tmpVarName
      }
      case _ => {}
    }
    (changedExpr, tmpName)
  }
}
