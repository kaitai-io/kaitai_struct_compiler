package io.kaitai.struct.languages

import io.kaitai.struct.datatype.{DataType, FixedEndian}
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.{GoTranslator, TranslatorResult, TypeDetector}
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig, Utils}

class GoCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with SingleOutputFile
    with UpperCamelCaseClasses
    with ObjectOrientedLanguage
    with UniversalFooter
    with UniversalDoc
    with AllocateIOLocalVar
    with GoReads
    with FixedContentsUsingArrayByteLiteral {
  import GoCompiler._

  override val translator = new GoTranslator(out, typeProvider, importList)

  override def innerClasses = false

  override def universalFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def indent: String = "\t"
  override def outFileName(topClassName: String): String =
    s"src/${config.goPackage}/$topClassName.go"

  override def outImports(topClass: ClassSpec) = {
    val imp = importList.toList
    imp.size match {
      case 0 => ""
      case 1 => "import \"" + imp.head + "\"\n"
      case _ =>
        "import (\n" +
        imp.map((x) => indent + "\"" + x + "\"").mkString("", "\n", "\n") +
        ")\n"
    }
  }

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"// $headerComment")
    if (!config.goPackage.isEmpty) {
      outHeader.puts
      outHeader.puts(s"package ${config.goPackage}")
    }
    outHeader.puts

    importList.add("github.com/kaitai-io/kaitai_struct_go_runtime/kaitai")

    out.puts
  }

  override def classHeader(name: List[String]): Unit = {
    out.puts(s"type ${types2class(name)} struct {")
    out.inc
  }

  override def classFooter(name: List[String]): Unit = universalFooter

  override def classConstructorHeader(name: List[String], parentType: DataType, rootClassName: List[String], isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    out.puts
    out.puts(
      s"func (this *${types2class(name)}) Read(" +
        s"io *$kstreamName, " +
        s"parent ${kaitaiType2NativeType(parentType)}, " +
        s"root *${types2class(rootClassName)}) (err error) {"
    )
    out.inc
    out.puts(s"${privateMemberName(IoIdentifier)} = io")
    out.puts(s"${privateMemberName(ParentIdentifier)} = parent")
    out.puts(s"${privateMemberName(RootIdentifier)} = root")
    out.puts
  }

  override def classConstructorFooter: Unit = {
    out.puts("return err")
    universalFooter
  }

  override def runRead(): Unit = {}
  override def runReadCalc(): Unit = ???
  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = {}
  override def readFooter(): Unit = {}

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"${idToStr(attrName)} ${kaitaiType2NativeType(attrType)}")
    translator.returnRes = None
  }

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {}

  override def universalDoc(doc: DocSpec): Unit = {
    out.puts
    out.puts( "/**")

    doc.summary.foreach((summary) => out.putsLines(" * ", summary))

    doc.ref match {
      case TextRef(text) =>
        out.putsLines(" * ", "@see \"" + text + "\"")
      case ref: UrlRef =>
        out.putsLines(" * ", s"@see ${ref.toAhref}")
      case NoRef =>
        // no reference => output nothing
    }

    out.puts( " */")
  }

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = ???

  override def attrFixedContentsParse(attrName: Identifier, contents: String): Unit = {
    out.puts(s"${privateMemberName(attrName)} = $normalIO.ensureFixedContents($contents);")
  }

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit = {
    val srcName = privateMemberName(varSrc)
    val destName = privateMemberName(varDest)

    proc match {
      case ProcessXor(xorValue) =>
        out.puts(s"$destName = $kstreamName.processXor($srcName, ${expression(xorValue)});")
      case ProcessZlib =>
        out.puts(s"$destName = $kstreamName.processZlib($srcName);")
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        out.puts(s"$destName = $kstreamName.processRotateLeft($srcName, $expr, 1);")
    }
  }

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = {
    val javaName = privateMemberName(varName)

    val ioName = idToStr(IoStorageIdentifier(varName))

    val args = rep match {
      case RepeatEos | RepeatExpr(_) => s"$javaName.get($javaName.size() - 1)"
      case RepeatUntil(_) => translator.specialName(Identifier.ITERATOR2)
      case NoRepeat => javaName
    }

    importList.add("bytes")

    out.puts(s"$ioName := kaitai.NewStream(bytes.NewReader($args))")
    ioName
  }

  override def useIO(ioEx: Ast.expr): String = {
    out.puts(s"$kstreamName io = ${expression(ioEx)};")
    "io"
  }

  override def pushPos(io: String): Unit = {
    out.puts(s"_pos, err := $io.Pos()")
    translator.outAddErrCheck()
  }

  override def seek(io: String, pos: Ast.expr): Unit = {
    importList.add("io")

    out.puts(s"_, err = $io.Seek(int64(${expression(pos)}), io.SeekStart)")
    translator.outAddErrCheck()
  }

  override def popPos(io: String): Unit = {
    importList.add("io")

    out.puts(s"_, err = $io.Seek(_pos, io.SeekStart)")
    translator.outAddErrCheck()
  }

  override def alignToByte(io: String): Unit =
    out.puts(s"$io.AlignToByte()")

  override def condIfHeader(expr: Ast.expr): Unit = {
    out.puts(s"if (${expression(expr)}) {")
    out.inc
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = new ArrayList<byte[]>();")
    //out.puts(s"${privateMemberName(id)} = make(${kaitaiType2NativeType(ArrayType(dataType))})")
    out.puts(s"for !$io.EOF() {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, r: TranslatorResult): Unit = {
    val name = privateMemberName(id)
    val expr = translator.resToStr(r)
    out.puts(s"$name = append($name, $expr)")
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: Ast.expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = new ArrayList<byte[]>((int) (${expression(repeatExpr)}));")
    out.puts(s"${privateMemberName(id)} = make(${kaitaiType2NativeType(ArrayType(dataType))}, ${expression(repeatExpr)})")
    out.puts(s"for i := range ${privateMemberName(id)} {")
    out.inc
  }

  override def handleAssignmentRepeatExpr(id: Identifier, r: TranslatorResult): Unit = {
    val name = privateMemberName(id)
    val expr = translator.resToStr(r)
    out.puts(s"$name[i] = $expr")
  }

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, untilExpr: Ast.expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = new ArrayList<byte[]>();")
    out.puts(s"${privateMemberName(id)} = new ${kaitaiType2NativeType(ArrayType(dataType))}();")
    out.puts("{")
    out.inc
    out.puts(s"${kaitaiType2NativeType(dataType)} ${translator.specialName(Identifier.ITERATOR)};")
    out.puts("do {")
    out.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, r: TranslatorResult, isRaw: Boolean): Unit = {
    val expr = translator.resToStr(r)
    val (typeDecl, tempVar) = if (isRaw) {
      ("byte[] ", translator.specialName(Identifier.ITERATOR2))
    } else {
      ("", translator.specialName(Identifier.ITERATOR))
    }
    out.puts(s"$typeDecl$tempVar = $expr;")
    out.puts(s"${privateMemberName(id)}.add($tempVar);")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, untilExpr: Ast.expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.dec
    out.puts(s"} while (!(${expression(untilExpr)}));")
    out.dec
    out.puts("}")
  }

  override def handleAssignmentSimple(id: Identifier, r: TranslatorResult): Unit = {
    val expr = translator.resToStr(r)
    out.puts(s"${privateMemberName(id)} = $expr")
  }

  override def parseExpr(dataType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    dataType match {
      case t: ReadableType =>
        s"$io.Read${Utils.capitalize(t.apiCall(defEndian))}()"
      case blt: BytesLimitType =>
        s"$io.ReadBytes(int(${expression(blt.size)}))"
      case _: BytesEosType =>
        s"$io.ReadBytesFull()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        s"$io.ReadBytesTerm($terminator, $include, $consume, $eosError)"
      case BitsType1 =>
        s"$io.ReadBitsInt(1) != 0"
      case BitsType(width: Int) =>
        s"$io.ReadBitsInt($width)"
      case t: UserType =>
        val addArgs = if (t.isOpaque) {
          ""
        } else {
          val parent = t.forcedParent match {
            case Some(USER_TYPE_NO_PARENT) => "null"
            case Some(fp) => translator.translate(fp)
            case None => "this"
          }
          s", $parent, _root"
        }
        s"${types2class(t.name)}($io$addArgs)"
    }
  }

//  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean) = {
//    val expr1 = padRight match {
//      case Some(padByte) => s"$kstreamName.bytesStripRight($expr0, (byte) $padByte)"
//      case None => expr0
//    }
//    val expr2 = terminator match {
//      case Some(term) => s"$kstreamName.bytesTerminate($expr1, (byte) $term, $include)"
//      case None => expr1
//    }
//    expr2
//  }

  override def switchStart(id: Identifier, on: Ast.expr): Unit =
    out.puts(s"switch (${expression(on)}) {")

  override def switchCaseStart(condition: Ast.expr): Unit = {
    // Java is very specific about what can be used as "condition" in "case
    // condition:".
    val condStr = condition match {
      case Ast.expr.EnumByLabel(enumName, enumVal) =>
        // If switch is over a enum, only literal enum values are supported,
        // and they must be written as "MEMBER", not "SomeEnum.MEMBER".
        value2Const(enumVal.name)
      case _ =>
        expression(condition)
    }

    out.puts(s"case $condStr: {")
    out.inc
  }

  override def switchCaseEnd(): Unit = {
    out.puts("break;")
    out.dec
    out.puts("}")
  }

  override def switchElseStart(): Unit = {
    out.puts("default: {")
    out.inc
  }

  override def switchEnd(): Unit =
    out.puts("}")

  override def instanceDeclaration(attrName: InstanceIdentifier, attrType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"${calculatedFlagForName(attrName)} bool")
    out.puts(s"${idToStr(attrName)} ${kaitaiType2NativeType(attrType)}")
  }

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"func (this *${types2class(className)}) ${publicMemberName(instName)}() (v ${kaitaiType2NativeType(dataType)}, err error) {")
    out.inc
    translator.returnRes = Some(dataType match {
      case _: IntType => "0"
      case _: BooleanType => "false"
      case _: StrType => "\"\""
      case _ => "nil"
    })
  }

  override def instanceCalculate(instName: Identifier, dataType: DataType, value: Ast.expr): Unit = {
    val r = translator.translate(value)
    val converted = dataType match {
      case _: UserType => r
      case _ => s"${kaitaiType2NativeType(dataType)}($r)"
    }
    out.puts(s"${privateMemberName(instName)} = $converted")
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier): Unit = {
    out.puts(s"if (this.${calculatedFlagForName(instName)}) {")
    out.inc
    instanceReturn(instName)
    universalFooter
  }

  override def instanceReturn(instName: InstanceIdentifier): Unit = {
    out.puts(s"return ${privateMemberName(instName)}, nil")
  }

  override def instanceSetCalculated(instName: InstanceIdentifier): Unit =
    out.puts(s"this.${calculatedFlagForName(instName)} = true")

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, EnumValueSpec)]): Unit = {
    val enumClass = type2class(enumName)

    out.puts
    out.puts(s"public enum $enumClass {")
    out.inc

    if (enumColl.size > 1) {
      enumColl.dropRight(1).foreach { case (id, label) =>
        out.puts(s"${value2Const(label.name)}($id),")
      }
    }
    enumColl.last match {
      case (id, label) =>
        out.puts(s"${value2Const(label.name)}($id);")
    }

    out.puts
    out.puts("private final long id;")
    out.puts(s"$enumClass(long id) { this.id = id; }")
    out.puts("public long id() { return id; }")
    out.puts(s"private static final Map<Long, $enumClass> byId = new HashMap<Long, $enumClass>(${enumColl.size});")
    out.puts("static {")
    out.inc
    out.puts(s"for ($enumClass e : $enumClass.values())")
    out.inc
    out.puts(s"byId.put(e.id(), e);")
    out.dec
    out.dec
    out.puts("}")
    out.puts(s"public static $enumClass byId(long id) { return byId.get(id); }")
    out.dec
    out.puts("}")
  }

  def value2Const(s: String) = s.toUpperCase

  def idToStr(id: Identifier): String = {
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => Utils.upperCamelCase(name)
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => Utils.lowerCamelCase(name)
      case RawIdentifier(innerId) => "_raw_" + idToStr(innerId)
      case IoStorageIdentifier(innerId) => "_io_" + idToStr(innerId)
    }
  }

  override def privateMemberName(id: Identifier): String = s"this.${idToStr(id)}"

  override def publicMemberName(id: Identifier): String = {
    id match {
      case IoIdentifier => "_IO"
      case RootIdentifier => "_Root"
      case ParentIdentifier => "_Parent"
      case NamedIdentifier(name) => Utils.upperCamelCase(name)
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => Utils.upperCamelCase(name)
      case RawIdentifier(innerId) => "_raw_" + idToStr(innerId)
    }
  }

  override def localTemporaryName(id: Identifier): String = s"_t_${idToStr(id)}"

  def calculatedFlagForName(id: Identifier) = s"_f_${idToStr(id)}"
}

object GoCompiler extends LanguageCompilerStatic
  with UpperCamelCaseClasses
  with StreamStructNames {

  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new GoCompiler(tp, config)

  /**
    * Determine Go data type corresponding to a KS data type.
    *
    * @param attrType KS data type
    * @return Go data type
    */
  def kaitaiType2NativeType(attrType: DataType): String = {
    attrType match {
      case Int1Type(false) => "uint8"
      case IntMultiType(false, Width2, _) => "uint16"
      case IntMultiType(false, Width4, _) => "uint32"
      case IntMultiType(false, Width8, _) => "uint64"

      case Int1Type(true) => "int8"
      case IntMultiType(true, Width2, _) => "int16"
      case IntMultiType(true, Width4, _) => "int32"
      case IntMultiType(true, Width8, _) => "int64"

      case FloatMultiType(Width4, _) => "float32"
      case FloatMultiType(Width8, _) => "float64"

      case BitsType(_) => "uint64"

      case _: BooleanType => "bool"
      case CalcIntType => "int"
      case CalcFloatType => "float64"

      case _: StrType => "string"
      case _: BytesType => "[]byte"

      case AnyType => "interface{}"
      case KaitaiStreamType => "*" + kstreamName
      case KaitaiStructType => kstructName

      case t: UserType => "*" + types2class(t.classSpec match {
        case Some(cs) => cs.name
        case None => t.name
      })
      case EnumType(name, _) => types2class(name)

      case ArrayType(inType) => s"[]${kaitaiType2NativeType(inType)}"

      case SwitchType(_, cases) => kaitaiType2NativeType(TypeDetector.combineTypes(cases.values))
    }
  }

  def types2class(names: List[String]) = names.map(x => type2class(x)).mkString("_")

  override def kstreamName: String = "kaitai.Stream"
  override def kstructName: String = "interface{}"
}
