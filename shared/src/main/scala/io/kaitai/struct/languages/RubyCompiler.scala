package io.kaitai.struct.languages

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.RubyTranslator
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig, Utils}

class RubyCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with ObjectOrientedLanguage
    with SingleOutputFile
    with UniversalFooter
    with UniversalDoc
    with UpperCamelCaseClasses
    with AllocateIOLocalVar
    with EveryReadIsExpression
    with FixedContentsUsingArrayByteLiteral
    with NoNeedForFullClassPath {

  import RubyCompiler._

  val translator = new RubyTranslator(typeProvider)

  override def universalFooter: Unit = {
    out.dec
    out.puts("end")
  }

  override def outFileName(topClassName: String): String = s"$topClassName.rb"
  override def indent: String = "  "

  override def outImports(topClass: ClassSpec) =
    importList.toList.map((x) => s"require '$x'").mkString("\n") + "\n"

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"# $headerComment")
    outHeader.puts

    importList.add("kaitai/struct/struct")

    out.puts

    // API compatibility check
    out.puts(
      "unless Gem::Version.new(Kaitai::Struct::VERSION) >= Gem::Version.new('" +
      KSVersion.minimalRuntime +
      "')"
    )
    out.inc
    out.puts(
      "raise \"Incompatible Kaitai Struct Ruby API: " +
      KSVersion.minimalRuntime +
      " or later is required, but you have #{Kaitai::Struct::VERSION}\""
    )
    out.dec
    out.puts("end")
    out.puts
  }

  override def classHeader(name: String): Unit = {
    out.puts(s"class ${type2class(name)} < $kstructName")
    out.inc
    if (config.readStoresPos)
      out.puts("attr_reader :_debug")
  }

  override def classConstructorHeader(name: String, parentType: DataType, rootClassName: String, isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    val endianSuffix = if (isHybrid) {
      ", _is_le = nil"
    } else {
      ""
    }

    val paramsList = Utils.join(params.map((p) => paramName(p.id)), ", ", ", ", "")

    out.puts(s"def initialize(_io, _parent = nil, _root = self$endianSuffix$paramsList)")
    out.inc
    out.puts("super(_io, _parent, _root)")

    if (isHybrid) {
      out.puts("@_is_le = _is_le")
    }

    // Store parameters passed to us
    params.foreach((p) => handleAssignmentSimple(p.id, paramName(p.id)))

    if (config.readStoresPos) {
      out.puts("@_debug = {}")
    }
  }

  override def runRead(name: List[String]): Unit = {
    out.puts("_read")
  }

  override def runReadCalc(): Unit = {
    out.puts
    out.puts(s"if @_is_le == true")
    out.inc
    out.puts("_read_le")
    out.dec
    out.puts("elsif @_is_le == false")
    out.inc
    out.puts("_read_be")
    out.dec
    out.puts("else")
    out.inc
    out.puts(s"raise ${ksErrorName(UndecidedEndiannessError)}.new(" + "\"" + typeProvider.nowClass.path.mkString("/", "/", "") + "\")")
    out.dec
    out.puts("end")
  }

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean) = {
    val suffix = endian match {
      case Some(e) => s"_${e.toSuffix}"
      case None => ""
    }
    out.puts
    out.puts(s"def _read$suffix")
    out.inc
  }

  override def readFooter() = {
    // This is required for debug mode to be able to do stuff like:
    //
    //     obj = Obj.new(...)._read
    //
    // i.e. drop-in replacement of non-debug mode invocation:
    //
    //     obj = Obj.new(...)
    out.puts("self")

    universalFooter
  }

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {}

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    attrName match {
      case RootIdentifier | ParentIdentifier =>
        // ignore, they are already added in Kaitai::Struct::Struct
      case _ =>
        out.puts(s"attr_reader :${publicMemberName(attrName)}")
    }
  }

  override def universalDoc(doc: DocSpec): Unit = {
    out.puts
    out.puts("##")

    doc.summary.foreach(summary => out.putsLines("# ", summary))

    doc.ref.foreach {
      case TextRef(text) =>
        out.putsLines("# ", s"@see '' $text", "  ")
      case UrlRef(url, text) =>
        out.putsLines("# ", s"@see $url $text", "  ")
    }
  }

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {
    out.puts("if @_is_le")
    out.inc
    leProc()
    out.dec
    out.puts("else")
    out.inc
    beProc()
    out.dec
    out.puts("end")
  }

  override def attrFixedContentsParse(attrName: Identifier, contents: String): Unit =
    out.puts(s"${privateMemberName(attrName)} = $normalIO.ensure_fixed_contents($contents)")

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec): Unit = {
    val srcExpr = getRawIdExpr(varSrc, rep)

    val expr = proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "process_xor_one"
          case _: BytesType => "process_xor_many"
        }
        s"$kstreamName::$procName($srcExpr, ${expression(xorValue)})"
      case ProcessZlib =>
        importList.add("zlib")
        s"Zlib::Inflate.inflate($srcExpr)"
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        s"$kstreamName::process_rotate_left($srcExpr, $expr, 1)"
      case ProcessCustom(name, args) =>
        val procClass = name.map((x) => type2class(x)).mkString("::")
        out.puts(s"_process = $procClass.new(${args.map(expression).mkString(", ")})")
        s"_process.decode($srcExpr)"
    }
    handleAssignment(varDest, expr, rep, false)
  }

  override def allocateIO(id: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(id)
    val ioName = s"_io_${idToStr(id)}"

    val args = getRawIdExpr(id, rep)

    out.puts(s"$ioName = $kstreamName.new($args)")
    ioName
  }

  def getRawIdExpr(varName: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(varName)
    rep match {
      case NoRepeat => memberName
      case RepeatExpr(_) => s"$memberName[i]"
      case _ => s"$memberName.last"
    }
  }

  override def useIO(ioEx: expr): String = {
    out.puts(s"io = ${expression(ioEx)}")
    "io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"_pos = $io.pos")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io.seek(${expression(pos)})")

  override def popPos(io: String): Unit =
    out.puts(s"$io.seek(_pos)")

  override def alignToByte(io: String): Unit =
    out.puts(s"$io.align_to_byte")

  override def attrDebugStart(attrId: Identifier, attrType: DataType, ios: Option[String], rep: RepeatSpec): Unit = {
    ios.foreach { (io) =>
      val name = attrId match {
        case _: RawIdentifier | _: SpecialIdentifier => return
        case _ => idToStr(attrId)
      }
      rep match {
        case NoRepeat =>
          out.puts(s"(@_debug['$name'] ||= {})[:start] = $io.pos")
        case _: RepeatExpr =>
          out.puts(s"(@_debug['$name'][:arr] ||= [])[i] = {:start => $io.pos}")
        case RepeatEos | _: RepeatUntil =>
          out.puts(s"(@_debug['$name'][:arr] ||= [])[${privateMemberName(attrId)}.size] = {:start => $io.pos}")
      }
    }
  }

  override def attrDebugEnd(attrId: Identifier, attrType: DataType, io: String, rep: RepeatSpec): Unit = {
    val name = attrId match {
      case _: RawIdentifier | _: SpecialIdentifier => return
      case _ => idToStr(attrId)
    }
    rep match {
      case NoRepeat =>
        out.puts(s"(@_debug['$name'] ||= {})[:end] = $io.pos")
      case _: RepeatExpr =>
        out.puts(s"@_debug['$name'][:arr][i][:end] = $io.pos")
      case RepeatEos | _: RepeatUntil =>
        out.puts(s"@_debug['$name'][:arr][${privateMemberName(attrId)}.size - 1][:end] = $io.pos")
    }
  }

  override def condIfHeader(expr: Ast.expr): Unit = {
    out.puts(s"if ${expression(expr)}")
    out.inc
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw): Unit = {
    if (needRaw.level >= 1)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = []")
    if (needRaw.level >= 2)
      out.puts(s"${privateMemberName(RawIdentifier(RawIdentifier(id)))} = []")

    out.puts(s"${privateMemberName(id)} = []")
    out.puts("i = 0")
    out.puts(s"while not $io.eof?")
    out.inc
  }
  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)} << $expr")
  override def condRepeatEosFooter: Unit = {
    out.puts("i += 1")
    super.condRepeatEosFooter
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw, repeatExpr: expr): Unit = {
    if (needRaw.level >= 1)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = Array.new(${expression(repeatExpr)})")
    if (needRaw.level >= 2)
      out.puts(s"${privateMemberName(RawIdentifier(RawIdentifier(id)))} = Array.new(${expression(repeatExpr)})")
    out.puts(s"${privateMemberName(id)} = Array.new(${expression(repeatExpr)})")
    out.puts(s"(${expression(repeatExpr)}).times { |i|")
    out.inc
  }
  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)}[i] = $expr")
  override def condRepeatExprFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw, untilExpr: expr): Unit = {
    if (needRaw.level >= 1)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = []")
    if (needRaw.level >= 2)
      out.puts(s"${privateMemberName(RawIdentifier(RawIdentifier(id)))} = []")
    out.puts(s"${privateMemberName(id)} = []")
    out.puts("i = 0")
    out.puts("begin")
    out.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val tmpName = translator.doName(if (isRaw) Identifier.ITERATOR2 else Identifier.ITERATOR)
    out.puts(s"$tmpName = $expr")
    out.puts(s"${privateMemberName(id)} << $tmpName")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw, untilExpr: expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.puts("i += 1")
    out.dec
    out.puts(s"end until ${expression(untilExpr)}")
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)} = $expr")

  override def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit =
    out.puts(s"$id = $expr")

  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    dataType match {
      case t: ReadableType =>
        s"$io.read_${t.apiCall(defEndian)}"
      case blt: BytesLimitType =>
        s"$io.read_bytes(${expression(blt.size)})"
      case _: BytesEosType =>
        s"$io.read_bytes_full"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        s"$io.read_bytes_term($terminator, $include, $consume, $eosError)"
      case BitsType1(bitEndian) =>
        s"$io.read_bits_int_${bitEndian.toSuffix}(1) != 0"
      case BitsType(width: Int, bitEndian) =>
        s"$io.read_bits_int_${bitEndian.toSuffix}($width)"
      case t: UserType =>
        val addParams = Utils.join(t.args.map((a) => translator.translate(a)), ", ", ", ", "")
        val addArgs = if (t.isOpaque) {
          ""
        } else {
          val parent = t.forcedParent match {
            case Some(USER_TYPE_NO_PARENT) => "nil"
            case Some(fp) => translator.translate(fp)
            case None => "self"
          }
          val addEndian = t.classSpec.get.meta.endian match {
            case Some(InheritedEndian) => ", @_is_le"
            case _ => ""
          }
          s", $parent, @_root$addEndian"
        }
        s"${types2class(t.name)}.new($io$addArgs$addParams)"
    }
  }

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean) = {
    val expr1 = padRight match {
      case Some(padByte) => s"$kstreamName::bytes_strip_right($expr0, $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) => s"$kstreamName::bytes_terminate($expr1, $term, $include)"
      case None => expr1
    }
    expr2
  }

  override def userTypeDebugRead(id: String, dataType: DataType, assignType: DataType): Unit =
    out.puts(s"$id._read")

  override def switchStart(id: Identifier, on: Ast.expr): Unit =
    out.puts(s"case ${expression(on)}")

  override def switchCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"when ${expression(condition)}")
    out.inc
  }

  override def switchCaseEnd(): Unit =
    out.dec

  override def switchElseStart(): Unit = {
    out.puts("else")
    out.inc
  }

  override def switchEnd(): Unit =
    out.puts("end")

  override def instanceHeader(className: String, instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"def ${instName.name}")
    out.inc
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    out.puts(s"return ${privateMemberName(instName)} unless ${privateMemberName(instName)}.nil?")
  }

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit = {
    out.puts(privateMemberName(instName))
  }

  override def enumDeclaration(curClass: String, enumName: String, enumColl: Seq[(Long, String)]): Unit = {
    val enumConst = value2Const(enumName)

    out.puts
    out.puts(s"$enumConst = {")
    out.inc
    enumColl.foreach { case (id, label) =>
      out.puts(s"${translator.doIntLiteral(id)} => ${enumValue(enumName, label)},")
    }
    out.dec
    out.puts("}")

    // Generate inverse hash
    out.puts(s"${inverseEnumName(enumConst)} = $enumConst.invert")
  }

  def enumValue(enumName: String, enumLabel: String) = translator.doEnumByLabel(List(enumName), enumLabel)

  def value2Const(s: String) = Utils.upperUnderscoreCase(s)

  override def debugClassSequence(seq: List[AttrSpec]) = {
    val seqStr = seq.map((attr) => "\"" + idToStr(attr.id) + "\"").mkString(", ")
    out.puts(s"SEQ_FIELDS = [$seqStr]")
  }

  override def classToString(toStringExpr: Ast.expr): Unit = {
    out.puts
    out.puts("def inspect")
    out.inc
    out.puts(translator.translate(toStringExpr))
    out.dec
    out.puts("end")
  }

  override def idToStr(id: Identifier): String = {
    id match {
      case NamedIdentifier(name) => Utils.lowerUnderscoreCase(name)
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case si: SpecialIdentifier => si.name
      case RawIdentifier(inner) => s"_raw_${idToStr(inner)}"
      case InstanceIdentifier(name) => Utils.lowerUnderscoreCase(name)
    }
  }

  override def privateMemberName(id: Identifier): String = s"@${idToStr(id)}"

  override def publicMemberName(id: Identifier): String = idToStr(id)

  override def localTemporaryName(id: Identifier): String = s"_t_${idToStr(id)}"

  override def ksErrorName(err: KSError): String = RubyCompiler.ksErrorName(err)

  override def attrValidateExpr(
    attrId: Identifier,
    attrType: DataType,
    checkExpr: Ast.expr,
    err: KSError,
    errArgs: List[Ast.expr]
  ): Unit = {
    val errArgsStr = errArgs.map(translator.translate).mkString(", ")
    out.puts(s"raise ${ksErrorName(err)}.new($errArgsStr) if not ${translator.translate(checkExpr)}")
  }

  def types2class(names: List[String]) = names.map(type2class).mkString("::")
}

object RubyCompiler extends LanguageCompilerStatic
  with StreamStructNames
  with ExceptionNames {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new RubyCompiler(tp, config)

  override def kstreamName: String = "Kaitai::Struct::Stream"
  override def kstructName: String = "Kaitai::Struct::Struct"
  override def ksErrorName(err: KSError): String = err match {
    case EndOfStreamError => "EOFError"
    case _ => s"Kaitai::Struct::${err.name}"
  }

  def inverseEnumName(enumName: String) = s"I__$enumName"
}
