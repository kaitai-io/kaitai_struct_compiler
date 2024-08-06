package io.kaitai.struct.languages

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.NimTranslator
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig, Utils, ExternalType}

class NimCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with SingleOutputFile
    with EveryReadIsExpression
    with UpperCamelCaseClasses
    with FixedContentsUsingArrayByteLiteral
    with UniversalFooter
    with AllocateIOLocalVar
    with SwitchIfOps
    with UniversalDoc {

  import NimCompiler._

  // Written from scratch
  def blankLine: Unit = out.puts
  def imports = importList.toList.map((x) => s"import $x").mkString("\n")
  def namespaced(names: List[String]): String = names.map(n => camelCase(n, true)).mkString("_")
  def typeSectionHeader: Unit = {
    out.puts("type")
    out.inc
  }
  def typeSectionFooter: Unit = {
    out.dec
    out.puts
  }
  def instanceForwardDeclaration(className: List[String], instName: InstanceIdentifier, dataType: DataType): Unit = {
    out.puts(s"proc ${idToStr(instName).dropRight(4)}*(this: ${namespaced(className)}): ${ksToNim(dataType)}")
  }
  def fromFile(name: List[String]): Unit = {
    val n = namespaced(name)
    out.puts(s"proc fromFile*(_: typedesc[$n], filename: string): $n =")
    out.inc
    out.puts(s"$n.read(newKaitaiFileStream(filename), nil, nil)")
    out.dec
    out.puts
  }

  override def classToString(toStringExpr: Ast.expr): Unit = {
    out.puts
    out.puts(s"proc `$$`(x: ${namespaced(typeProvider.nowClass.name)}): string =")
    out.inc
    out.puts(s"return ${translator.translate(toStringExpr)}")
    out.dec
    out.puts
  }

  override def externalTypeDeclaration(extType: ExternalType): Unit =
    importList.add(extType.name.head)
  override def innerEnums = false
  override val translator: NimTranslator = new NimTranslator(typeProvider, importList)
  override def universalFooter: Unit = {
    out.dec
  }
  override def allocateIO(id: Identifier, rep: RepeatSpec): String = {
    val ioName = s"${idToStr(id)}Io"
    val arg = rep match {
      case NoRepeat => idToStr(id) + "Expr"
      case _ => translator.doName(Identifier.ITERATOR2)
    }
    out.puts(s"let $ioName = newKaitaiStream($arg)")
    ioName
  }

  // Members declared in io.kaitai.struct.languages.components.SingleOutputFile
  override def outImports(topClass: ClassSpec) =
    importList.toList.map((x) => s"import $x").mkString("\n") + "\n\n"

  // Members declared in io.kaitai.struct.languages.components.ExtraAttrs
  // def extraAttrForIO(id: Identifier, rep: RepeatSpec): List[AttrSpec] = ???

  // Members declared in io.kaitai.struct.languages.components.ExceptionNames
  override def ksErrorName(err: KSError): String = "KaitaiError" // TODO: maybe add more debugging info

  // Members declared in io.kaitai.struct.languages.components.LanguageCompiler
  override def alignToByte(io: String): Unit = out.puts(s"alignToByte($io)")
  override def attrFixedContentsParse(attrName: Identifier, contents: String): Unit = {
    out.puts(s"this.${idToStr(attrName)} = $normalIO.ensureFixedContents($contents)")
  }
  // def attrParse(attr: AttrLikeSpec, id: Identifier, defEndian: Option[Endianness]): Unit = ???
  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {
    out.puts("if this.isLe:")
    out.inc
    leProc()
    out.dec
    out.puts("else:")
    out.inc
    beProc()
    out.dec
  }

  // Works but is crappily written; I want to rewrite this later XXX
  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec): Unit = {
    val srcExpr = rep match {
      case RepeatEos | RepeatExpr(_) | RepeatUntil(_) => privateMemberName(varSrc) + "[i]"
      case NoRepeat => privateMemberName(varSrc)
    }
    val expr = proc match {
      case ProcessXor(xorValue) =>
        s"$srcExpr.processXor(${expression(xorValue)})"
      case ProcessZlib =>
        s"$srcExpr.processZlib()"
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        s"$srcExpr.processRotateLeft(int($expr))"
      case ProcessCustom(name, args) =>
        val namespace = name.head
        val procPath = name.mkString(".")
        importList.add(namespace)
        s"$procPath($srcExpr, ${args.map(expression).mkString(", ")})"
    }
    handleAssignment(varDest, expr, rep, false)
  }
  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"`${idToStr(attrName)}`*: ${ksToNim(attrType)}")
  }
  override def instanceDeclaration(attrName: InstanceIdentifier, attrType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"`${idToStr(attrName)}`: ${ksToNim(attrType)}")
    out.puts(s"`${instanceFlagIdentifier(attrName)}`: bool")
  }
  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {}
  override def classConstructorHeader(name: List[String], parentType: DataType, rootClassName: List[String], isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {}
  override def classHeader(name: List[String]): Unit = {
    out.puts(s"${namespaced(name)}* = ref object of KaitaiStruct")
    out.inc
  }
  override def condIfHeader(expr: Ast.expr): Unit = {
    out.puts(s"if ${expression(expr)}:")
    out.inc
  }
  override def classFooter(name: List[String]): Unit = {
    typeProvider.nowClass.meta.endian match {
      case Some(_: CalcEndian) | Some(InheritedEndian) =>
        out.puts(s"${idToStr(EndianIdentifier)}: bool")
      case _ =>
    }
    universalFooter
  }

  override def condRepeatInitAttr(id: Identifier, dataType: DataType): Unit = {
    // sequences don't have to be manually initialized in Nim - they're automatically initialized as
    // empty sequences (see https://narimiran.github.io/nim-basics/#_result_variable)
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType): Unit = {
    out.puts("block:")
    out.inc
    out.puts("var i: int")
    out.puts(s"while not $io.isEof:")
    out.inc
  }
  override def condRepeatEosFooter: Unit = {
    out.puts("inc i")
    out.dec
    out.dec
  }
  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, repeatExpr: Ast.expr): Unit = {
    out.puts(s"for i in 0 ..< int(${expression(repeatExpr)}):")
    out.inc
  }
  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, untilExpr: Ast.expr): Unit = {
    out.puts("block:")
    out.inc
    out.puts("var i: int")
    out.puts("while true:")
    out.inc
  }
  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, untilExpr: Ast.expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.puts(s"if ${expression(untilExpr)}:")
    out.inc
    out.puts("break")
    out.dec
    out.puts("inc i")
    out.dec
    out.dec
  }
  // For this to work, we need a {.lenientCase.} pragma which disables nim's exhaustive case coverage check
  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, EnumValueSpec)]): Unit = {
    val enumClass = namespaced(curClass)
    out.puts(s"${enumClass}_${camelCase(enumName, true)}* = enum")
    out.inc
    enumColl.foreach { case (id, label) =>
      val order = if (s"$id" == "-9223372036854775808") "low(int64)" else s"$id"
      out.puts(s"${label.name} = $order")
    }
    out.dec
  }
//  def enumFooter: Unit = {
//    universalFooter
//    out.puts
//  }
//  def enumTemplate: Unit = {
//    out.puts("template defineEnum(typ) =")
//    out.inc
//    out.puts("type typ* = distinct int64")
//    out.puts("proc `==`*(x, y: typ): bool {.borrow.}")
//    out.dec
//  }
//  def enumTemplateFooter: Unit = out.puts
//  def enumHeader: Unit = {
//    out.puts("const")
//    out.inc
//  }
//  def enumConstantsFooter: Unit = {
//    universalFooter
//    out.puts
//  }
//  def enumConstants(curClass: List[String], enumName: String,  enumColl: Seq[(Long, EnumValueSpec)]): Unit = {
//    val enumClass = namespaced(curClass)
//    enumColl.foreach { case (id: Long, label: EnumValueSpec) =>
//      // This hack is needed because the lowest int64 literal is not supported in Nim
//      val const = if (s"$id" == "-9223372036854775808") "low(int64)" else s"$id"
//      out.puts(s"${label.name}* = ${enumClass}_$enumName($const)") }
//  }
  override def fileHeader(topClassName: String): Unit = {
    importList.add(config.nimModule)
    importList.add("options")
  }
  override def indent: String = "  "
  override def instanceCalculate(instName: Identifier, dataType: DataType, value: Ast.expr): Unit = {
    val cast = s"${ksToNim(dataType)}(${expression(value)})"
    handleAssignmentSimple(instName, cast)
  }
  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    out.puts(s"if this.${instanceFlagIdentifier(instName)}:")
    out.inc
    out.puts(s"return ${privateMemberName(instName)}")
    out.dec
  }
  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"proc ${idToStr(instName).dropRight(4)}(this: ${namespaced(className)}): ${ksToNim(dataType)} = ")
    out.inc
  }
  override def instanceFooter = {
    universalFooter
    out.puts
  }
  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit = {
    out.puts(s"this.${instanceFlagIdentifier(instName)} = true")
    out.puts(s"return ${privateMemberName(instName)}")
  }
  // def normalIO: String = ???
  override def outFileName(topClassName: String): String = s"$topClassName.nim"
  override def pushPos(io: String): Unit = out.puts(s"let pos = $io.pos()")
  override def popPos(io: String): Unit = out.puts(s"$io.seek(pos)")
  override def readFooter(): Unit = {
    universalFooter
    out.puts
  }
  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = {
    val t = namespaced(typeProvider.nowClass.name)
    val p = ksToNim(typeProvider.nowClass.parentType)
    val r = namespaced(typeProvider.topClass.name)
    val paramsArg = Utils.join(typeProvider.nowClass.params.map((p) =>
      s"${paramName(p.id)}: any"
    ), ", ", ", ", "")

    endian match {
      case None =>
        out.puts(s"proc read*(_: typedesc[$t], io: KaitaiStream, root: KaitaiStruct, parent: $p$paramsArg): $t =")
        out.inc
        out.puts("template this: untyped = result")
        out.puts(s"this = new($t)")
        // The cast in the if clause is used to bypass semantic analysis
        // The cast in the else clause should be a normal type conversion instead,
        // but for some reason it doesn't work. Needs further investigation
        out.puts(s"let root = if root == nil: cast[$r](this) else: cast[$r](root)")
        out.puts(s"this.io = io")
        out.puts(s"this.root = root")
        out.puts(s"this.parent = parent")
        typeProvider.nowClass.params.foreach((p) => handleAssignmentSimple(p.id, s"${ksToNim(p.dataType)}(${paramName(p.id)})"))

        typeProvider.nowClass.meta.endian match {
          case Some(_: CalcEndian) =>
            out.puts(s"this.${idToStr(EndianIdentifier)} = false")
          case Some(InheritedEndian) =>
            out.puts(s"this.${idToStr(EndianIdentifier)} = " +
              s"this.${idToStr(ParentIdentifier)}." +
              s"${idToStr(EndianIdentifier)}")
          case _ =>
        }
        out.puts
      case Some(e) =>
        out.puts
        out.puts(s"proc read${camelCase(e.toSuffix, true)}(this: $t) =")
        out.inc
    }
  }
  // def results(topClass: ClassSpec): Map[String, String] = ???
  override def runRead(name: List[String]): Unit = out.puts("read()") // TODO: missing type argument
  override def runReadCalc(): Unit = {
    out.puts
    out.puts("if this.isLe:")
    out.inc
    out.puts("readLe(this)")
    out.dec
    out.puts("else:")
    out.inc
    out.puts("readBe(this)")
    out.dec
  }
  override def seek(io: String, pos: Ast.expr): Unit = out.puts(s"$io.seek(int(${expression(pos)}))")
  override def useIO(ioEx: Ast.expr): String = {
    out.puts(s"let io = ${expression(ioEx)}")
    "io"
  }
  override def classForwardDeclaration(name: List[String]): Unit = {
    val t = namespaced(typeProvider.nowClass.name)
    val p = ksToNim(typeProvider.nowClass.parentType)
    val paramsArg = Utils.join(typeProvider.nowClass.params.map((p) =>
      s"${paramName(p.id)}: any"
    ), ", ", ", ", "")

    out.puts(s"proc read*(_: typedesc[$t], io: KaitaiStream, root: KaitaiStruct, parent: $p$paramsArg): $t")
  }

  // Members declared in io.kaitai.struct.languages.components.ObjectOrientedLanguage
  override def idToStr(id: Identifier): String = {
    id match {
      case IoIdentifier => "io"
      case NamedIdentifier(name) =>  camelCase(name, false)
      case InstanceIdentifier(name) => camelCase(name, false) + "Inst"
      case IoStorageIdentifier(innerId) => "io" + camelCase(idToStr(innerId), true)
      case SpecialIdentifier(name) => camelCase(name, false)
      case NumberedIdentifier(idx) => s"${NumberedIdentifier.TEMPLATE}$idx"
      case RawIdentifier(innerId) => "raw" + camelCase(idToStr(innerId), true)
    }
  }
  override def localTemporaryName(id: Identifier): String = idToStr(id)
  override def privateMemberName(id: Identifier): String = {
    val name = idToStr(id)
    val prefix = "this"
    s"$prefix.$name"
  }
  override def publicMemberName(id: Identifier): String = idToStr(id)

  // Members declared in io.kaitai.struct.languages.components.EveryReadIsExpression
  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Seq[Byte]], include: Boolean): String = {
    val expr1 = padRight match {
      case Some(padByte) => s"$expr0.bytesStripRight($padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) =>
        if (term.length == 1) {
          val t = term.head & 0xff
          s"$expr1.bytesTerminate($t, $include)"
        } else {
          s"$expr1.bytesTerminateMulti(${translator.doByteArrayLiteral(term)}, $include)"
        }
      case None => expr1
    }
    expr2
  }
  def handleAssignmentIterative(id: Identifier, expr: String): Unit = {
    // Need better design for this XXX
    val exprName = id match {
      case _: RawIdentifier => translator.doName(Identifier.ITERATOR2)
      case _ => translator.doName(Identifier.ITERATOR)
    }
    out.puts(s"let $exprName = $expr")
    out.puts(s"${privateMemberName(id)}.add($exprName)")
  }
  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    handleAssignmentIterative(id, expr)
  }
  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit = {
    handleAssignmentIterative(id, expr)
  }
  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    handleAssignmentIterative(id, expr)
  }
  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    // Need better design for this XXX
    val exprName = idToStr(id) + "Expr"
    out.puts(s"let $exprName = $expr")
    out.puts(s"${privateMemberName(id)} = $exprName")
  }
  override def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit = {}
  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    val expr = dataType match {
      case t: ReadableType =>
        s"$io.read${Utils.capitalize(t.apiCall(defEndian))}()"
      case blt: BytesLimitType =>
        s"$io.readBytes(int(${expression(blt.size)}))"
      case _: BytesEosType =>
        s"$io.readBytesFull()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        if (terminator.length == 1) {
          val term = terminator.head & 0xff
          s"$io.readBytesTerm($term, $include, $consume, $eosError)"
        } else {
          s"$io.readBytesTermMulti(${translator.doByteArrayLiteral(terminator)}, $include, $consume, $eosError)"
        }
      case BitsType1(bitEndian) =>
        s"$io.readBitsInt${camelCase(bitEndian.toSuffix, true)}(1) != 0"
      case BitsType(width: Int, bitEndian) =>
        s"$io.readBitsInt${camelCase(bitEndian.toSuffix, true)}($width)"
      case t: UserType =>
        val (parent, root) = if (t.isExternal(typeProvider.nowClass)) {
          ("nil", "nil")
        } else {
          val parent = t.forcedParent match {
            case Some(USER_TYPE_NO_PARENT) => "nil"
            case Some(fp) => translator.translate(fp)
            case None => "this"
          }
          (parent, "this.root")
        }
        val addParams = Utils.join(t.args.map((a) => translator.translate(a)), ", ", ", ", "")
        val concreteName = namespaced(t.classSpec match {
          case Some(cs) => cs.name
          case None => t.name
        })
        // FIXME: apparently, the Nim compiler uses a different order of
        // `$parent` and `$root` than literally every other language
        s"${concreteName}.read($io, $root, $parent$addParams)"
    }

    if (assignType != dataType) {
      s"${ksToNim(assignType)}($expr)"
    } else {
      expr
    }
  }
  override def userTypeDebugRead(id: String, dataType: DataType, assignType: DataType): Unit = {} // TODO

  // Members declared in io.kaitai.struct.languages.components.SwitchOps
  override def switchCasesUsingIf[T](
    id: Identifier,
    on: Ast.expr,
    onType: DataType,
    cases: Map[Ast.expr, T],
    normalCaseProc: (T) => Unit,
    elseCaseProc: (T) => Unit
  ): Unit = {
    switchIfStart(id, on, onType)

    // Pass 1: only normal case clauses
    var first = true

    cases.foreach { case (condition, result) =>
      condition match {
        case SwitchType.ELSE_CONST =>
          // skip for now
        case _ =>
          if (first) {
            switchIfCaseFirstStart(condition)
            first = false
          } else {
            switchIfCaseStart(condition)
          }
          normalCaseProc(result)
          switchIfCaseEnd()
      }
    }

    // Pass 2: else clause, if it is there
    cases.get(SwitchType.ELSE_CONST).foreach { (result) =>
      if (cases.size == 1) {
        elseCaseProc(result)
      } else {
        switchIfElseStart()
        elseCaseProc(result)
        switchIfElseEnd()
      }
    }

    switchIfEnd()
  }

  override def switchCaseEnd(): Unit = universalFooter
  override def switchCaseStart(condition: Ast.expr): Unit = {}
  override def switchElseStart(): Unit = {}
  override def switchEnd(): Unit = {}
  override def switchStart(id: Identifier, on: Ast.expr): Unit = {}

  // Members declared in io.kaitai.struct.languages.components.SwitchIfOps
  override def switchRequiresIfs(onType: DataType): Boolean = true
  override def switchIfStart(id: Identifier, on: Ast.expr, onType: DataType): Unit = {
    out.puts("block:")
    out.inc
    out.puts(s"let on = ${expression(on)}")
  }
  override def switchIfCaseFirstStart(condition: Ast.expr): Unit = {
    out.puts(s"if on == ${expression(condition)}:")
    out.inc
  }
  override def switchIfCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"elif on == ${expression(condition)}:")
    out.inc
  }
  override def switchIfCaseEnd(): Unit = out.dec
  override def switchIfElseStart(): Unit = {
    out.puts("else:")
    out.inc
  }
  override def switchIfEnd(): Unit = out.dec

  // Members declared in io.kaitai.struct.languages.components.UniversalDoc
  override def universalDoc(doc: DocSpec): Unit = {
    out.puts
    out.puts( "##[")
    doc.summary.foreach(summary => out.puts(summary))
    doc.ref.foreach {
      case TextRef(text) =>
        out.puts("@see \"" + text + "\"")
      case ref: UrlRef =>
        out.puts(s"@see ${ref.toAhref}")
    }
    out.puts( "]##")
  }

  def instanceFlagIdentifier(id: InstanceIdentifier) = s"${idToStr(id)}Flag"
}

object NimCompiler extends LanguageCompilerStatic
  with StreamStructNames
  with UpperCamelCaseClasses
  with ExceptionNames {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new NimCompiler(tp, config)

  // Members declared in io.kaitai.struct.languages.components.StreamStructNames
  override def kstreamName: String = "KaitaiStream"
  override def kstructName: String = "KaitaiStruct"
  def ksErrorName(err: KSError): String = "KaitaiError" // TODO: maybe add more debugging info

  def camelCase(s: String, upper: Boolean): String = {
    if (upper) {
      s.split("_").map(Utils.capitalize).mkString
    } else {
      if (s.startsWith("_")) {
        camelCase(s.substring(1), false)
      } else {
        val firstWord :: restWords = s.split("_").toList
        (firstWord :: restWords.map(Utils.capitalize)).mkString
      }
    }
  }

  def namespaced(names: List[String]): String = names.map(n => camelCase(n, true)).mkString("_")

  def ksToNim(attrType: DataType): String = {
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

      case BitsType(_, _) => "uint64"

      case _: BooleanType => "bool"
      case CalcIntType => "int"
      case CalcFloatType => "float64"

      case _: StrType => "string"
      case _: BytesType => "seq[byte]"

      case KaitaiStructType | CalcKaitaiStructType(_) => "KaitaiStruct"
      case KaitaiStreamType | OwnedKaitaiStreamType => "KaitaiStream"

      case t: UserType => namespaced(t.classSpec match {
        case Some(cs) => cs.name
        case None => t.name
      })

      case t: EnumType => namespaced(t.enumSpec.get.name)
      case at: ArrayType => s"seq[${ksToNim(at.elType)}]"
      case st: SwitchType => ksToNim(st.combinedType)

      case AnyType => "KaitaiStruct"
    }
  }
}
