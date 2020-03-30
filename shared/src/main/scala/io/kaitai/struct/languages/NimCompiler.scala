package io.kaitai.struct.languages

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.NimTranslator
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig, Utils}

class NimCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with SingleOutputFile
    with EveryReadIsExpression
    with UpperCamelCaseClasses
    with FixedContentsUsingArrayByteLiteral
    with UniversalFooter
    with CommonReads
    with AllocateIOLocalVar {

  import NimCompiler._

  // Written from scratch
  def imports = importList.toList.map((x) => s"import $x").mkString("\n")
  def namespaced(names: List[String]): String = names.map(n => camelCase(n, true)).mkString("_")
  def sectionHeader(className: List[String]): Unit = out.puts("### " + namespaced(className) + " ###")
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
  def destructor(name: List[String]): Unit = {
    val n = namespaced(name) + "Obj"
    out.puts(s"proc `=destroy`(x: var $n) =")
    out.inc
    out.puts(s"close(x.io)")
    out.dec
    out.puts
  }
  override def innerEnums = false
  override val translator: NimTranslator = new NimTranslator(typeProvider, importList)

  override def universalFooter: Unit = {
    out.dec
  }

  override def allocateIO(id: Identifier, rep: RepeatSpec): String = {
    val nimName = idToStr(id)

    val ioName = s"${nimName}Io"

    out.puts(s"let $ioName = newKaitaiStringStream($nimName)")
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
    out.puts(s"result.${idToStr(attrName)} = $normalIO.ensureFixedContents($contents)")
  }
  // def attrParse(attr: AttrLikeSpec, id: Identifier, defEndian: Option[Endianness]): Unit = ???
  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {
    out.puts("if result.isLe:")
    out.inc
    leProc()
    out.dec
    out.puts("else:")
    out.inc
    beProc()
    out.dec
  }
  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec): Unit = {
    val srcExpr = idToStr(varSrc)

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
        s"$srcExpr.processRotateLeft($expr, 1)"
      case ProcessCustom(name, args) =>
        val namespace = name.init.mkString(".")
        val procClass = namespace +
          (if (namespace.nonEmpty) "." else "") +
          type2class(name.last)
        val procName = s"process_${idToStr(varSrc)}"
        out.puts(s"$procName = $procClass(${args.map(expression).mkString(", ")})")
        s"$procName.decode($srcExpr)"
    }
    handleAssignment(varDest, expr, rep, false)
  }
    
  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"${idToStr(attrName)}*: ${ksToNim(attrType)}")
  }
  override def instanceDeclaration(attrName: InstanceIdentifier, attrType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"${idToStr(attrName)}*: Option[${ksToNim(attrType)}]")
  }
  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {}
  override def classConstructorHeader(name: List[String], parentType: DataType, rootClassName: List[String], isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {}
  override def classHeader(name: List[String]): Unit = {
    val t = namespaced(name)
    out.puts(s"${t}* = ref ${t}Obj")
    out.puts(s"${t}Obj* = object")
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
  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw): Unit = {
    out.puts(s"${privateMemberName(id)} = newSeq[${ksToNim(dataType)}]()")
    out.puts("block:")
    out.inc
    out.puts("var i: int")
    out.puts(s"while not $io.eof:")
    out.inc
  }
  override def condRepeatEosFooter: Unit = {
    out.puts("inc i")
    out.dec
    out.dec
  }
  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw, repeatExpr: Ast.expr): Unit = {
    if (needRaw.level >= 1)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = newString(${expression(repeatExpr)})")
    if (needRaw.level >= 2)
      out.puts(s"${privateMemberName(RawIdentifier(RawIdentifier(id)))} = newString(${expression(repeatExpr)})")
    out.puts(s"${idToStr(id)} = newSeq[${ksToNim(dataType)}](${expression(repeatExpr)})")
    out.puts(s"for i in 0 ..< ${expression(repeatExpr)}:")
    out.inc
  }
  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw, repeatExpr: Ast.expr): Unit = {
    out.puts(s"${privateMemberName(id)} = newSeq[${ksToNim(dataType)}]()")
    out.puts("block:")
    out.inc
    out.puts(s"${ksToNim(dataType)} ${translator.doName("_")};")
    out.puts("var i: int")
    out.puts("while true:")
    out.inc
  }
  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw, repeatExpr: Ast.expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.puts(s"if ${expression(repeatExpr)}:")
    out.inc
    out.puts("break")
    out.dec
    out.puts("inc i")
    out.dec
  }
  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, EnumValueSpec)]): Unit = {
    val enumClass = namespaced(curClass)
    out.puts(s"${enumClass}_$enumName* = enum")
    out.inc
    enumColl.foreach { case (id: Long, label: EnumValueSpec) => out.puts(s"${label.name} = $id") }
    out.dec
  }
  override def fileHeader(topClassName: String): Unit = {
    importList.add(config.nimModule)
    importList.add("options")
  }
  override def indent: String = "  "
  // def instanceCalculate(instName: Identifier, dataType: DataType, value: Ast.expr): Unit = ???
  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    out.puts(s"if isSome(${privateMemberName(instName)}):")
    out.inc
    instanceReturn(instName, dataType)
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
    out.puts(s"return get(${privateMemberName(instName)})")
  }
  // def normalIO: String = ???
  override def outFileName(topClassName: String): String = s"$topClassName.nim"
  override def popPos(io: String): Unit = out.puts(s"$io.seek(pos)")
  override def pushPos(io: String): Unit = out.puts(s"let pos = $io.pos()")
  override def readFooter(): Unit = {
    out.puts("result = this")
    universalFooter
    out.puts
  }
  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = {
    val t = namespaced(typeProvider.nowClass.name)
    val p = ksToNim(typeProvider.nowClass.parentType)
    val r = namespaced(typeProvider.topClass.name)

    endian match {
      case None =>
        out.puts(s"proc read*(_: typedesc[$t], io: KaitaiStream, root: $r, parent: $p): $t =")
        out.inc
        out.puts(s"let this = new($t)")
        out.puts(s"let root = if root == nil: cast[$r](result) else: root")
        out.puts(s"this.io = io")
        out.puts(s"this.root = root")
        out.puts(s"this.parent = parent")

        typeProvider.nowClass.meta.endian match {
          case Some(_: CalcEndian) =>
            out.puts(s"result.${idToStr(EndianIdentifier)} = false")
          case Some(InheritedEndian) =>
            out.puts(s"result.${idToStr(EndianIdentifier)} = " +
              s"result.${idToStr(ParentIdentifier)}." +
              s"${idToStr(EndianIdentifier)}")
          case _ =>
        }
        out.puts
      case Some(e) =>
        out.puts
        out.puts(s"proc read${camelCase(e.toSuffix, true)}(subject: $t) =")
        out.inc
    }
  }
  // def results(topClass: ClassSpec): Map[String, String] = ???
  override def runRead(): Unit = out.puts("read()") // TODO: missing type argument
  override def runReadCalc(): Unit = {
    out.puts
    out.puts("if result.isLe:")
    out.inc
    out.puts("readLe(result)")
    out.dec
    out.puts("else:")
    out.inc
    out.puts("readBe(result)")
    out.dec
  }
  override def seek(io: String, pos: Ast.expr): Unit = out.puts(s"$io.seek(${expression(pos)})")
  // def type2class(className: String): String = ???
  override def useIO(ioEx: Ast.expr): String = {
    out.puts(s"let io = ${expression(ioEx)}")
    "io"
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
  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean): String = {
    val expr1 = padRight match {
      case Some(padByte) => s"$expr0.bytesStripRight($padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) => s"$expr1.bytesTerminate($term, $include)"
      case None => expr1
    }
    expr2
  }
  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}.add($expr)")
  }
  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}.add($expr)")
  }
  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val tmpName = translator.doName(if (isRaw) Identifier.ITERATOR2 else Identifier.ITERATOR)
    out.puts(s"let $tmpName = $expr")
    out.puts(s"${privateMemberName(id)}.add($tmpName)")
  }
  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    out.puts(s"let ${localTemporaryName(id)} = $expr")
    if (idToStr(id).endsWith("Inst")) {
      out.puts(s"${privateMemberName(id)} = some(${localTemporaryName(id)})")
    } else {
      out.puts(s"${privateMemberName(id)} = ${localTemporaryName(id)}")
    }
  }
  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    val expr = dataType match {
      case t: ReadableType =>
        s"$io.read${Utils.capitalize(t.apiCall(defEndian))}()"
      case blt: BytesLimitType =>
        s"$io.readBytes(int(${expression(blt.size)}))"
      case _: BytesEosType =>
        s"$io.readBytesFull()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        s"$io.readBytesTerm($terminator, $include, $consume, $eosError)"
      case BitsType1 =>
        s"$io.readBitsInt(1) != 0"
      case BitsType(width: Int) =>
        s"$io.readBitsInt($width)"
      case t: UserType =>
        val addArgs = if (t.isOpaque) {
          ""
        } else {
          val parent = t.forcedParent match {
            case Some(USER_TYPE_NO_PARENT) => "nil"
            case Some(fp) => translator.translate(fp)
            case None => "this"
          }
          s", this.root, $parent"
        }
        val addParams = Utils.join(t.args.map((a) => translator.translate(a)), ", ", ", ", "")
        val concreteName = namespaced(t.classSpec match {
          case Some(cs) => cs.name
          case None => t.name
        })
        s"${concreteName}.read($io$addArgs$addParams)"
    }

    if (assignType != dataType) {
      s"${ksToNim(assignType)}($expr)"
    } else {
      expr
    }
  }

  // Members declared in io.kaitai.struct.languages.components.SwitchOps
  override def switchCaseEnd(): Unit = {} // TODO
  override def switchCaseStart(condition: Ast.expr): Unit = {} // TODO
  override def switchElseStart(): Unit = {} // TODO
  override def switchEnd(): Unit = {} // TODO
  override def switchStart(id: Identifier, on: Ast.expr): Unit = {} // TODO
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
  override def kstructName: String = "KSTRUCTNAME???"
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

      case BitsType(_) => "uint64"

      case _: BooleanType => "bool"
      case CalcIntType => "int"
      case CalcFloatType => "float64"

      case _: StrType => "string"
      case _: BytesType => "string"

      case KaitaiStructType | CalcKaitaiStructType => "ref RootObj"
      case KaitaiStreamType => "KaitaiStream"

      case t: UserType => namespaced(t.classSpec match {
        case Some(cs) => cs.name
        case None => t.name
      })

      case t: EnumType => namespaced(t.enumSpec.get.name)

      case at: ArrayType => s"seq[${ksToNim(at.elType)}]"

      case st: SwitchType => ksToNim(st.combinedType)
    }
  }
}
