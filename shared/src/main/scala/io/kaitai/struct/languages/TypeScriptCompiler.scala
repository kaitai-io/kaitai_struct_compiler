package io.kaitai.struct.languages

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype.{CalcEndian, Endianness, DataType, FixedEndian, InheritedEndian}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.TypeScriptTranslator
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig, Utils}

class TypeScriptCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with ObjectOrientedLanguage
    with UpperCamelCaseClasses
    with SingleOutputFile
    with UniversalDoc
    with AllocateIOLocalVar
    with EveryReadIsExpression
    with FixedContentsUsingArrayByteLiteral {
  import TypeScriptCompiler._

  override val translator = new TypeScriptTranslator(typeProvider)

  override def indent: String = "  "
  override def outFileName(topClassName: String): String = s"${type2class(topClassName)}.ts"

  override def outImports(topClass: ClassSpec) = {
    val impList = importList.toList
    val quotedImpList = impList.map((x) => s"'$x'")

    // the following goes at the top of the file
    "const func = function(KaitaiStream) {"
  }

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"// $headerComment")
    outHeader.puts

    outHeader.puts("interface IDebug {")
    outHeader.inc
    outHeader.puts("start?: number;")
    outHeader.puts("ioOffset?: number;")
    outHeader.puts("end?: number;")
    outHeader.puts("arr?: IDebug[];")
    outHeader.puts("enumName?: string;")
    outHeader.puts("[key: string]: number | string | IDebug | IDebug[] | undefined;")
    outHeader.dec
    outHeader.puts("}")

    importList.add("kaitai-struct/KaitaiStream")
  }

  override def fileFooter(name: String): Unit = {
    out.puts(s"return ${type2class(name)};")
    out.dec
    out.puts("}")
    out.puts
    out.puts("export default func;")
    out.puts
  }

  override def opaqueClassDeclaration(classSpec: ClassSpec): Unit = {

  }

  override def classHeader(name: List[String]): Unit = {
    val shortClassName = type2class(name.last)

    out.puts

    if (name.size > 1) {
      out.puts(s"static $shortClassName = class {")
    } else {
      out.puts(s"class $shortClassName {")
    }

    out.inc
  }

  override def classFooter(name: List[String]): Unit = {
    out.dec
    out.puts("}")
    out.puts
  }

  override def classConstructorHeader(name: List[String], parentClassName: DataType, rootClassName: List[String], isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    if (config.readStoresPos) {
      out.puts("public _debug!: IDebug;")
    }

    typeProvider.nowClass.meta.endian match {
      case Some(_: CalcEndian) | Some(InheritedEndian) =>
        out.puts(s"public _is_le!: boolean;")
      case _ =>
        // no _is_le variable
    }

    val endianSuffix = if (isHybrid) {
      ", _is_le?: any"
    } else {
      ""
    }

    val paramsList = Utils.join(params.map((p) => s"${paramName(p.id)}?: ${kaitaiType2NativeType(p.dataType)}"), ", ", ", ", "")

    // Parameter names
    val pIo = paramName(IoIdentifier)
    val pParent = paramName(ParentIdentifier)
    val pRoot = paramName(RootIdentifier)

    out.puts(s"constructor(public _io: any, parent: any, root?: any$endianSuffix$paramsList) {")
    out.inc
    out.puts(s"this.$pParent = parent;")
    out.puts(s"this.$pRoot = root || this;")

    if (isHybrid)
      out.puts("this._is_le = _is_le;")

    params.foreach((p) => handleAssignmentSimple(p.id, paramName(p.id)))

    out.puts

    if (config.readStoresPos) {
      out.puts("this._debug = {};")
    }

  }

  override def classConstructorFooter: Unit = {
    out.dec
    out.puts("}")
    out.puts
  }

  override def runRead(): Unit = {
    out.puts("this._read();")
  }

  override def runReadCalc(): Unit = {
    out.puts
    out.puts(s"if (this._is_le === true) {")
    out.inc
    out.puts("this._readLE();")
    out.dec
    out.puts("} else if (this._is_le === false) {")
    out.inc
    out.puts("this._readBE();")
    out.dec
    out.puts("} else {")
    out.inc
    out.puts("throw new KaitaiStream.UndecidedEndiannessError();")
    out.dec
    out.puts("}")
  }

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean) = {
    val suffix = endian match {
      case Some(e) => e.toSuffix.toUpperCase
      case None => ""
    }
    out.puts(s"public _read$suffix() {")
    out.inc
  }

  override def readFooter() = {
    out.dec
    out.puts("}")
    out.puts
  }

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {}

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"public ${privateMemberName(attrName).substring(5)}!: ${kaitaiType2NativeType(attrType.asNonOwning, false)};")
  }

  override def universalDoc(doc: DocSpec): Unit = {
    // JSDoc docstring style: http://usejsdoc.org/about-getting-started.html
    out.puts
    out.puts( "/**")

    doc.summary.foreach((summary) => out.putsLines(" * ", summary))

    // http://usejsdoc.org/tags-see.html
    doc.ref match {
      case TextRef(text) =>
        out.putsLines(" * ", s"@see $text")
      case UrlRef(url, text) =>
        out.putsLines(" * ", s"@see {@link $url|$text}")
      case NoRef =>
    }

    out.puts( " */")
  }

  // override def attrParse(attr: AttrLikeSpec, id: Identifier, defEndian: Option[Endianness]): Unit = {
  //   out.puts(s"this.meta[${privateMemberName(id)}] = '$attr'")
  // }

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {
    out.puts("if (this._is_le) {")
    out.inc
    leProc()
    out.dec
    out.puts("} else {")
    out.inc
    beProc()
    out.dec
    out.puts("}")
  }

  override def attrFixedContentsParse(attrName: Identifier, contents: String): Unit = {
    out.puts(s"${privateMemberName(attrName)} = " +
      s"$normalIO.ensureFixedContents($contents);")
  }

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit = {
    val srcName = privateMemberName(varSrc)
    val destName = privateMemberName(varDest)

    proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "processXorOne"
          case _: BytesType => "processXorMany"
        }
        out.puts(s"$destName = $kstreamName.$procName($srcName, ${expression(xorValue)});")
      case ProcessZlib =>
        out.puts(s"$destName = $kstreamName.processZlib($srcName);")
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        out.puts(s"$destName = $kstreamName.processRotateLeft($srcName, $expr, 1);")
      case ProcessCustom(name, args) =>
        val nameInit = name.init
        val pkgName = if (nameInit.isEmpty) "" else nameInit.mkString("-") + "/"
        val procClass = type2class(name.last)

        importList.add(s"$pkgName$procClass")

        out.puts(s"let _process = new $procClass(${args.map(expression).mkString(", ")});")
        out.puts(s"$destName = _process.decode($srcName);")
    }
  }

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = {
    val langName = idToStr(varName)
    val memberCall = s"${privateMemberName(varName)}"

    val ioName = s"_io_$langName"

    val args = rep match {
      case RepeatEos | RepeatUntil(_) => s"$memberCall[$memberCall.length - 1]"
      case RepeatExpr(_) => s"$memberCall[i]"
      case NoRepeat => memberCall
    }

    out.puts(s"let $ioName = new $kstreamName($args);")
    ioName
  }

  override def useIO(ioEx: expr): String = {
    out.puts(s"let io = ${expression(ioEx)};")
    "io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"let _pos = $io.pos;")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io.seek(${expression(pos)});")

  override def popPos(io: String): Unit =
    out.puts(s"$io.seek(_pos);")

  override def alignToByte(io: String): Unit =
    out.puts(s"$io.alignToByte();")

  override def attrDebugStart(attrId: Identifier, attrType: DataType, io: Option[String], rep: RepeatSpec): Unit = {
    if (!attrDebugNeeded(attrId))
      return

    val debugName = attrDebugName(attrId, rep, false)

    val ioProps = io match {
      case None => ""
      case Some(x) => s"start: $x.pos, ioOffset: $x._byteOffset"
    }

    val enumNameProps = attrType match {
      case t: EnumType => s"""enumName: \"${types2class(t.enumSpec.get.name)}\""""
      case _ => ""
    }

    out.puts(s"$debugName = { $ioProps${if (ioProps != "" && enumNameProps != "") ", " else ""}$enumNameProps };")
  }

  override def attrDebugEnd(attrId: Identifier, attrType: DataType, io: String, rep: RepeatSpec): Unit = {
    if (!attrDebugNeeded(attrId))
      return
    val debugName = attrDebugName(attrId, rep, true)

    out.puts(s"$debugName.end = $io.pos;")
  }

  override def condIfHeader(expr: expr): Unit = {
    out.puts(s"if (${expression(expr)}) {")
    out.inc
  }

  override def condIfFooter(expr: expr): Unit = {
    out.dec
    out.puts("}")
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = [];")
    out.puts(s"${privateMemberName(id)} = [];")
    if (config.readStoresPos)
      out.puts(s"this._debug.${idToStr(id)}.arr = [];")
    out.puts("var i = 0;")
    out.puts(s"while (!$io.isEof()) {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}.push($expr);")
  }

  override def condRepeatEosFooter: Unit = {
    out.puts("i++;")
    out.dec
    out.puts("}")
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = new Array(${expression(repeatExpr)});")
    out.puts(s"${privateMemberName(id)} = new Array(${expression(repeatExpr)});")
    if (config.readStoresPos)
      out.puts(s"this._debug.${idToStr(id)}.arr = new Array(${expression(repeatExpr)});")
    out.puts(s"for (let i = 0; i < ${expression(repeatExpr)}; i++) {")
    out.inc
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}[i] = $expr;")
  }

  override def condRepeatExprFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, untilExpr: expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = []")
    out.puts(s"${privateMemberName(id)} = []")
    if (config.readStoresPos)
      out.puts(s"this._debug.${idToStr(id)}.arr = [];")
    out.puts("var i = 0;")
    out.puts("do {")
    out.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val tmpName = translator.doName(if (isRaw) Identifier.ITERATOR2 else Identifier.ITERATOR)
    out.puts(s"var $tmpName = $expr;")
    out.puts(s"${privateMemberName(id)}.push($tmpName);")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, untilExpr: expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.puts("i++;")
    out.dec
    out.puts(s"} while (!(${expression(untilExpr)}));")
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    if (expr.startsWith("["))
      // ensures that byte arrays are treated appropriately
      out.puts(s"${privateMemberName(id)} = new Uint8Array($expr);")
    else {
      out.puts(s"${privateMemberName(id)} = $expr;")
    }
  }

  override def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit =
    out.puts(s"var $id = $expr;")

  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    dataType match {
      case t: ReadableType =>
        s"$io.read${Utils.capitalize(t.apiCall(defEndian))}()"
      case blt: BytesLimitType =>
        s"$io.readBytes(${expression(blt.size)})"
      case _: BytesEosType =>
        s"$io.readBytesFull()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        s"$io.readBytesTerm($terminator, $include, $consume, $eosError)"
      case BitsType1 =>
        s"$io.readBitsInt(1) != 0"
      case BitsType(width: Int) =>
        s"$io.readBitsInt($width)"
      case t: UserType =>
        val parent = t.forcedParent match {
          case Some(USER_TYPE_NO_PARENT) => "null"
          case Some(fp) => translator.translate(fp)
          case None => "this"
        }
        val root = if (t.isOpaque) "null" else "this._root"
        val addEndian = t.classSpec.get.meta.endian match {
          case Some(InheritedEndian) => ", this._is_le"
          case _ => ""
        }
        val addParams = Utils.join(t.args.map((a) => translator.translate(a)), ", ", ", ", "")
        s"new ${types2class(t.classSpec.get.name)}($io, $parent, $root$addEndian$addParams)"
    }
  }

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean) = {
    val expr1 = padRight match {
      case Some(padByte) => s"$kstreamName.bytesStripRight($expr0, $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) => s"$kstreamName.bytesTerminate($expr1, $term, $include)"
      case None => expr1
    }
    expr2
  }

  override def userTypeDebugRead(id: String): Unit = {
    val incThis = if (id.startsWith("_t_")) "" else "this."
    out.puts(s"$id._read();")
  }

  /**
    * Designates switch mode. If false, we're doing real switch-case for this
    * attribute. If true, we're doing if-based emulation.
    */
  var switchIfs = false

  val NAME_SWITCH_ON = Ast.expr.Name(Ast.identifier(Identifier.SWITCH_ON))

  override def switchStart(id: Identifier, on: Ast.expr): Unit = {
    val onType = translator.detectType(on)
    typeProvider._currentSwitchType = Some(onType)

    // Determine switching mode for this construct based on type
    switchIfs = onType match {
      case _: IntType | _: BooleanType | _: EnumType | _: StrType => false
      case _ => true
    }

    if (switchIfs) {
      out.puts("{")
      out.inc
      out.puts(s"let ${expression(NAME_SWITCH_ON)} = ${expression(on)};")
    } else {
      // makes the typescript compiler happy when expression(on) evaluates to a number literal
      out.puts(s"var __temp__ = ${expression(on)};")
      out.puts("switch (__temp__) {")
      out.inc
    }
  }

  def switchCmpExpr(condition: Ast.expr): String =
    expression(
      Ast.expr.Compare(
        NAME_SWITCH_ON,
        Ast.cmpop.Eq,
        condition
      )
    )

  override def switchCaseFirstStart(condition: Ast.expr): Unit = {
    if (switchIfs) {
      out.puts(s"if (${switchCmpExpr(condition)}) {")
      out.inc
    } else {
      switchCaseStart(condition)
    }
  }

  override def switchCaseStart(condition: Ast.expr): Unit = {
    if (switchIfs) {
      out.puts(s"else if (${switchCmpExpr(condition)}) {")
      out.inc
    } else {
      out.puts(s"case ${expression(condition)}: {")
      out.inc
    }
  }

  override def switchCaseEnd(): Unit = {
    if (switchIfs) {
      out.dec
      out.puts("}")
    } else {
      out.puts("break;")
      out.dec
      out.puts("}")
    }
    
  }

  override def switchElseStart(): Unit = {
    if (switchIfs) {
      out.puts("else {")
      out.inc
    } else {
      out.puts("default: {")
      out.inc
    }
  }

  override def switchEnd(): Unit = {
    out.dec
    out.puts("}")
  }

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    // val typeOut = dataType match {
    //   case SwitchType(_, cases, _) => {
    //     cases.map(kv => {
    //       val b = StringBuilder.newBuilder
    //       if (kv._2.isInstanceOf[UserType]) {
    //         b.append("typeof ")
    //         b.append(kaitaiType2NativeType(kv._2, false))
    //         b.append(".prototype")
    //       } else {
    //         b.append(kaitaiType2NativeType(kv._2, false))
    //       }
    //       b.toString()
    //     }).mkString(" | ")
    //   }
    //   case otherType => {
    //     val b = StringBuilder.newBuilder
    //     if (otherType.isInstanceOf[UserType]) {
    //       b.append("typeof ")
    //       b.append(kaitaiType2NativeType(kv._2, false))
    //       b.append(".prototype")
    //     } else {
    //       b.append(kaitaiType2NativeType(kv._2, false))
    //     }
    //     b.toString()
    //   }
    // }

    out.puts(s"private ${privateMemberName(instName).substring(5)}!: ${kaitaiType2NativeType(dataType, false)};")
    // out.puts(s"private ${privateMemberName(instName).substring(5)}!: $typeof${kaitaiType2NativeType(dataType, false)}$prototype$arr;")
    out.puts(s"get ${publicMemberName(instName)}() {")
    out.inc
  }

  override def instanceClear(instName: InstanceIdentifier) = {
    out.puts(s"${privateMemberName(instName)}")
  }

  override def instanceFooter: Unit = {
    out.dec
    out.puts("}")
    out.puts
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    out.puts(s"if (${privateMemberName(instName)} !== undefined)")
    out.inc
    instanceReturn(instName, dataType)
    out.dec
  }

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit = {
    out.puts(s"return ${privateMemberName(instName)};")
  }

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, EnumValueSpec)]): Unit = {
    out.puts(s"static ${type2class(enumName)} = Object.freeze({")
    out.inc

    // Name to ID mapping
    enumColl.foreach { case (id, label) =>
      out.puts(s"${enumValue(enumName, label.name)}: $id,")
    }
    out.puts

    // ID to name mapping
    enumColl.foreach { case (id, label) =>
      val idStr = if (id < 0) {
        "\"" + id.toString + "\""
      } else {
        id.toString
      }
      out.puts(s"""$idStr: "${enumValue(enumName, label.name)}",""")
    }

    out.dec
    out.puts("});")
    out.puts
  }

  def enumValue(enumName: String, label: String) = label.toUpperCase

  override def debugClassSequence(seq: List[AttrSpec]) = {
    val seqStr = seq.map((attr) => "\"" + idToStr(attr.id) + "\"").mkString(", ")
    out.puts(s"public SEQ_FIELDS = [$seqStr]")
  }

  def idToStr(id: Identifier): String = {
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => Utils.lowerCamelCase(name)
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => s"_m_${Utils.lowerCamelCase(name)}"
      case RawIdentifier(innerId) => "_raw_" + idToStr(innerId)
    }
  }

  override def privateMemberName(id: Identifier): String = s"this.${idToStr(id)}"

  override def publicMemberName(id: Identifier): String = {
    id match {
      case NamedIdentifier(name) => Utils.lowerCamelCase(name)
      case InstanceIdentifier(name) => Utils.lowerCamelCase(name)
    }
  }

  override def localTemporaryName(id: Identifier): String = s"_t_${idToStr(id)}"

  private
  def attrDebugNeeded(attrId: Identifier) = attrId match {
    case _: NamedIdentifier | _: NumberedIdentifier | _: InstanceIdentifier => true
    case _: RawIdentifier | _: SpecialIdentifier => false
  }

  def attrDebugName(attrId: Identifier, rep: RepeatSpec, end: Boolean) = {
    val arrIndexExpr = rep match {
      case NoRepeat => ""
      case _: RepeatExpr => ".arr[i]"
      case RepeatEos | _: RepeatUntil => s".arr[${privateMemberName(attrId)}.length${if (end) " - 1" else ""}]"
    }

    s"this._debug.${idToStr(attrId)}$arrIndexExpr"
  }
}

object TypeScriptCompiler extends LanguageCompilerStatic
  with UpperCamelCaseClasses
  with StreamStructNames {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new TypeScriptCompiler(tp, config)

  def kaitaiType2NativeType(attrType: DataType, absolute: Boolean = false): String = {
    attrType match {
      // for if/when BigInt support is added in js/ts runtime
      // case IntMultiType(false, Width8, _) => "BigInt"
      // case IntMultiType(true, Width8, _) => "number"
      case _: NumericType => "number"

      case BitsType(_) => "number"

      case _: BooleanType => "boolean"
      case CalcIntType => "number"
      case CalcFloatType => "number"

      case _: StrType => "string"
      case _: BytesType => "Uint8Array"

      case t: UserType =>
        s"typeof ${types2class(t.classSpec match {
          case None => t.name
          case Some(cs) => cs.name
        })}.prototype"

      // for now enums are numbers by default
      case t: EnumType => "number" //types2class(t.enumSpec.get.name)

      case ArrayType(inType) =>
        s"(${kaitaiType2NativeType(inType, absolute)})[]"

      case CalcArrayType(inType) => s"(${kaitaiType2NativeType(inType, absolute)})[]"

      case AnyType => "any"

      case KaitaiStreamType => s"$kstreamName*"
      case KaitaiStructType | CalcKaitaiStructType => "any"

      // unfolds and removes duplicates by converting to set
      case SwitchType(_, cases, _) => cases.map(kv => kaitaiType2NativeType(kv._2, false)).toSet.mkString(" | ")
    }
  }

  override def kstreamName: String = "KaitaiStream"

  // FIXME: probably KaitaiStruct will emerge some day in JavaScript runtime, but for now it is unused
  override def kstructName: String = ???

  def types2class(types: List[String]): String = types.map(type2class).mkString(".")

  override def type2class(name: String): String = Utils.upperCamelCase(name)
}
