package io.kaitai.struct.languages

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.JavaScriptTranslator
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig, Utils}

class JavaScriptCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with ObjectOrientedLanguage
    with UpperCamelCaseClasses
    with SingleOutputFile
    with UniversalDoc
    with AllocateIOLocalVar
    with EveryReadIsExpression
    with SwitchIfOps
    with FixedContentsUsingArrayByteLiteral {
  import JavaScriptCompiler._

  override val translator = new JavaScriptTranslator(typeProvider)

  override def indent: String = "  "
  override def outFileName(topClassName: String): String = s"${type2class(topClassName)}.js"

  override def outImports(topClass: ClassSpec) = {
    val impList = importList.toList
    val quotedImpList = impList.map((x) => s"'$x'")
    val defineArgs = quotedImpList.mkString(", ")
    val moduleArgs = quotedImpList.map((x) => s"require($x)").mkString(", ")
    val argClasses = impList.map((x) => x.split('/').last)
    val rootArgs = argClasses.map((x) => s"root.$x").mkString(", ")

    "(function (root, factory) {\n" +
      indent + "if (typeof define === 'function' && define.amd) {\n" +
      indent * 2 + s"define([$defineArgs], factory);\n" +
      indent + "} else if (typeof module === 'object' && module.exports) {\n" +
      indent * 2 + s"module.exports = factory($moduleArgs);\n" +
      indent + "} else {\n" +
      indent * 2 + s"root.${types2class(topClass.name)} = factory($rootArgs);\n" +
      indent + "}\n" +
      s"}(typeof self !== 'undefined' ? self : this, function (${argClasses.mkString(", ")}) {"
  }

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"// $headerComment")
    outHeader.puts

    importList.add("kaitai-struct/KaitaiStream")
  }

  override def fileFooter(name: String): Unit = {
    out.puts(s"return ${type2class(name)};")
    out.puts("}));")
  }

  override def opaqueClassDeclaration(classSpec: ClassSpec): Unit = {
    val className = type2class(classSpec.name.head)
    importList.add(s"./$className")
  }

  override def classHeader(name: List[String]): Unit = {
    val shortClassName = type2class(name.last)

    val addNameExpr = if (name.size > 1) {
      s" = ${types2class(name.takeRight(2))}"
    } else {
      ""
    }

    out.puts
    out.puts(s"var $shortClassName$addNameExpr = (function() {")
    out.inc
  }

  override def classFooter(name: List[String]): Unit = {
    out.puts
    out.puts(s"return ${type2class(name.last)};")
    out.dec
    out.puts("})();")
  }

  override def classConstructorHeader(name: List[String], parentClassName: DataType, rootClassName: List[String], isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    val endianSuffix = if (isHybrid) {
      ", _is_le"
    } else {
      ""
    }

    val paramsList = Utils.join(params.map((p) => paramName(p.id)), ", ", ", ", "")

    out.puts(s"function ${type2class(name.last)}(_io, _parent, _root$endianSuffix$paramsList) {")
    out.inc
    out.puts("this._io = _io;")
    out.puts("this._parent = _parent;")
    out.puts("this._root = _root || this;")

    if (isHybrid)
      out.puts("this._is_le = _is_le;")

    // Store parameters passed to us
    params.foreach((p) => handleAssignmentSimple(p.id, paramName(p.id)))

    if (config.readStoresPos) {
      out.puts("this._debug = {};")
    }
    out.puts
  }

  override def classConstructorFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def runRead(name: List[String]): Unit = {
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
      case Some(e) => Utils.upperUnderscoreCase(e.toSuffix)
      case None => ""
    }
    out.puts(s"${type2class(typeProvider.nowClass.name.last)}.prototype._read$suffix = function() {")
    out.inc
  }

  override def readFooter() = {
    out.dec
    out.puts("}")
  }

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {}

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {}

  override def universalDoc(doc: DocSpec): Unit = {
    // JSDoc docstring style: http://usejsdoc.org/about-getting-started.html
    out.puts
    out.puts( "/**")

    doc.summary.foreach(summary => out.putsLines(" * ", summary))

    // http://usejsdoc.org/tags-see.html
    doc.ref.foreach {
      case TextRef(text) =>
        out.putsLines(" * ", s"@see $text")
      case UrlRef(url, text) =>
        out.putsLines(" * ", s"@see {@link $url|$text}")
    }

    out.puts( " */")
  }

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

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec): Unit = {
    val srcExpr = getRawIdExpr(varSrc, rep)

    val expr = proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "processXorOne"
          case _: BytesType => "processXorMany"
        }
        s"$kstreamName.$procName($srcExpr, ${expression(xorValue)})"
      case ProcessZlib =>
        s"$kstreamName.processZlib($srcExpr)"
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        s"$kstreamName.processRotateLeft($srcExpr, $expr, 1)"
      case ProcessCustom(name, args) =>
        val nameInit = name.init
        val pkgName = if (nameInit.isEmpty) "" else nameInit.mkString("-") + "/"
        val procClass = type2class(name.last)

        importList.add(s"$pkgName$procClass")

        out.puts(s"var _process = new $procClass(${args.map(expression).mkString(", ")});")
        s"_process.decode($srcExpr)"
    }
    handleAssignment(varDest, expr, rep, false)
  }

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = {
    val langName = idToStr(varName)
    val memberCall = privateMemberName(varName)

    val ioName = s"_io_$langName"

    val args = getRawIdExpr(varName, rep)

    out.puts(s"var $ioName = new $kstreamName($args);")
    ioName
  }

  def getRawIdExpr(varName: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(varName)
    rep match {
      case NoRepeat => memberName
      case RepeatExpr(_) => s"$memberName[i]"
      case _ => s"$memberName[$memberName.length - 1]"
    }
  }

  override def useIO(ioEx: expr): String = {
    out.puts(s"var io = ${expression(ioEx)};")
    "io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"var _pos = $io.pos;")

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
      case Some(x) => s"start: $x.pos, ioOffset: $x.byteOffset"
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

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw): Unit = {
    if (needRaw.level >= 1)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = [];")
    if (needRaw.level >= 2)
      out.puts(s"${privateMemberName(RawIdentifier(RawIdentifier(id)))} = [];")
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

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw, repeatExpr: expr): Unit = {
    if (needRaw.level >= 1)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = new Array(${expression(repeatExpr)});")
    if (needRaw.level >= 2)
      out.puts(s"${privateMemberName(RawIdentifier(RawIdentifier(id)))} = new Array(${expression(repeatExpr)});")
    out.puts(s"${privateMemberName(id)} = new Array(${expression(repeatExpr)});")
    if (config.readStoresPos)
      out.puts(s"this._debug.${idToStr(id)}.arr = new Array(${expression(repeatExpr)});")
    out.puts(s"for (var i = 0; i < ${expression(repeatExpr)}; i++) {")
    out.inc
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}[i] = $expr;")
  }

  override def condRepeatExprFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw, untilExpr: expr): Unit = {
    if (needRaw.level >= 1)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = []")
    if (needRaw.level >= 2)
      out.puts(s"${privateMemberName(RawIdentifier(RawIdentifier(id)))} = [];")
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

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw, untilExpr: expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.puts("i++;")
    out.dec
    out.puts(s"} while (!(${expression(untilExpr)}));")
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)} = $expr;")
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
      case BitsType1(bitEndian) =>
        s"$io.readBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}(1) != 0"
      case BitsType(width: Int, bitEndian) =>
        s"$io.readBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}($width)"
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
        s"new ${types2class(t.name)}($io, $parent, $root$addEndian$addParams)"
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

  override def userTypeDebugRead(id: String, dataType: DataType, assignType: DataType): Unit = {
    out.puts(s"$id._read();")
  }

  override def switchRequiresIfs(onType: DataType): Boolean = onType match {
    case _: IntType | _: BooleanType | _: EnumType | _: StrType => false
    case _ => true
  }

  //<editor-fold desc="switching: true version">

  override def switchStart(id: Identifier, on: Ast.expr): Unit =
    out.puts(s"switch (${expression(on)}) {")

  override def switchCaseFirstStart(condition: Ast.expr): Unit =
    switchCaseStart(condition)

  override def switchCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"case ${expression(condition)}:")
    out.inc
  }

  override def switchCaseEnd(): Unit = {
    out.puts("break;")
    out.dec
  }

  override def switchElseStart(): Unit = {
    out.puts("default:")
    out.inc
  }

  override def switchEnd(): Unit =
    out.puts("}")

  //</editor-fold>

  //<editor-fold desc="switching: emulation with ifs">

  val NAME_SWITCH_ON = Ast.expr.Name(Ast.identifier(Identifier.SWITCH_ON))

  override def switchIfStart(id: Identifier, on: Ast.expr, onType: DataType): Unit = {
    out.puts("{")
    out.inc
    out.puts(s"var ${expression(NAME_SWITCH_ON)} = ${expression(on)};")
  }

  private def switchCmpExpr(condition: Ast.expr): String =
    expression(
      Ast.expr.Compare(
        NAME_SWITCH_ON,
        Ast.cmpop.Eq,
        condition
      )
    )

  override def switchIfCaseFirstStart(condition: Ast.expr): Unit = {
    out.puts(s"if (${switchCmpExpr(condition)}) {")
    out.inc
  }

  override def switchIfCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"else if (${switchCmpExpr(condition)}) {")
    out.inc
  }

  override def switchIfCaseEnd(): Unit = {
    out.dec
    out.puts("}")
  }

  override def switchIfElseStart(): Unit = {
    out.puts("else {")
    out.inc
  }

  override def switchIfEnd(): Unit = {
    out.dec
    out.puts("}")
  }

  //</editor-fold>

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"Object.defineProperty(${type2class(className.last)}.prototype, '${publicMemberName(instName)}', {")
    out.inc
    out.puts("get: function() {")
    out.inc
  }

  override def instanceFooter: Unit = {
    out.dec
    out.puts("}")
    out.dec
    out.puts("});")
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
    out.puts(s"${type2class(curClass.last)}.${type2class(enumName)} = Object.freeze({")
    out.inc

    // Name to ID mapping
    enumColl.foreach { case (id, label) =>
      out.puts(s"${enumValue(enumName, label.name)}: ${translator.doIntLiteral(id)},")
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

  def enumValue(enumName: String, label: String) = Utils.upperUnderscoreCase(label)

  override def debugClassSequence(seq: List[AttrSpec]) = {
    //val seqStr = seq.map((attr) => "\"" + idToStr(attr.id) + "\"").mkString(", ")
    //out.puts(s"SEQ_FIELDS = [$seqStr]")
  }

  override def classToString(toStringExpr: Ast.expr): Unit = {
    val className = type2class(translator.provider.nowClass.name.last)

    out.puts
    out.puts(s"${className}.prototype.toString = function() {")
    out.inc
    out.puts(s"return ${translator.translate(toStringExpr)};")
    out.dec
    out.puts("}")
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

  override def ksErrorName(err: KSError): String = JavaScriptCompiler.ksErrorName(err)

  override def attrValidateExpr(
    attrId: Identifier,
    attrType: DataType,
    checkExpr: Ast.expr,
    err: KSError,
    errArgs: List[Ast.expr]
  ): Unit = {
    val errArgsStr = errArgs.map(translator.translate).mkString(", ")
    out.puts(s"if (!(${translator.translate(checkExpr)})) {")
    out.inc
    out.puts(s"throw new ${ksErrorName(err)}($errArgsStr);")
    out.dec
    out.puts("}")
  }

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

object JavaScriptCompiler extends LanguageCompilerStatic
  with UpperCamelCaseClasses
  with StreamStructNames
  with ExceptionNames {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new JavaScriptCompiler(tp, config)

  override def kstreamName: String = "KaitaiStream"

  // FIXME: probably KaitaiStruct will emerge some day in JavaScript runtime, but for now it is unused
  override def kstructName: String = ???

  override def ksErrorName(err: KSError): String = err match {
    case EndOfStreamError => s"KaitaiStream.EOFError"
    case _ => s"KaitaiStream.${err.name}"
  }

  def types2class(types: List[String]): String = types.map(type2class).mkString(".")
}
