package io.kaitai.struct.languages

import io.kaitai.struct._
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.JavaTranslator

class JavaCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with SingleOutputFile
    with UpperCamelCaseClasses
    with ObjectOrientedLanguage
    with EveryReadIsExpression
    with UniversalFooter
    with UniversalDoc
    with AllocateIOLocalVar
    with FixedContentsUsingArrayByteLiteral
    with SwitchIfOps
    with NoNeedForFullClassPath {
  import JavaCompiler._

  val translator = new JavaTranslator(typeProvider, importList)

  // Preprocess fromFileClass and make import
  val fromFileClass = {
    val pos = config.java.fromFileClass.lastIndexOf('.')
    if (pos < 0) {
      // If relative "fromFileClass", then just use it as is
      config.java.fromFileClass
    } else {
      // If absolute "fromFileClass", add relevant import + use relative
      importList.add(config.java.fromFileClass)
      config.java.fromFileClass.substring(pos + 1)
    }
  }

  override def universalFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def indent: String = "    "
  override def outFileName(topClassName: String): String =
    s"${config.java.javaPackage.replace('.', '/')}/${type2class(topClassName)}.java"

  override def outImports(topClass: ClassSpec) =
    "\n" + importList.toList.map((x) => s"import $x;").mkString("\n") + "\n"

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"// $headerComment")
    if (!config.java.javaPackage.isEmpty) {
      outHeader.puts
      outHeader.puts(s"package ${config.java.javaPackage};")
    }

    // Used in every class
    importList.add(s"io.kaitai.struct.$kstructName")
    importList.add(s"io.kaitai.struct.$kstreamName")
    importList.add("java.io.IOException")

    out.puts
  }

  override def classHeader(name: String): Unit = {
    val staticStr = if (out.indentLevel > 0) {
      "static "
    } else {
      ""
    }

    out.puts(s"public ${staticStr}class ${type2class(name)} extends $kstructName {")
    out.inc

    if (config.readStoresPos) {
      out.puts("public Map<String, Integer> _attrStart = new HashMap<String, Integer>();")
      out.puts("public Map<String, Integer> _attrEnd = new HashMap<String, Integer>();")
      out.puts("public Map<String, ArrayList<Integer>> _arrStart = new HashMap<String, ArrayList<Integer>>();")
      out.puts("public Map<String, ArrayList<Integer>> _arrEnd = new HashMap<String, ArrayList<Integer>>();")
      out.puts

      importList.add("java.util.ArrayList")
      importList.add("java.util.HashMap")
      importList.add("java.util.Map")
    }

    val isInheritedEndian = typeProvider.nowClass.meta.endian match {
      case Some(InheritedEndian) => true
      case _ => false
    }

    // fromFile helper makes no sense for inherited endianness structures:
    // they require endianness to be parsed anyway
    if (!isInheritedEndian && !config.java.fromFileClass.isEmpty && typeProvider.nowClass.params.isEmpty) {
      out.puts(s"public static ${type2class(name)} fromFile(String fileName) throws IOException {")
      out.inc
      out.puts(s"return new ${type2class(name)}(new $fromFileClass(fileName));")
      out.dec
      out.puts("}")
    }
  }

  override def classConstructorHeader(name: String, parentType: DataType, rootClassName: String, isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    typeProvider.nowClass.meta.endian match {
      case Some(_: CalcEndian) | Some(InheritedEndian) =>
        out.puts("private Boolean _is_le;")
      case _ =>
        // no _is_le variable
    }

    val paramsArg = Utils.join(params.map((p) =>
      s"${kaitaiType2JavaType(p.dataType)} ${paramName(p.id)}"
    ), ", ", ", ", "")

    if (isHybrid) {
      // Inherited endian classes can be only internal, so they have mandatory 4th argument
      // and 1..3-argument constructors don't make sense

      out.puts
      out.puts(s"public ${type2class(name)}($kstreamName _io, ${kaitaiType2JavaType(parentType)} _parent, ${type2class(rootClassName)} _root, boolean _is_le$paramsArg) {")
      out.inc
      out.puts("super(_io);")
      out.puts("this._parent = _parent;")
      out.puts("this._root = _root;")
      out.puts("this._is_le = _is_le;")
    } else {
      // Normal 3 constructors, chained into the last

      val paramsRelay = Utils.join(params.map((p) => paramName(p.id)), ", ", ", ", "")

      out.puts
      out.puts(s"public ${type2class(name)}($kstreamName _io$paramsArg) {")
      out.inc
      out.puts(s"this(_io, null, null$paramsRelay);")
      out.dec
      out.puts("}")

      out.puts
      out.puts(s"public ${type2class(name)}($kstreamName _io, ${kaitaiType2JavaType(parentType)} _parent$paramsArg) {")
      out.inc
      out.puts(s"this(_io, _parent, null$paramsRelay);")
      out.dec
      out.puts("}")

      out.puts
      out.puts(s"public ${type2class(name)}($kstreamName _io, ${kaitaiType2JavaType(parentType)} _parent, ${type2class(rootClassName)} _root$paramsArg) {")
      out.inc
      out.puts("super(_io);")
      out.puts("this._parent = _parent;")
      if (name == rootClassName) {
        out.puts("this._root = _root == null ? this : _root;")
      } else {
        out.puts("this._root = _root;")
      }
    }

    // Store parameters passed to us
    params.foreach((p) => handleAssignmentSimple(p.id, paramName(p.id)))
  }

  override def runRead(name: List[String]): Unit =
    out.puts("_read();")

  override def runReadCalc(): Unit = {
    out.puts
    out.puts("if (_is_le == null) {")
    out.inc
    out.puts(s"throw new $kstreamName.UndecidedEndiannessError();")
    out.dec
    out.puts("} else if (_is_le) {")
    out.inc
    out.puts("_readLE();")
    out.dec
    out.puts("} else {")
    out.inc
    out.puts("_readBE();")
    out.dec
    out.puts("}")
  }

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean) = {
    val readAccessAndType = if (!config.autoRead) {
      "public"
    } else {
      "private"
    }
    val suffix = endian match {
      case Some(e) => Utils.upperUnderscoreCase(e.toSuffix)
      case None => ""
    }
    out.puts(s"$readAccessAndType void _read$suffix() {")
    out.inc
  }

  override def readFooter(): Unit = universalFooter

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"private ${kaitaiType2JavaType(attrType, isNullable)} ${idToStr(attrName)};")
  }

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"public ${kaitaiType2JavaType(attrType, isNullable)} ${idToStr(attrName)}() { return ${idToStr(attrName)}; }")
  }

  override def universalDoc(doc: DocSpec): Unit = {
    out.puts
    out.puts( "/**")

    doc.summary.foreach(summary => out.putsLines(" * ", summary))

    doc.ref.foreach {
      case TextRef(text) =>
        out.putsLines(" * ", "@see \"" + text + "\"")
      case ref: UrlRef =>
        out.putsLines(" * ", s"@see ${ref.toAhref}")
    }

    out.puts( " */")
  }

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {
    out.puts("if (_is_le) {")
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
    out.puts(s"${privateMemberName(attrName)} = $normalIO.ensureFixedContents($contents);")
  }

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec): Unit = {
    val srcExpr = getRawIdExpr(varSrc, rep)

    val expr = proc match {
      case ProcessXor(xorValue) =>
        val xorValueStr = translator.detectType(xorValue) match {
          case _: IntType => translator.doCast(xorValue, Int1Type(true))
          case _ => expression(xorValue)
        }
        s"$kstreamName.processXor($srcExpr, $xorValueStr)"
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
        val namespace = name.init.mkString(".")
        val procClass = namespace +
          (if (namespace.nonEmpty) "." else "") +
          type2class(name.last)
        val procName = s"_process_${idToStr(varSrc)}"
        out.puts(s"$procClass $procName = new $procClass(${args.map(expression).mkString(", ")});")
        s"$procName.decode($srcExpr)"
    }
    handleAssignment(varDest, expr, rep, false)
  }

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = {
    val ioName = idToStr(IoStorageIdentifier(varName))

    val args = rep match {
      case RepeatUntil(_) => translator.doName(Identifier.ITERATOR2)
      case _ => getRawIdExpr(varName, rep)
    }

    importList.add("io.kaitai.struct.ByteBufferKaitaiStream")
    out.puts(s"$kstreamName $ioName = new ByteBufferKaitaiStream($args);")
    ioName
  }

  def getRawIdExpr(varName: Identifier, rep: RepeatSpec): String = {
    val memberName = idToStr(varName)
    rep match {
      case NoRepeat => memberName
      case _ => s"$memberName.get($memberName.size() - 1)"
    }
  }

  override def useIO(ioEx: expr): String = {
    out.puts(s"$kstreamName io = ${expression(ioEx)};")
    "io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"long _pos = $io.pos();")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io.seek(${expression(pos)});")

  override def popPos(io: String): Unit =
    out.puts(s"$io.seek(_pos);")

  override def alignToByte(io: String): Unit =
    out.puts(s"$io.alignToByte();")

  override def attrDebugStart(attrId: Identifier, attrType: DataType, ios: Option[String], rep: RepeatSpec): Unit = {
    ios.foreach { (io) =>
      val name = idToStr(attrId)
      rep match {
        case NoRepeat =>
          out.puts("_attrStart.put(\"" + name + "\", " + io + ".pos());")
        case _: RepeatExpr | RepeatEos | _: RepeatUntil =>
          getOrCreatePosList("_arrStart", name, io)
      }
    }
  }

  override def attrDebugArrInit(attrName: Identifier, attrType: DataType): Unit = {
    // no _debug[$name]['arr'] initialization needed in Java
  }

  override def attrDebugEnd(attrId: Identifier, attrType: DataType, io: String, rep: RepeatSpec): Unit = {
    val name = idToStr(attrId)
    rep match {
      case NoRepeat =>
        out.puts("_attrEnd.put(\"" + name + "\", " + io + ".pos());")
      case _: RepeatExpr | RepeatEos | _: RepeatUntil =>
        getOrCreatePosList("_arrEnd", name, io)
    }
  }

  def getOrCreatePosList(listName: String, varName: String, io: String): Unit = {
    out.puts("{")
    out.inc
    out.puts("ArrayList<Integer> _posList = " + listName + ".get(\"" + varName + "\");")
    out.puts("if (_posList == null) {")
    out.inc
    out.puts("_posList = new ArrayList<Integer>();")
    out.puts(listName + ".put(\"" + varName + "\", _posList);")
    out.dec
    out.puts("}")
    out.puts(s"_posList.add($io.pos());")
    out.dec
    out.puts("}")
  }

  override def condIfHeader(expr: expr): Unit = {
    out.puts(s"if (${expression(expr)}) {")
    out.inc
  }

  override def condRepeatInitAttr(id: Identifier, dataType: DataType): Unit =
    out.puts(s"${privateMemberName(id)} = new ${kaitaiType2JavaType(ArrayTypeInStream(dataType))}();")

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType): Unit = {
    out.puts("{")
    out.inc
    out.puts("int i = 0;")
    out.puts(s"while (!$io.isEof()) {")
    out.inc

    importList.add("java.util.ArrayList")
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}.add($expr);")
  }

  override def condRepeatEosFooter: Unit = {
    out.puts("i++;")
    out.dec
    out.puts("}")
    out.dec
    out.puts("}")
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, repeatExpr: expr): Unit = {
    out.puts(s"for (int i = 0; i < ${expression(repeatExpr)}; i++) {")
    out.inc

    importList.add("java.util.ArrayList")
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit =
    handleAssignmentRepeatEos(id, expr)

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, untilExpr: expr): Unit = {
    out.puts("{")
    out.inc
    out.puts(s"${kaitaiType2JavaType(dataType)} ${translator.doName("_")};")
    out.puts("int i = 0;")
    out.puts("do {")
    out.inc

    importList.add("java.util.ArrayList")
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val (typeDecl, tempVar) = if (isRaw) {
      ("byte[] ", translator.doName(Identifier.ITERATOR2))
    } else {
      ("", translator.doName(Identifier.ITERATOR))
    }
    out.puts(s"$typeDecl$tempVar = $expr;")
    out.puts(s"${privateMemberName(id)}.add($tempVar);")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, untilExpr: expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.puts("i++;")
    out.dec
    out.puts(s"} while (!(${expression(untilExpr)}));")
    out.dec
    out.puts("}")
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)} = $expr;")

  override def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit =
    out.puts(s"${kaitaiType2JavaType(dataType)} $id = $expr;")

  override def blockScopeHeader: Unit = {
    out.puts("{")
    out.inc
  }
  override def blockScopeFooter: Unit = universalFooter

  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    val expr = dataType match {
      case t: ReadableType =>
        s"$io.read${Utils.capitalize(t.apiCall(defEndian))}()"
      case blt: BytesLimitType =>
        s"$io.readBytes(${expression(blt.size)})"
      case _: BytesEosType =>
        s"$io.readBytesFull()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        if (terminator.length == 1) {
          val term = terminator.head & 0xff
          s"$io.readBytesTerm((byte) $term, $include, $consume, $eosError)"
        } else {
          s"$io.readBytesTermMulti(${translator.doByteArrayLiteral(terminator)}, $include, $consume, $eosError)"
        }
      case BitsType1(bitEndian) =>
        s"$io.readBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}(1) != 0"
      case BitsType(width: Int, bitEndian) =>
        s"$io.readBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}($width)"
      case t: UserType =>
        val addArgs = if (t.isExternal(typeProvider.nowClass)) {
          ""
        } else {
          val parent = t.forcedParent match {
            case Some(USER_TYPE_NO_PARENT) => "null"
            case Some(fp) => translator.translate(fp)
            case None => "this"
          }
          val addEndian = t.classSpec.get.meta.endian match {
            case Some(InheritedEndian) => ", _is_le"
            case _ => ""
          }
          s", $parent, _root$addEndian"
        }
        val addParams = Utils.join(t.args.map((a) => translator.translate(a)), ", ", ", ", "")
        s"new ${types2class(t.name)}($io$addArgs$addParams)"
    }

    if (assignType != dataType) {
      s"(${kaitaiType2JavaType(assignType)}) ($expr)"
    } else {
      expr
    }
  }

  override def createSubstreamFixedSize(id: Identifier, blt: BytesLimitType, io: String, rep: RepeatSpec, defEndian: Option[FixedEndian]): String = {
    val ioName = idToStr(IoStorageIdentifier(id))
    handleAssignmentTempVar(KaitaiStreamType, ioName, s"$io.substream(${translator.translate(blt.size)})")
    ioName
  }

  override def extraRawAttrForUserTypeFromBytes(id: Identifier, ut: UserTypeFromBytes, condSpec: ConditionalSpec): List[AttrSpec] = {
    if (config.zeroCopySubstream) {
      ut.bytes match {
        case BytesLimitType(sizeExpr, None, _, None, None) =>
          // substream will be used, no need for store raws
          List()
        case _ =>
          // buffered implementation will be used, fall back to raw storage
          super.extraRawAttrForUserTypeFromBytes(id, ut, condSpec)
      }
    } else {
      // zero-copy streams disabled, fall back to raw storage
      super.extraRawAttrForUserTypeFromBytes(id, ut, condSpec)
    }
  }

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Seq[Byte]], include: Boolean) = {
    val expr1 = padRight match {
      case Some(padByte) => s"$kstreamName.bytesStripRight($expr0, (byte) $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) =>
        if (term.length == 1) {
          val t = term.head & 0xff
          s"$kstreamName.bytesTerminate($expr1, (byte) $t, $include)"
        } else {
          s"$kstreamName.bytesTerminateMulti($expr1, ${translator.doByteArrayLiteral(term)}, $include)"
        }
      case None => expr1
    }
    expr2
  }

  override def userTypeDebugRead(id: String, dataType: DataType, assignType: DataType): Unit = {
    val expr = if (assignType != dataType) {
      s"((${kaitaiType2JavaType(dataType)}) ($id))"
    } else {
      id
    }
    out.puts(s"$expr._read();")
  }

  override def tryFinally(tryBlock: () => Unit, finallyBlock: () => Unit): Unit = {
    out.puts("try {")
    out.inc
    tryBlock()
    out.dec
    out.puts("} finally {")
    out.inc
    finallyBlock()
    out.dec
    out.puts("}")
  }

  override def switchCasesRender[T](
    id: Identifier,
    on: Ast.expr,
    cases: Map[Ast.expr, T],
    normalCaseProc: T => Unit,
    elseCaseProc: T => Unit
  ): Unit = {
    // Java has a stupid limitation of being unable to match nulls in switch.
    // If our type is nullable, we'll do an extra check. For now, we're only
    // doing this workaround for enums.

    val onType = typeProvider._currentSwitchType.get
    val isNullable = onType match {
      case _: EnumType => true
      case _ => false
    }

    if (isNullable) {
      val nameSwitchStr = expression(NAME_SWITCH_ON)
      out.puts("{")
      out.inc
      out.puts(s"${kaitaiType2JavaType(onType)} $nameSwitchStr = ${expression(on)};")
      out.puts(s"if ($nameSwitchStr != null) {")
      out.inc

      super.switchCasesRender(id, on, cases, normalCaseProc, elseCaseProc)

      out.dec
      cases.get(SwitchType.ELSE_CONST) match {
        case Some(result) =>
          out.puts("} else {")
          out.inc
          elseCaseProc(result)
          out.dec
          out.puts("}")
        case None =>
          out.puts("}")
      }

      out.dec
      out.puts("}")
    } else {
      super.switchCasesRender(id, on, cases, normalCaseProc, elseCaseProc)
    }
  }

  override def switchRequiresIfs(onType: DataType): Boolean = onType match {
    case _: IntType | _: EnumType | _: StrType => false
    case _ => true
  }

  //<editor-fold desc="switching: true version">

  val NAME_SWITCH_ON = Ast.expr.Name(Ast.identifier(Identifier.SWITCH_ON))

  override def switchStart(id: Identifier, on: Ast.expr): Unit =
    out.puts(s"switch (${expression(on)}) {")

  override def switchCaseFirstStart(condition: Ast.expr): Unit = switchCaseStart(condition)

  override def switchCaseStart(condition: Ast.expr): Unit = {
    // Java is very specific about what can be used as "condition" in "case
    // condition:".
    val condStr = condition match {
      case enumByLabel: Ast.expr.EnumByLabel =>
        // If switch is over a enum, only literal enum values are supported,
        // and they must be written as "MEMBER", not "SomeEnum.MEMBER".
        value2Const(enumByLabel.label.name)
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

  override def switchEnd(): Unit = {
    out.puts("}")
  }

  //</editor-fold>

  //<editor-fold desc="switching: emulation with ifs">

  override def switchIfStart(id: Identifier, on: expr, onType: DataType): Unit = {
    out.puts("{")
    out.inc
    out.puts(s"${kaitaiType2JavaType(onType)} ${expression(NAME_SWITCH_ON)} = ${expression(on)};")
  }

  def switchCmpExpr(condition: Ast.expr): String =
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

  override def instanceDeclaration(attrName: InstanceIdentifier, attrType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"private ${kaitaiType2JavaTypeBoxed(attrType)} ${idToStr(attrName)};")
  }

  override def instanceHeader(className: String, instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"public ${kaitaiType2JavaTypeBoxed(dataType)} ${idToStr(instName)}() {")
    out.inc
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    out.puts(s"if (${privateMemberName(instName)} != null)")
    out.inc
    instanceReturn(instName, dataType)
    out.dec
  }

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit = {
    out.puts(s"return ${privateMemberName(instName)};")
  }

  override def instanceCalculate(instName: Identifier, dataType: DataType, value: expr): Unit = {
    val primType = kaitaiType2JavaTypePrim(dataType)
    val boxedType = kaitaiType2JavaTypeBoxed(dataType)

    if (primType != boxedType) {
      // Special trick to achieve both implicit type conversion + boxing.
      // Unfortunately, Java can't do both in one assignment, i.e. this would fail:
      //
      // Double c = 1.0f + 1;

      out.puts(s"$primType _tmp = ($primType) (${expression(value)});")
      out.puts(s"${privateMemberName(instName)} = _tmp;")
    } else {
      out.puts(s"${privateMemberName(instName)} = ${expression(value)};")
    }
  }

  override def enumDeclaration(curClass: String, enumName: String, enumColl: Seq[(Long, String)]): Unit = {
    val enumClass = type2class(enumName)

    out.puts
    out.puts(s"public enum $enumClass {")
    out.inc

    if (enumColl.size > 1) {
      enumColl.dropRight(1).foreach { case (id, label) =>
        out.puts(s"${value2Const(label)}(${translator.doIntLiteral(id)}),")
      }
    }
    enumColl.last match {
      case (id, label) =>
        out.puts(s"${value2Const(label)}(${translator.doIntLiteral(id)});")
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

    importList.add("java.util.Map")
    importList.add("java.util.HashMap")
  }

  override def debugClassSequence(seq: List[AttrSpec]) = {
    val seqStr = seq.map((attr) => "\"" + idToStr(attr.id) + "\"").mkString(", ")
    out.puts(s"public static String[] _seqFields = new String[] { $seqStr };")
  }

  override def classToString(toStringExpr: Ast.expr): Unit = {
    out.puts
    out.puts("@Override")
    out.puts("public String toString() {")
    out.inc
    out.puts(s"return ${translator.translate(toStringExpr)};")
    out.dec
    out.puts("}")
  }

  def value2Const(s: String) = Utils.upperUnderscoreCase(s)

  override def idToStr(id: Identifier): String = JavaCompiler.idToStr(id)

  override def publicMemberName(id: Identifier): String = idToStr(id)

  override def privateMemberName(id: Identifier): String = JavaCompiler.privateMemberName(id)

  override def localTemporaryName(id: Identifier): String = s"_t_${idToStr(id)}"

  def kaitaiType2JavaType(attrType: DataType): String =
    JavaCompiler.kaitaiType2JavaType(attrType, importList)

  def kaitaiType2JavaType(attrType: DataType, isNullable: Boolean): String =
    JavaCompiler.kaitaiType2JavaType(attrType, isNullable, importList)

  def kaitaiType2JavaTypePrim(attrType: DataType): String =
    JavaCompiler.kaitaiType2JavaTypePrim(attrType, importList)

  def kaitaiType2JavaTypeBoxed(attrType: DataType): String =
    JavaCompiler.kaitaiType2JavaTypeBoxed(attrType, importList)

  override def ksErrorName(err: KSError): String = err match {
    case EndOfStreamError => config.java.endOfStreamErrorClass
    case ConversionError => "NumberFormatException"
    case _ => s"KaitaiStream.${err.name}"
  }

  override def attrValidateExpr(
    attr: AttrLikeSpec,
    checkExpr: Ast.expr,
    err: KSError,
    errArgs: List[Ast.expr]
  ): Unit =
    attrValidate(s"!(${translator.translate(checkExpr)})", err, errArgs)

  override def attrValidateInEnum(
    attr: AttrLikeSpec,
    et: EnumType,
    valueExpr: Ast.expr,
    err: ValidationNotInEnumError,
    errArgs: List[Ast.expr]
  ): Unit = {
    // NOTE: this condition works for now because we haven't implemented
    // https://github.com/kaitai-io/kaitai_struct/issues/778 for Java yet, but
    // it will need to be changed when we do.
    attrValidate(s"${translator.translate(valueExpr)} == null", err, errArgs)
  }

  private def attrValidate(failCondExpr: String, err: KSError, errArgs: List[Ast.expr]): Unit = {
    val errArgsStr = errArgs.map(translator.translate).mkString(", ")
    out.puts(s"if ($failCondExpr) {")
    out.inc
    out.puts(s"throw new ${ksErrorName(err)}($errArgsStr);")
    out.dec
    out.puts("}")
  }
}

object JavaCompiler extends LanguageCompilerStatic
  with UpperCamelCaseClasses
  with StreamStructNames {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new JavaCompiler(tp, config)

  def idToStr(id: Identifier): String =
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => Utils.lowerCamelCase(name)
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => Utils.lowerCamelCase(name)
      case RawIdentifier(innerId) => s"_raw_${idToStr(innerId)}"
      case IoStorageIdentifier(innerId) => s"_io_${idToStr(innerId)}"
    }

  def privateMemberName(id: Identifier): String = s"this.${idToStr(id)}"

  def kaitaiType2JavaType(attrType: DataType, importList: ImportList): String = kaitaiType2JavaTypePrim(attrType, importList)

  def kaitaiType2JavaType(attrType: DataType, isNullable: Boolean, importList: ImportList): String =
    if (isNullable) {
      kaitaiType2JavaTypeBoxed(attrType, importList)
    } else {
      kaitaiType2JavaTypePrim(attrType, importList)
    }

  /**
    * Determine Java data type corresponding to a KS data type. A "primitive" type (i.e. "int", "long", etc) will
    * be returned if possible.
    *
    * @param attrType KS data type
    * @return Java data type
    */
  def kaitaiType2JavaTypePrim(attrType: DataType, importList: ImportList): String = {
    attrType match {
      case Int1Type(false) => "int"
      case IntMultiType(false, Width2, _) => "int"
      case IntMultiType(false, Width4, _) => "long"
      case IntMultiType(false, Width8, _) => "long"

      case Int1Type(true) => "byte"
      case IntMultiType(true, Width2, _) => "short"
      case IntMultiType(true, Width4, _) => "int"
      case IntMultiType(true, Width8, _) => "long"

      case FloatMultiType(Width4, _) => "float"
      case FloatMultiType(Width8, _) => "double"

      case BitsType(_, _) => "long"

      case _: BooleanType => "boolean"
      case CalcIntType => "int"
      case CalcFloatType => "double"

      case _: StrType => "String"
      case _: BytesType => "byte[]"

      case AnyType => "Object"
      case KaitaiStreamType | OwnedKaitaiStreamType => kstreamName
      case KaitaiStructType | CalcKaitaiStructType(_) => kstructName

      case t: UserType => types2class(t.name)
      case EnumType(name, _) => types2class(name)

      case _: ArrayType => kaitaiType2JavaTypeBoxed(attrType, importList)

      case st: SwitchType => kaitaiType2JavaTypePrim(st.combinedType, importList)
    }
  }

  /**
    * Determine Java data type corresponding to a KS data type. A non-primitive type (i.e. "Integer", "Long", etc) will
    * be returned, to be used when proper objects should be used.
    *
    * @param attrType KS data type
    * @return Java data type
    */
  def kaitaiType2JavaTypeBoxed(attrType: DataType, importList: ImportList): String = {
    attrType match {
      case Int1Type(false) => "Integer"
      case IntMultiType(false, Width2, _) => "Integer"
      case IntMultiType(false, Width4, _) => "Long"
      case IntMultiType(false, Width8, _) => "Long"

      case Int1Type(true) => "Byte"
      case IntMultiType(true, Width2, _) => "Short"
      case IntMultiType(true, Width4, _) => "Integer"
      case IntMultiType(true, Width8, _) => "Long"

      case FloatMultiType(Width4, _) => "Float"
      case FloatMultiType(Width8, _) => "Double"

      case BitsType(_, _) => "Long"

      case _: BooleanType => "Boolean"
      case CalcIntType => "Integer"
      case CalcFloatType => "Double"

      case _: StrType => "String"
      case _: BytesType => "byte[]"

      case AnyType => "Object"
      case KaitaiStreamType | OwnedKaitaiStreamType => kstreamName
      case KaitaiStructType | CalcKaitaiStructType(_) => kstructName

      case t: UserType => types2class(t.name)
      case EnumType(name, _) => types2class(name)

      case at: ArrayType => {
        importList.add("java.util.ArrayList")
        s"ArrayList<${kaitaiType2JavaTypeBoxed(at.elType, importList)}>"
      }

      case st: SwitchType => kaitaiType2JavaTypeBoxed(st.combinedType, importList)
    }
  }

  def types2class(names: List[String]) = names.map(x => type2class(x)).mkString(".")

  override def kstreamName: String = "KaitaiStream"
  override def kstructName: String = "KaitaiStruct"
}
