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
    with FetchInstances
    with EveryWriteIsExpression
    with GenericChecks
    with UniversalFooter
    with UniversalDoc
    with AllocateIOLocalVar
    with SwitchIfOps
    with NoNeedForFullClassPath {
  import JavaCompiler._

  val translator = new JavaTranslator(typeProvider, importList, config)

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

    out.puts(
      s"public ${staticStr}class ${type2class(name)} " +
      s"extends ${JavaCompiler.kstructNameFull(config)} {"
    )
    out.inc

    if (config.readStoresPos) {
      out.puts("public Map<String, Integer> _attrStart = new HashMap<String, Integer>();")
      out.puts("public Map<String, Integer> _attrEnd = new HashMap<String, Integer>();")
      out.puts("public Map<String, List<Integer>> _arrStart = new HashMap<String, List<Integer>>();")
      out.puts("public Map<String, List<Integer>> _arrEnd = new HashMap<String, List<Integer>>();")
      out.puts

      importList.add("java.util.HashMap")
      importList.add("java.util.List")
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

      if (config.readWrite) {
        out.puts(s"public ${type2class(name)}(${paramsArg.stripPrefix(", ")}) {")
        out.inc
        out.puts(s"this(null, null, null$paramsRelay);")
        out.dec
        out.puts("}")
      }

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

  override def runWriteCalc(): Unit = {
    out.puts
    out.puts("if (_is_le == null) {")
    out.inc
    out.puts(s"throw new $kstreamName.UndecidedEndiannessError();")
    out.dec
    out.puts("} else if (_is_le) {")
    out.inc
    out.puts("_write_SeqLE();")
    out.dec
    out.puts("} else {")
    out.inc
    out.puts("_write_SeqBE();")
    out.dec
    out.puts("}")
  }

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean) = {
    endian match {
      case Some(e) =>
        out.puts(s"private void _read${Utils.upperUnderscoreCase(e.toSuffix)}() {")
      case None =>
        out.puts(s"${if (!config.autoRead) "public" else "private"} void _read() {")
    }
    out.inc
  }

  override def readFooter(): Unit = {
    if (config.readWrite) {
      out.puts("_dirty = false;")
    }
    universalFooter
  }

  override def fetchInstancesHeader(): Unit = {
    out.puts
    out.puts("public void _fetchInstances() {")
    out.inc
  }

  override def fetchInstancesFooter: Unit = universalFooter

  override def attrInvokeFetchInstances(baseExpr: Ast.expr, exprType: DataType, dataType: DataType): Unit = {
    val expr = castIfNeeded(expression(baseExpr), exprType, dataType)
    out.puts(s"$expr._fetchInstances();")
  }

  override def attrInvokeInstance(instName: InstanceIdentifier): Unit = {
    out.puts(s"${publicMemberName(instName)}();")
  }

  override def writeHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = {
    out.puts
    endian match {
      case Some(e) =>
        out.puts(s"private void _write_Seq${Utils.upperUnderscoreCase(e.toSuffix)}() {")
        out.inc
      case None =>
        out.puts("public void _write_Seq() {")
        out.inc
        out.puts("_assertNotDirty();")
    }
  }

  override def checkHeader(): Unit = {
    out.puts
    out.puts("public void _check() {")
    out.inc
  }

  override def checkFooter(): Unit = {
    out.puts("_dirty = false;")
    universalFooter
  }

  override def writeInstanceHeader(instName: InstanceIdentifier): Unit = {
    out.puts
    out.puts(s"private void _write${idToSetterStr(instName)}() {")
    out.inc
    instanceClearWriteFlag(instName)
  }

  override def checkInstanceHeader(instName: InstanceIdentifier): Unit = {
    out.puts(s"if (_enabled${idToSetterStr(instName)}) {")
    out.inc
  }

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"private ${kaitaiType2JavaType(attrType, isNullable)} ${idToStr(attrName)};")
  }

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    val javaType = kaitaiType2JavaType(attrType, isNullable)
    val name = idToStr(attrName)

    out.puts(s"public $javaType $name() { return $name; }")
  }

  override def attributeSetter(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    val javaType = kaitaiType2JavaType(attrType, isNullable)
    val name = idToStr(attrName)
    val itemType = getArrayItemType(attrType)
    val typeBasedOnIo = itemType == KaitaiStreamType || itemType == OwnedKaitaiStreamType
    val setDirtyBit = if (typeBasedOnIo) "" else "_dirty = true; "

    out.puts(s"public void set${idToSetterStr(attrName)}($javaType _v) { $setDirtyBit$name = _v; }")
  }

  override def attrSetProperty(base: Ast.expr, propName: Identifier, value: String): Unit = {
    out.puts(s"${expression(base)}.set${idToSetterStr(propName)}($value);")
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

  override def attrUnprocess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec, dataType: BytesType, exprTypeOpt: Option[DataType]): Unit = {
    val exprType = exprTypeOpt.getOrElse(dataType)
    val srcExprRaw = expression(Identifier.itemExpr(varSrc, rep))
    val srcExpr = castIfNeeded(srcExprRaw, exprType, dataType)

    val expr = proc match {
      case ProcessXor(xorValue) =>
        val argStr = if (translator.inSubIOWriteBackHandler) "_processXorArg" else expression(xorValue)
        val xorValueStr = translator.detectType(xorValue) match {
          case _: IntType => castIfNeeded(argStr, AnyType, Int1Type(true))
          case _ => argStr
        }
        s"$kstreamName.processXor($srcExpr, $xorValueStr)"
      case ProcessZlib =>
        s"$kstreamName.unprocessZlib($srcExpr)"
      case ProcessRotate(isLeft, rotValue) =>
        val argStr = if (translator.inSubIOWriteBackHandler) "_processRotateArg" else expression(rotValue)
        val expr = if (!isLeft) {
          argStr
        } else {
          s"8 - ($argStr)"
        }
        s"$kstreamName.processRotateLeft($srcExpr, $expr, 1)"
      case ProcessCustom(name, args) =>
        val namespace = name.init.mkString(".")
        val procClass = namespace +
          (if (namespace.nonEmpty) "." else "") +
          type2class(name.last)
        val procName = s"_process_${idToStr(varSrc)}"
        if (!translator.inSubIOWriteBackHandler) {
          out.puts(s"$procClass $procName = new $procClass(${args.map(expression).mkString(", ")});")
        }
        s"$procName.encode($srcExpr)"
    }
    handleAssignment(varDest, expr, rep, false)
  }

  override def attrUnprocessPrepareBeforeSubIOHandler(proc: ProcessExpr, varSrc: Identifier): Unit = {
    proc match {
      case ProcessXor(xorValue) =>
        val dataType = translator.detectType(xorValue)
        out.puts(s"final ${kaitaiType2JavaType(dataType)} _processXorArg = ${expression(xorValue)};")
      case ProcessRotate(_, rotValue) =>
        val dataType = translator.detectType(rotValue)
        out.puts(s"final ${kaitaiType2JavaType(dataType)} _processRotateArg = ${expression(rotValue)};")
      case ProcessZlib => // no process arguments
      case ProcessCustom(name, args) =>
        val namespace = name.init.mkString(".")
        val procClass = namespace +
          (if (namespace.nonEmpty) "." else "") +
          type2class(name.last)
        val procName = s"_process_${idToStr(varSrc)}"
        out.puts(s"final $procClass $procName = new $procClass(${args.map(expression).mkString(", ")});")
    }
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

  override def allocateIOFixed(varName: Identifier, size: String): String = {
    val ioName = idToStr(IoStorageIdentifier(varName))

    out.puts(s"final $kstreamName $ioName = new ByteBufferKaitaiStream($size);")
    ioName
  }

  override def exprIORemainingSize(io: String): String =
    s"$io.size() - $io.pos()"

  override def allocateIOGrowing(varName: Identifier): String =
    allocateIOFixed(varName, "100000") // FIXME to use real growing buffer

  override def subIOWriteBackHeader(subIO: String, rep: RepeatSpec, process: Option[ProcessExpr]): String = {
    val parentIoName = "parent"
    out.puts(s"final ${type2class(typeProvider.nowClass.name.last)} _this = this;")
    rep match {
      case NoRepeat =>
        // do nothing
      case _ =>
        out.puts("final int _i = i;")
    }
    out.puts(s"$subIO.setWriteBackHandler(new $kstreamName.WriteBackHandler(_pos2) {")
    out.inc
    out.puts("@Override")
    out.puts(s"protected void write($kstreamName $parentIoName) {")
    out.inc

    translator.inSubIOWriteBackHandler = true

    parentIoName
  }

  override def subIOWriteBackFooter(subIO: String): Unit = {
    translator.inSubIOWriteBackHandler = false

    out.dec
    out.puts("}")
    out.dec
    out.puts("});")
  }

  override def addChildIO(io: String, childIO: String): Unit =
    out.puts(s"$io.addChildStream($childIO);")

  def getRawIdExpr(varName: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(varName)
    rep match {
      case NoRepeat => memberName
      case RepeatExpr(_) => s"$memberName.get(i)"
      case _ => s"$memberName.get($memberName.size() - 1)"
    }
  }

  override def useIO(ioEx: expr): String = {
    out.puts(s"$kstreamName io = ${expression(ioEx)};")
    "io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"long _pos = $io.pos();")

  override def pushPosForSubIOWriteBackHandler(io: String): Unit =
    out.puts(s"long _pos2 = $io.pos();")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io.seek(${expression(pos)});")

  override def seekRelative(io: String, relPos: String): Unit =
    out.puts(s"$io.seek($io.pos() + ($relPos));")

  override def popPos(io: String): Unit =
    out.puts(s"$io.seek(_pos);")

  // NOTE: the compiler does not need to output alignToByte() calls for Java anymore,
  // since the byte alignment is handled by the runtime library since commit
  // https://github.com/kaitai-io/kaitai_struct_java_runtime/commit/1bc75aa91199588a1cb12a5a1c672b80b66619ac
  override def alignToByte(io: String): Unit = {}

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
    importList.add("java.util.List")
    importList.add("java.util.ArrayList")
    out.puts("{")
    out.inc
    out.puts("List<Integer> _posList = " + listName + ".get(\"" + varName + "\");")
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

  override def condRepeatInitAttr(id: Identifier, dataType: DataType): Unit = {
    importList.add("java.util.ArrayList")
    out.puts(s"${privateMemberName(id)} = new ArrayList<${kaitaiType2JavaTypeBoxed(dataType)}>();")
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType): Unit = {
    out.puts("{")
    out.inc
    out.puts("int i = 0;")
    out.puts(s"while (!$io.isEof()) {")
    out.inc
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
  }

  // used for all repetitions in _check()
  override def condRepeatCommonHeader(id: Identifier, io: String, dataType: DataType): Unit = {
    out.puts(s"for (int i = 0; i < ${privateMemberName(id)}.size(); i++) {")
    out.inc
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit =
    handleAssignmentRepeatEos(id, expr)

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, untilExpr: expr): Unit = {
    out.puts("{")
    out.inc
    out.puts(s"${kaitaiType2JavaType(dataType)} ${translator.doName(Identifier.ITERATOR)};")
    out.puts("int i = 0;")
    out.puts("do {")
    out.inc
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

    castIfNeeded(expr, dataType, assignType)
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
      case Some(padByte) if terminator.map(term => padByte != (term.last & 0xff)).getOrElse(true) =>
        s"$kstreamName.bytesStripRight($expr0, (byte) $padByte)"
      case _ => expr0
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
    val expr = castIfNeeded(id, assignType, dataType)
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

  override def instanceWriteFlagDeclaration(attrName: InstanceIdentifier): Unit = {
    out.puts(s"private boolean _shouldWrite${idToSetterStr(attrName)} = false;")
    out.puts(s"private boolean _enabled${idToSetterStr(attrName)} = true;")
  }

  override def instanceSetWriteFlag(instName: InstanceIdentifier): Unit = {
    out.puts(s"_shouldWrite${idToSetterStr(instName)} = _enabled${idToSetterStr(instName)};")
  }

  override def instanceClearWriteFlag(instName: InstanceIdentifier): Unit = {
    out.puts(s"_shouldWrite${idToSetterStr(instName)} = false;")
  }

  override def instanceEnabledSetter(instName: InstanceIdentifier): Unit = {
    out.puts(s"public void set${idToSetterStr(instName)}_Enabled(boolean _v) { _dirty = true; _enabled${idToSetterStr(instName)} = _v; }")
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

  override def instanceCheckWriteFlagAndWrite(instName: InstanceIdentifier): Unit = {
    out.puts(s"if (_shouldWrite${idToSetterStr(instName)})")
    out.inc
    out.puts(s"_write${idToSetterStr(instName)}();")
    out.dec
  }

  override def instanceReturnNullIfDisabled(instName: InstanceIdentifier): Unit = {
    out.puts(s"if (!_enabled${idToSetterStr(instName)})")
    out.inc
    out.puts("return null;")
    out.dec
  }

  override def instanceHasValueIfHeader(instName: InstanceIdentifier): Unit = {
    out.puts(s"if (${privateMemberName(instName)} != null) {")
    out.inc
  }

  override def instanceHasValueIfFooter(): Unit =
    condIfFooter

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit = {
    out.puts(s"return ${privateMemberName(instName)};")
  }

  override def instanceCalculate(instName: Identifier, dataType: DataType, value: expr): Unit = {
    val primType = kaitaiType2JavaTypePrim(dataType)
    val boxedType = kaitaiType2JavaTypeBoxed(dataType)

    if (dataType.isInstanceOf[NumericType]) {
      // Special trick to achieve both type conversion + boxing.
      // Unfortunately, Java can't do both by itself, i.e. this would fail:
      //
      // Double c = 1.0f + 1;

      out.puts(s"${privateMemberName(instName)} = ${translator.doCast(value, dataType)};")
    } else {
      out.puts(s"${privateMemberName(instName)} = ${expression(value)};")
    }
  }

  override def instanceInvalidate(instName: InstanceIdentifier): Unit = {
    out.puts(s"public void _invalidate${idToSetterStr(instName)}() { ${privateMemberName(instName)} = null; }")
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

  override def internalEnumIntType(basedOn: IntType): DataType = {
    basedOn match {
      case IntMultiType(signed, _, endian) => IntMultiType(signed, Width8, endian)
      case _ => IntMultiType(true, Width8, None)
    }
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

  override def attrPrimitiveWrite(
    io: String,
    valueExpr: Ast.expr,
    dataType: DataType,
    defEndian: Option[FixedEndian],
    exprTypeOpt: Option[DataType]
  ): Unit = {
    val exprType = exprTypeOpt.getOrElse(dataType)
    val exprRaw = expression(valueExpr)
    val expr = castIfNeeded(exprRaw, exprType, dataType)

    val stmt = dataType match {
      case t: ReadableType =>
        s"$io.write${Utils.capitalize(t.apiCall(defEndian))}($expr)"
      case BitsType1(bitEndian) =>
        s"$io.writeBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}(1, ${translator.boolToInt(valueExpr)})"
      case BitsType(width: Int, bitEndian) =>
        s"$io.writeBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}($width, $expr)"
      case _: BytesType =>
        s"$io.writeBytes($expr)"
    }
    out.puts(stmt + ";")
  }

  override def attrBytesLimitWrite(io: String, expr: Ast.expr, size: String, term: Int, padRight: Int): Unit =
    out.puts(s"$io.writeBytesLimit(${expression(expr)}, $size, (byte) $term, (byte) $padRight);")

  override def attrUserTypeInstreamWrite(io: String, valueExpr: Ast.expr, dataType: DataType, exprType: DataType) = {
    val exprRaw = expression(valueExpr)
    val expr = castIfNeeded(exprRaw, exprType, dataType)
    out.puts(s"$expr._write_Seq($io);")
  }

  override def exprStreamToByteArray(io: String): String =
    s"$io.toByteArray()"

  override def attrBasicCheck(checkExpr: Ast.expr, actual: Ast.expr, expected: Ast.expr, msg: String): Unit = {
    val msgStr = expression(Ast.expr.Str(msg))

    out.puts(s"if (${expression(checkExpr)})")
    out.inc
    out.puts(s"throw new ConsistencyError($msgStr, ${expression(expected)}, ${expression(actual)});")
    out.dec

    importList.add("io.kaitai.struct.ConsistencyError")
  }

  override def attrObjectsEqualCheck(actual: Ast.expr, expected: Ast.expr, msg: String): Unit = {
    val msgStr = expression(Ast.expr.Str(msg))

    out.puts(s"if (!Objects.equals(${expression(actual)}, ${expression(expected)}))")
    out.inc
    out.puts(s"throw new ConsistencyError($msgStr, ${expression(expected)}, ${expression(actual)});")
    out.dec

    importList.add("java.util.Objects")
    importList.add("io.kaitai.struct.ConsistencyError")
  }

  override def attrParentParamCheck(actualParentExpr: Ast.expr, ut: UserType, shouldDependOnIo: Option[Boolean], msg: String): Unit = {
    /** @note Must be kept in sync with [[JavaCompiler.parseExpr]] */
    val (expectedParent, dependsOnIo) = ut.forcedParent match {
      case Some(USER_TYPE_NO_PARENT) => ("null", false)
      case Some(fp) =>
        (expression(fp), userExprDependsOnIo(fp))
      case None => ("this", false)
    }
    if (shouldDependOnIo.map(shouldDepend => dependsOnIo != shouldDepend).getOrElse(false))
      return

    val msgStr = expression(Ast.expr.Str(msg))

    out.puts(s"if (!Objects.equals(${expression(actualParentExpr)}, $expectedParent))")
    out.inc
    out.puts(s"throw new ConsistencyError($msgStr, $expectedParent, ${expression(actualParentExpr)});")
    out.dec

    importList.add("java.util.Objects")
    importList.add("io.kaitai.struct.ConsistencyError")
  }

  override def attrIsEofCheck(io: String, expectedIsEof: Boolean, msg: String): Unit = {
    val msgStr = expression(Ast.expr.Str(msg))

    val eofExpr = s"$io.isEof()"
    val ifExpr = if (expectedIsEof) {
      s"!($eofExpr)"
    } else {
      eofExpr
    }
    out.puts(s"if ($ifExpr)")
    out.inc
    out.puts(s"throw new ConsistencyError($msgStr, 0, ${exprIORemainingSize(io)});")
    out.dec

    importList.add("io.kaitai.struct.ConsistencyError")
  }

  override def condIfIsEofHeader(io: String, wantedIsEof: Boolean): Unit = {
    val eofExpr = s"$io.isEof()"
    val ifExpr = if (!wantedIsEof) {
      s"!($eofExpr)"
    } else {
      eofExpr
    }

    out.puts(s"if ($ifExpr) {")
    out.inc
  }

  override def condIfIsEofFooter: Unit = universalFooter

  def value2Const(s: String) = Utils.upperUnderscoreCase(s)

  override def idToStr(id: Identifier): String = JavaCompiler.idToStr(id)

  override def publicMemberName(id: Identifier): String = idToStr(id)

  def idToSetterStr(id: Identifier): String = {
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => Utils.upperCamelCase(name)
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => Utils.upperCamelCase(name)
      case RawIdentifier(innerId) => "_raw_" + idToSetterStr(innerId)
      case OuterSizeIdentifier(innerId) => s"${idToSetterStr(innerId)}_OuterSize"
      case InnerSizeIdentifier(innerId) => s"${idToSetterStr(innerId)}_InnerSize"
    }
  }

  override def privateMemberName(id: Identifier): String =
    JavaCompiler.privateMemberName(id, translator.inSubIOWriteBackHandler)

  override def localTemporaryName(id: Identifier): String = s"_t_${idToStr(id)}"

  def castIfNeeded(exprRaw: String, exprType: DataType, targetType: DataType): String =
    JavaCompiler.castIfNeeded(exprRaw, exprType, targetType, importList, config)

  def kaitaiType2JavaType(attrType: DataType): String =
    JavaCompiler.kaitaiType2JavaType(attrType, importList, config)

  def kaitaiType2JavaType(attrType: DataType, isNullable: Boolean): String =
    JavaCompiler.kaitaiType2JavaType(attrType, isNullable, importList, config)

  def kaitaiType2JavaTypePrim(attrType: DataType): String =
    JavaCompiler.kaitaiType2JavaTypePrim(attrType, importList, config)

  def kaitaiType2JavaTypeBoxed(attrType: DataType): String =
    JavaCompiler.kaitaiType2JavaTypeBoxed(attrType, importList, config)

  override def ksErrorName(err: KSError): String = err match {
    case EndOfStreamError => config.java.endOfStreamErrorClass
    case ConversionError => "NumberFormatException"
    case _ => s"KaitaiStream.${err.name}"
  }

  override def attrValidateExpr(
    attr: AttrLikeSpec,
    checkExpr: Ast.expr,
    err: KSError,
    useIo: Boolean,
    actual: Ast.expr,
    expected: Option[Ast.expr] = None
  ): Unit =
    attrValidate(attr, s"!(${translator.translate(checkExpr)})", err, useIo, actual, expected)

  override def attrValidateInEnum(
    attr: AttrLikeSpec,
    et: EnumType,
    valueExpr: Ast.expr,
    err: ValidationNotInEnumError,
    useIo: Boolean
  ): Unit = {
    // NOTE: this condition works for now because we haven't implemented
    // https://github.com/kaitai-io/kaitai_struct/issues/778 for Java yet, but
    // it will need to be changed when we do.
    attrValidate(attr, s"${translator.translate(valueExpr)} == null", err, useIo, valueExpr, None)
  }

  private def attrValidate(
    attr: AttrLikeSpec,
    failCondExpr: String,
    err: KSError,
    useIo: Boolean,
    actual: Ast.expr,
    expected: Option[Ast.expr]
  ): Unit = {
    val errArgsStr = expected.map(expression) ++ List(
      expression(actual),
      if (useIo) expression(Ast.expr.InternalName(IoIdentifier)) else "null",
      expression(Ast.expr.Str(attr.path.mkString("/", "/", "")))
    )
    out.puts(s"if ($failCondExpr) {")
    out.inc
    out.puts(s"throw new ${ksErrorName(err)}(${errArgsStr.mkString(", ")});")
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
      case OuterSizeIdentifier(innerId) => s"${idToStr(innerId)}_OuterSize"
      case InnerSizeIdentifier(innerId) => s"${idToStr(innerId)}_InnerSize"
    }

  def privateMemberName(id: Identifier, inSubIOWriteBackHandler: Boolean): String =
    s"${if (inSubIOWriteBackHandler) "_" else ""}this.${idToStr(id)}"

  def kaitaiType2JavaType(attrType: DataType, importList: ImportList, config: RuntimeConfig): String = kaitaiType2JavaTypePrim(attrType, importList, config)

  def kaitaiType2JavaType(attrType: DataType, isNullable: Boolean, importList: ImportList, config: RuntimeConfig): String =
    if (isNullable) {
      kaitaiType2JavaTypeBoxed(attrType, importList, config)
    } else {
      kaitaiType2JavaTypePrim(attrType, importList, config)
    }

  def castIfNeeded(exprRaw: String, exprType: DataType, targetType: DataType, importList: ImportList, config: RuntimeConfig): String =
    if (exprType != targetType) {
      val castTypeId = kaitaiType2JavaTypePrim(targetType, importList, config)
      targetType match {
        // Handles both unboxing + downcasting at the same time if needed
        // (solution from https://github.com/kaitai-io/kaitai_struct_compiler/pull/149)
        //
        // See also https://github.com/kaitai-io/kaitai_struct_compiler/pull/212#issuecomment-731149487
        case _: NumericType => s"((Number) ($exprRaw)).${castTypeId}Value()"
        case _ => s"(($castTypeId) ($exprRaw))"
      }
    } else {
      exprRaw
    }

  /**
    * Determine Java data type corresponding to a KS data type. A "primitive" type (i.e. "int", "long", etc) will
    * be returned if possible.
    *
    * @param attrType KS data type
    * @return Java data type
    */
  def kaitaiType2JavaTypePrim(attrType: DataType, importList: ImportList, config: RuntimeConfig): String = {
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
      case KaitaiStructType | CalcKaitaiStructType(_) => kstructNameFull(config)

      case t: UserType => types2class(t.name)
      case EnumType(name, _) => types2class(name)

      case _: ArrayType => kaitaiType2JavaTypeBoxed(attrType, importList, config)

      case st: SwitchType => kaitaiType2JavaTypePrim(st.combinedType, importList, config)
    }
  }

  /**
    * Determine Java data type corresponding to a KS data type. A non-primitive type (i.e. "Integer", "Long", etc) will
    * be returned, to be used when proper objects should be used.
    *
    * @param attrType KS data type
    * @return Java data type
    */
  def kaitaiType2JavaTypeBoxed(attrType: DataType, importList: ImportList, config: RuntimeConfig): String = {
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
      case KaitaiStructType | CalcKaitaiStructType(_) => kstructNameFull(config)

      case t: UserType => types2class(t.name)
      case EnumType(name, _) => types2class(name)

      case at: ArrayType => {
        importList.add("java.util.List")
        s"List<${kaitaiType2JavaTypeBoxed(at.elType, importList, config)}>"
      }

      case st: SwitchType => kaitaiType2JavaTypeBoxed(st.combinedType, importList, config)
    }
  }

  def types2class(names: List[String]) = names.map(x => type2class(x)).mkString(".")

  override def kstreamName: String = "KaitaiStream"
  override def kstructName: String = "KaitaiStruct"

  def kstructNameFull(config: RuntimeConfig): String = {
    kstructName + ((config.autoRead, config.readWrite) match {
      case (_, true) => ".ReadWrite"
      case (false, false) => ".ReadOnly"
      case (true, false) => ""
    })
  }
}
