package io.kaitai.struct.languages

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.PythonTranslator
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig, StringLanguageOutputWriter, Utils}

class PythonCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with ObjectOrientedLanguage
    with UpperCamelCaseClasses
    with SingleOutputFile
    with UniversalFooter
    with EveryReadIsExpression
    with FetchInstances
    with EveryWriteIsExpression
    with GenericChecks
    with AllocateIOLocalVar
    with UniversalDoc
    with SwitchIfOps
    with NoNeedForFullClassPath {

  import PythonCompiler._

  override val translator = new PythonTranslator(typeProvider, importList)

  override def innerDocstrings = true

  /** See [[subIOWriteBackHeader]] => the code generated when `true` will be inside the definition
   * of the "write back handler" callback function. */
  private var inSubIOWriteBackHandler = false

  override def universalFooter: Unit = {
    out.dec
    out.puts
  }

  override def indent: String = "    "
  override def outFileName(topClassName: String): String = s"$topClassName.py"

  override def outImports(topClass: ClassSpec) =
    importList.toList.mkString("", "\n", "\n")

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"# $headerComment")

    // https://github.com/kaitai-io/kaitai_struct/issues/675
    // TODO: Make conditional once we'll have Python type annotations
    outHeader.puts("# type: ignore")

    outHeader.puts

    importList.add("import kaitaistruct")
    importList.add(s"from kaitaistruct import $kstructNameFull, $kstreamName, BytesIO")

    out.puts
    out.puts

    // API compatibility check
    out.puts(
      // The API_VERSION tuple attribute was introduced in version 0.10 of the
      // Python runtime. Runtime version 0.9 and older only have a __version__
      // attribute that stores the version in string form.
      // We don't need to include any complex handling for runtimes that don't
      // have the API_VERSION attribute - we know that such a runtime must have
      // version 0.9 or older, which means that it is incompatible with code
      // generated by newer compiler versions.
      "if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < " +
        KSVersion.minimalRuntime.toPythonTuple +
        ":"
    )
    out.inc
    out.puts(
      "raise Exception(\"Incompatible Kaitai Struct Python API: " +
        KSVersion.minimalRuntime +
        " or later is required, but you have %s\" % (kaitaistruct.__version__))"
    )
    out.dec
    out.puts
  }

  override def opaqueClassDeclaration(classSpec: ClassSpec): Unit = {
    val name = classSpec.name.head
    out.puts(
      if (config.pythonPackage.nonEmpty) {
        s"from ${config.pythonPackage} import $name"
      } else {
        s"import $name"
      }
    )
  }

  override def classHeader(name: String): Unit = {
    out.puts(s"class ${type2class(name)}($kstructNameFull):")
    out.inc
  }

  override def classConstructorHeader(name: String, parentType: DataType, rootClassName: String, isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    val endianAdd = if (isHybrid) ", _is_le=None" else ""
    val paramsList = Utils.join(params.map((p) => paramName(p.id)), ", ", ", ", "")

    val ioDefaultVal = if (config.readWrite) "=None" else ""
    out.puts(s"def __init__(self$paramsList, _io$ioDefaultVal, _parent=None, _root=None$endianAdd):")
    out.inc
    out.puts("self._io = _io")
    out.puts("self._parent = _parent")
    if (name == rootClassName) {
      out.puts("self._root = _root if _root else self")
    } else {
      out.puts("self._root = _root")
    }

    if (isHybrid)
      out.puts("self._is_le = _is_le")

    // Store parameters passed to us
    params.foreach((p) => handleAssignmentSimple(p.id, paramName(p.id)))

    if (config.readStoresPos) {
      importList.add("import collections")
      out.puts("self._debug = collections.defaultdict(dict)")
    }
  }

  override def runRead(name: List[String]): Unit = {
    out.puts("self._read()")
  }

  override def runReadCalc(): Unit = {
    out.puts("if not hasattr(self, '_is_le'):")
    out.inc
    out.puts(s"raise ${ksErrorName(UndecidedEndiannessError)}(" + "\"" + typeProvider.nowClass.path.mkString("/", "/", "") + "\")")
    out.dec
    out.puts("elif self._is_le == True:")
    out.inc
    out.puts("self._read_le()")
    out.dec
    out.puts("elif self._is_le == False:")
    out.inc
    out.puts("self._read_be()")
    out.dec
  }

  override def runWriteCalc(): Unit = {
    out.puts("if not hasattr(self, '_is_le'):")
    out.inc
    out.puts(s"raise ${ksErrorName(UndecidedEndiannessError)}(" + "\"" + typeProvider.nowClass.path.mkString("/", "/", "") + "\")")
    out.dec
    out.puts("elif self._is_le == True:")
    out.inc
    out.puts("self._write__seq_le()")
    out.dec
    out.puts("elif self._is_le == False:")
    out.inc
    out.puts("self._write__seq_be()")
    out.dec
  }

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = {
    val suffix = endian match {
      case Some(e) => s"_${e.toSuffix}"
      case None => ""
    }
    out.puts(s"def _read$suffix(self):")
    out.inc
    if (isEmpty)
      out.puts("pass")
  }

  override def fetchInstancesHeader(): Unit = {
    out.puts
    out.puts("def _fetch_instances(self):")
    out.inc
    out.puts("pass")
  }

  override def fetchInstancesFooter: Unit = universalFooter

  override def attrInvokeFetchInstances(baseExpr: Ast.expr, exprType: DataType, dataType: DataType): Unit = {
    val expr = expression(baseExpr)
    out.puts(s"$expr._fetch_instances()")
  }

  override def attrInvokeInstance(instName: InstanceIdentifier): Unit = {
    out.puts(s"_ = self.${publicMemberName(instName)}")
  }

  override def writeHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = {
    out.puts
    endian match {
      case Some(e) =>
        out.puts(s"def _write__seq_${e.toSuffix}(self):")
        out.inc
        if (isEmpty)
          out.puts("pass")
      case None =>
        out.puts("def _write__seq(self, io=None):")
        out.inc
        // FIXME: remove super() args when dropping support for Python 2 (see
        // https://pylint.readthedocs.io/en/v2.16.2/user_guide/messages/refactor/super-with-arguments.html)
        out.puts(s"super(${types2class(typeProvider.nowClass.name)}, self)._write__seq(io)")
    }
  }

  override def checkHeader(): Unit = {
    out.puts
    out.puts("def _check(self):")
    out.inc
    out.puts("pass")
  }

  override def writeInstanceHeader(instName: InstanceIdentifier): Unit = {
    out.puts
    out.puts(s"def _write_${publicMemberName(instName)}(self):")
    out.inc
    instanceClearWriteFlag(instName)
  }

  override def checkInstanceHeader(instName: InstanceIdentifier): Unit = {
    out.puts
    out.puts(s"def _check_${publicMemberName(instName)}(self):")
    out.inc
    out.puts("pass")
  }

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {}

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {}

  override def attributeSetter(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    if (attrName.isInstanceOf[InstanceIdentifier]) {
      val name = publicMemberName(attrName)

      out.puts(s"@$name.setter")
      out.puts(s"def $name(self, v):")
      out.inc
      handleAssignmentSimple(attrName, "v")
      out.dec
    }
  }

  override def attrSetProperty(base: Ast.expr, propName: Identifier, value: String): Unit = {
    out.puts(s"${expression(base)}.${publicMemberName(propName)} = $value")
  }

  override def universalDoc(doc: DocSpec): Unit = {
    val docStr = doc.summary match {
      case Some(summary) =>
        val lastChar = summary.last
        if (lastChar == '.' || lastChar == '\n') {
          summary
        } else {
          summary + "."
        }
      case None =>
        ""
    }

    val extraNewline = if (docStr.isEmpty || docStr.last == '\n') "" else "\n"
    val refStr = doc.ref.map {
      case TextRef(text) =>
        val seeAlso = new StringLanguageOutputWriter("")
        seeAlso.putsLines("   ", text)
        s"$extraNewline\n.. seealso::\n${seeAlso.result}"
      case ref: UrlRef =>
        val seeAlso = new StringLanguageOutputWriter("")
        seeAlso.putsLines("   ", s"${ref.text} - ${ref.url}")
        s"$extraNewline\n.. seealso::\n${seeAlso.result}"
    }.mkString("\n")

    out.putsLines("", "\"\"\"" + docStr + refStr + "\"\"\"")
  }

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {
    out.puts("if self._is_le:")
    out.inc
    leProc()
    out.dec
    out.puts("else:")
    out.inc
    beProc()
    out.dec
  }

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec): Unit = {
    val srcExpr = getRawIdExpr(varSrc, rep)

    val expr = proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "process_xor_one"
          case _: BytesType => "process_xor_many"
        }
        s"$kstreamName.$procName($srcExpr, ${expression(xorValue)})"
      case ProcessZlib =>
        importList.add("import zlib")
        s"zlib.decompress($srcExpr)"
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        s"$kstreamName.process_rotate_left($srcExpr, $expr, 1)"
      case ProcessCustom(name, args) =>
        val procClass = if (name.length == 1) {
          val onlyName = name.head
          val className = type2class(onlyName)
          importList.add(s"from $onlyName import $className")
          className
        } else {
          val pkgName = name.init.mkString(".")
          importList.add(s"import $pkgName")
          s"$pkgName.${type2class(name.last)}"
        }
        out.puts(s"_process = $procClass(${args.map(expression).mkString(", ")})")
        s"_process.decode($srcExpr)"
    }
    handleAssignment(varDest, expr, rep, false)
  }

  override def attrUnprocess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec, dt: BytesType, exprTypeOpt: Option[DataType]): Unit = {
    val srcExpr = varSrc match {
      // use `_raw_items[_raw_items.size - 1]`
      case _: RawIdentifier => getRawIdExpr(varSrc, rep)
      // but `items[_index]`
      case _ => expression(itemExpr(varSrc, rep))
    }

    val expr = proc match {
      case ProcessXor(xorValue) =>
        val argStr = if (inSubIOWriteBackHandler) "_process_val" else expression(xorValue)
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "process_xor_one"
          case _: BytesType => "process_xor_many"
        }
        s"$kstreamName.$procName($srcExpr, $argStr)"
      case ProcessZlib =>
        importList.add("import zlib")
        s"zlib.compress($srcExpr)"
      case ProcessRotate(isLeft, rotValue) =>
        val argStr = if (inSubIOWriteBackHandler) "_process_val" else expression(rotValue)
        val expr = if (!isLeft) {
          argStr
        } else {
          s"8 - ($argStr)"
        }
        s"$kstreamName.process_rotate_left($srcExpr, $expr, 1)"
      case ProcessCustom(name, args) =>
        val procClass = if (name.length == 1) {
          val onlyName = name.head
          val className = type2class(onlyName)
          importList.add(s"from $onlyName import $className")
          className
        } else {
          val pkgName = name.init.mkString(".")
          importList.add(s"import $pkgName")
          s"$pkgName.${type2class(name.last)}"
        }

        val procName = if (inSubIOWriteBackHandler) {
          "_process_val"
        } else {
          val procName = s"_process_${idToStr(varSrc)}"
          out.puts(s"$procName = $procClass(${args.map(expression).mkString(", ")})")
          procName
        }
        s"$procName.encode($srcExpr)"
    }
    handleAssignment(varDest, expr, rep, false)
  }

  override def attrUnprocessPrepareBeforeSubIOHandler(proc: ProcessExpr, varSrc: Identifier): Unit = {
    // NOTE: the local variable "_process_val" will be captured in a default value of a parameter
    // when defining the "write back handler" function (in subIOWriteBackHeader), see
    // https://docs.python.org/3/faq/programming.html#why-do-lambdas-defined-in-a-loop-with-different-values-all-return-the-same-result
    proc match {
      case ProcessXor(xorValue) =>
        out.puts(s"_process_val = ${expression(xorValue)}")
      case ProcessRotate(_, rotValue) =>
        out.puts(s"_process_val = ${expression(rotValue)}")
      case ProcessZlib => // no process arguments
      case ProcessCustom(name, args) =>
        val procClass = if (name.length == 1) {
          val onlyName = name.head
          val className = type2class(onlyName)
          importList.add(s"from $onlyName import $className")
          className
        } else {
          val pkgName = name.init.mkString(".")
          importList.add(s"import $pkgName")
          s"$pkgName.${type2class(name.last)}"
        }
        val procName = "_process_val"
        out.puts(s"$procName = $procClass(${args.map(expression).mkString(", ")})")
    }
  }

  override def normalIO: String = "self._io"

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = {
    val varStr = privateMemberName(varName)
    val ioName = s"_io_${idToStr(varName)}"

    val args = getRawIdExpr(varName, rep)

    out.puts(s"$ioName = $kstreamName(BytesIO($args))")
    ioName
  }

  override def allocateIOFixed(varName: Identifier, size: String): String = {
    val varStr = privateMemberName(varName)
    val ioName = s"_io_${idToStr(varName)}"

    // NOTE: in Python 2, bytes() converts an integer argument to a string (e.g. bytes(12) => '12'),
    // so we have to use bytearray() instead
    out.puts(s"$ioName = $kstreamName(BytesIO(bytearray($size)))")
    ioName
  }

  override def exprIORemainingSize(io: String): String =
    s"$io.size() - $io.pos()"

  override def subIOWriteBackHeader(subIO: String, process: Option[ProcessExpr]): String = {
    val parentIoName = "parent"
    // NOTE: local variables "$subIO" and "_process_val" are captured here as default values of
    // "handler" parameters, see
    // https://docs.python.org/3/faq/programming.html#why-do-lambdas-defined-in-a-loop-with-different-values-all-return-the-same-result
    val processValArg =
      process.map(proc => proc match {
        case _: ProcessXor | _: ProcessRotate | _: ProcessCustom =>
          ", _process_val=_process_val"
        case _ =>
          ""
      }).getOrElse("")
    out.puts(s"def handler(parent, $subIO=$subIO$processValArg):")
    out.inc

    inSubIOWriteBackHandler = true

    parentIoName
  }

  override def subIOWriteBackFooter(subIO: String): Unit = {
    inSubIOWriteBackHandler = false

    out.dec
    out.puts(s"$subIO.write_back_handler = $kstreamName.WriteBackHandler(_pos2, handler)")
  }

  override def addChildIO(io: String, childIO: String): Unit =
    out.puts(s"$io.add_child_stream($childIO)")

  def getRawIdExpr(varName: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(varName)
    rep match {
      case NoRepeat => memberName
      case _ => s"$memberName[-1]"
    }
  }

  override def useIO(ioEx: expr): String = {
    out.puts(s"io = ${expression(ioEx)}")
    "io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"_pos = $io.pos()")

  override def pushPosForSubIOWriteBackHandler(io: String): Unit =
    out.puts(s"_pos2 = $io.pos()")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io.seek(${expression(pos)})")

  override def seekRelative(io: String, relPos: String): Unit =
    out.puts(s"$io.seek($io.pos() + ($relPos))")

  override def popPos(io: String): Unit =
    out.puts(s"$io.seek(_pos)")

  // NOTE: the compiler does not need to output align_to_byte() calls for Python anymore,
  // since the byte alignment is handled by the runtime library since commit
  // https://github.com/kaitai-io/kaitai_struct_python_runtime/commit/1cb84b84d358e1cdffe35845d1e6688bff923952
  override def alignToByte(io: String): Unit = {}

  override def attrDebugStart(attrId: Identifier, attrType: DataType, ios: Option[String], rep: RepeatSpec): Unit = {
    ios.foreach { (io) =>
      val name = attrId match {
        case _: RawIdentifier | _: SpecialIdentifier => return
        case _ => idToStr(attrId)
      }
      rep match {
        case NoRepeat =>
          out.puts(s"self._debug['$name']['start'] = $io.pos()")
        case _: RepeatExpr | RepeatEos | _: RepeatUntil =>
          /** TODO: move array initialization to [[condRepeatCommonInit]] - see
           * [[JavaScriptCompiler.condRepeatCommonInit]] for inspiration */
          out.puts(s"if not 'arr' in self._debug['$name']:")
          out.inc
          out.puts(s"self._debug['$name']['arr'] = []")
          out.dec
          out.puts(s"self._debug['$name']['arr'].append({'start': $io.pos()})")
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
        out.puts(s"self._debug['$name']['end'] = $io.pos()")
      case _: RepeatExpr =>
        out.puts(s"self._debug['$name']['arr'][i]['end'] = $io.pos()")
      case RepeatEos | _: RepeatUntil =>
        out.puts(s"self._debug['$name']['arr'][len(${privateMemberName(attrId)}) - 1]['end'] = $io.pos()")
    }
  }

  override def condIfHeader(expr: Ast.expr): Unit = {
    out.puts(s"if ${expression(expr)}:")
    out.inc
    out.puts("pass")
  }

  override def condRepeatCommonInit(id: Identifier, dataType: DataType, needRaw: NeedRaw): Unit = {
    if (needRaw.level >= 1)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = []")
    if (needRaw.level >= 2)
      out.puts(s"${privateMemberName(RawIdentifier(RawIdentifier(id)))} = []")
    if (config.readWrite) {
      dataType match {
        case utb: UserTypeFromBytes =>
          if (writeNeedsOuterSize(utb.bytes))
            out.puts(s"${privateMemberName(OuterSizeIdentifier(id))} = []")
          if (writeNeedsInnerSize(utb.bytes))
            out.puts(s"${privateMemberName(InnerSizeIdentifier(id))} = []")
        case _ => // do nothing
      }
    }
    out.puts(s"${privateMemberName(id)} = []")
  }

  override def condRepeatCommonWriteInit(id: Identifier, dataType: DataType, needRaw: NeedRaw): Unit = {
    if (needRaw.level >= 1)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = []")
    if (needRaw.level >= 2)
      out.puts(s"${privateMemberName(RawIdentifier(RawIdentifier(id)))} = []")
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType): Unit = {
    out.puts("i = 0")
    out.puts(s"while not $io.is_eof():")
    out.inc
  }
  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)}.append($expr)")

  override def condRepeatEosFooter: Unit = {
    out.puts("i += 1")
    universalFooter
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, repeatExpr: expr): Unit = {
    out.puts(s"for i in range(${expression(repeatExpr)}):")
    out.inc
  }

  // used for all repetitions in _check()
  override def condRepeatCommonHeader(id: Identifier, io: String, dataType: DataType): Unit = {
    // TODO: replace range(len()) with enumerate() (see
    // https://pylint.readthedocs.io/en/v2.16.2/user_guide/messages/convention/consider-using-enumerate.html)
    out.puts(s"for i in range(len(${privateMemberName(id)})):")
    out.inc
    out.puts("pass")
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit =
    handleAssignmentRepeatEos(id, expr)

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, untilExpr: expr): Unit = {
    out.puts("i = 0")
    out.puts("while True:")
    out.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val tmpName = translator.doName(if (isRaw) Identifier.ITERATOR2 else Identifier.ITERATOR)
    out.puts(s"$tmpName = $expr")
    out.puts(s"${privateMemberName(id)}.append($tmpName)")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, untilExpr: expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.puts(s"if ${expression(untilExpr)}:")
    out.inc
    out.puts("break")
    out.dec
    out.puts("i += 1")
    out.dec
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)} = $expr")

  override def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit =
    out.puts(s"$id = $expr")

  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    dataType match {
      case t: ReadableType =>
        s"$io.read_${t.apiCall(defEndian)}()"
      case blt: BytesLimitType =>
        s"$io.read_bytes(${expression(blt.size)})"
      case _: BytesEosType =>
        s"$io.read_bytes_full()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        s"$io.read_bytes_term($terminator, ${bool2Py(include)}, ${bool2Py(consume)}, ${bool2Py(eosError)})"
      case BitsType1(bitEndian) =>
        s"$io.read_bits_int_${bitEndian.toSuffix}(1) != 0"
      case BitsType(width: Int, bitEndian) =>
        s"$io.read_bits_int_${bitEndian.toSuffix}($width)"
      case t: UserType =>
        val addParams = Utils.join(t.args.map((a) => translator.translate(a)), "", ", ", ", ")
        val addArgs = if (t.isOpaque) {
          ""
        } else {
          val parent = t.forcedParent match {
            case Some(USER_TYPE_NO_PARENT) => "None"
            case Some(fp) => translator.translate(fp)
            case None => "self"
          }
          val addEndian = t.classSpec.get.meta.endian match {
            case Some(InheritedEndian) => ", self._is_le"
            case _ => ""
          }
          s", $parent, self._root$addEndian"
        }
        s"${userType2class(t)}($addParams$io$addArgs)"
    }
  }

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean) = {
    val expr1 = padRight match {
      case Some(padByte) if terminator.map(term => padByte != term).getOrElse(true) =>
        s"$kstreamName.bytes_strip_right($expr0, $padByte)"
      case _ => expr0
    }
    val expr2 = terminator match {
      case Some(term) => s"$kstreamName.bytes_terminate($expr1, $term, ${bool2Py(include)})"
      case None => expr1
    }
    expr2
  }

  override def userTypeDebugRead(id: String, dataType: DataType, assignType: DataType): Unit =
    out.puts(s"$id._read()")

  override def switchStart(id: Identifier, on: Ast.expr): Unit = {}
  override def switchCaseStart(condition: Ast.expr): Unit = {}
  override def switchCaseEnd(): Unit = {}
  override def switchElseStart(): Unit = {}
  override def switchEnd(): Unit = {}

  override def switchRequiresIfs(onType: DataType): Boolean = true
  override def switchIfStart(id: Identifier, on: Ast.expr, onType: DataType): Unit = {
    out.puts(s"_on = ${expression(on)}")
  }

  override def switchIfCaseFirstStart(condition: Ast.expr): Unit = {
    out.puts(s"if _on == ${expression(condition)}:")
    out.inc
    out.puts("pass")
  }

  override def switchIfCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"elif _on == ${expression(condition)}:")
    out.inc
    out.puts("pass")
  }

  override def switchIfCaseEnd(): Unit =
    out.dec

  override def switchIfElseStart(): Unit = {
    out.puts(s"else:")
    out.inc
    out.puts("pass")
  }

  override def switchIfEnd(): Unit = {}

  override def instanceWriteFlagDeclaration(attrName: InstanceIdentifier): Unit = {}

  override def instanceWriteFlagInit(attrName: InstanceIdentifier): Unit = {
    instanceClearWriteFlag(attrName)
    out.puts(s"self.${publicMemberName(attrName)}__to_write = True")
  }

  override def instanceSetWriteFlag(instName: InstanceIdentifier): Unit = {
    out.puts(s"self._should_write_${publicMemberName(instName)} = self.${publicMemberName(instName)}__to_write")
  }

  override def instanceClearWriteFlag(instName: InstanceIdentifier): Unit = {
    out.puts(s"self._should_write_${publicMemberName(instName)} = False")
  }

  override def instanceToWriteSetter(instName: InstanceIdentifier): Unit = {}

  override def instanceHeader(className: String, instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    out.puts("@property")
    out.puts(s"def ${publicMemberName(instName)}(self):")
    out.inc
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    out.puts(s"if hasattr(self, '${idToStr(instName)}'):")
    out.inc
    out.puts(s"return ${privateMemberName(instName)}")
    out.dec
    out.puts
  }

  override def instanceCheckWriteFlagAndWrite(instName: InstanceIdentifier): Unit = {
    out.puts(s"if self._should_write_${publicMemberName(instName)}:")
    out.inc
    out.puts(s"self._write_${publicMemberName(instName)}()")
    out.dec
  }

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit = {
    // workaround to avoid Python raising an "AttributeError: instance has no attribute"
    out.puts(s"return getattr(self, '${idToStr(instName)}', None)")
  }

  override def instanceInvalidate(instName: InstanceIdentifier): Unit = {
    out.puts(s"def _invalidate_${publicMemberName(instName)}(self):")
    out.inc
    out.puts(s"del ${privateMemberName(instName)}")
    out.dec
  }

  override def enumDeclaration(curClass: String, enumName: String, enumColl: Seq[(Long, String)]): Unit = {
    importList.add("from enum import Enum")

    out.puts
    out.puts(s"class ${type2class(enumName)}(Enum):")
    out.inc
    enumColl.foreach { case (id: Long, label: String) => out.puts(s"$label = ${translator.doIntLiteral(id)}") }
    out.dec
  }

  override def internalEnumIntType(basedOn: IntType): DataType =
    basedOn

  override def debugClassSequence(seq: List[AttrSpec]) = {
    val seqStr = seq.map((attr) => "\"" + idToStr(attr.id) + "\"").mkString(", ")
    out.puts(s"SEQ_FIELDS = [$seqStr]")
  }

  override def classToString(toStringExpr: Ast.expr): Unit = {
    out.puts
    out.puts("def __repr__(self):")
    out.inc
    out.puts(s"return ${translator.translate(toStringExpr)}")
    out.dec
  }

  override def attrPrimitiveWrite(
    io: String,
    valueExpr: Ast.expr,
    dataType: DataType,
    defEndian: Option[FixedEndian],
    exprTypeOpt: Option[DataType]
  ): Unit = {
    val expr = expression(valueExpr)

    val stmt = dataType match {
      case t: ReadableType =>
        s"$io.write_${t.apiCall(defEndian)}($expr)"
      case BitsType1(bitEndian) =>
        s"$io.write_bits_int_${bitEndian.toSuffix}(1, ${translator.boolToInt(valueExpr)})"
      case BitsType(width: Int, bitEndian) =>
        s"$io.write_bits_int_${bitEndian.toSuffix}($width, $expr)"
      case _: BytesType =>
        s"$io.write_bytes($expr)"
    }
    out.puts(stmt)
  }

  override def attrBytesLimitWrite(io: String, expr: Ast.expr, size: String, term: Int, padRight: Int): Unit =
    out.puts(s"$io.write_bytes_limit(${expression(expr)}, $size, $term, $padRight)")

  override def attrUserTypeInstreamWrite(io: String, valueExpr: Ast.expr, dataType: DataType, exprType: DataType) = {
    val expr = expression(valueExpr)
    out.puts(s"$expr._write__seq($io)")
  }

  override def exprStreamToByteArray(io: String): String =
    s"$io.to_byte_array()"

  override def attrBasicCheck(checkExpr: Ast.expr, actual: Ast.expr, expected: Ast.expr, msg: String): Unit = {
    val msgStr = expression(Ast.expr.Str(msg))

    out.puts(s"if ${expression(checkExpr)}:")
    out.inc
    out.puts(s"raise kaitaistruct.ConsistencyError($msgStr, ${expression(actual)}, ${expression(expected)})")
    out.dec
  }

  override def attrObjectsEqualCheck(actual: Ast.expr, expected: Ast.expr, msg: String): Unit = {
    val msgStr = expression(Ast.expr.Str(msg))

    out.puts(s"if ${expression(actual)} != ${expression(expected)}:")
    out.inc
    out.puts(s"raise kaitaistruct.ConsistencyError($msgStr, ${expression(actual)}, ${expression(expected)})")
    out.dec
  }

  override def attrParentParamCheck(actualParentExpr: Ast.expr, ut: UserType, shouldDependOnIo: Option[Boolean], msg: String): Unit = {
    if (ut.isOpaque)
      return
    /** @note Must be kept in sync with [[PythonCompiler.parseExpr]] */
    val (expectedParent, dependsOnIo) = ut.forcedParent match {
      case Some(USER_TYPE_NO_PARENT) => ("None", false)
      case Some(fp) =>
        (expression(fp), userExprDependsOnIo(fp))
      case None => ("self", false)
    }
    if (shouldDependOnIo.map(shouldDepend => dependsOnIo != shouldDepend).getOrElse(false))
      return

    val msgStr = expression(Ast.expr.Str(msg))

    out.puts(s"if ${expression(actualParentExpr)} != $expectedParent:")
    out.inc
    out.puts(s"raise kaitaistruct.ConsistencyError($msgStr, ${expression(actualParentExpr)}, $expectedParent)")
    out.dec
  }

  override def attrIsEofCheck(io: String, expectedIsEof: Boolean, msg: String): Unit = {
    val msgStr = expression(Ast.expr.Str(msg))

    val eofExpr = s"$io.is_eof()"
    val ifExpr = if (expectedIsEof) {
      s"not $eofExpr"
    } else {
      eofExpr
    }
    out.puts(s"if $ifExpr:")
    out.inc
    out.puts(s"raise kaitaistruct.ConsistencyError($msgStr, ${exprIORemainingSize(io)}, 0)")
    out.dec
  }

  override def condIfIsEofHeader(io: String, wantedIsEof: Boolean): Unit = {
    val eofExpr = s"$io.is_eof()"
    val ifExpr = if (!wantedIsEof) {
      s"not $eofExpr"
    } else {
      eofExpr
    }

    out.puts(s"if $ifExpr:")
    out.inc
  }

  override def condIfIsEofFooter: Unit = universalFooter

  def bool2Py(b: Boolean): String = if (b) { "True" } else { "False" }

  override def idToStr(id: Identifier): String = PythonCompiler.idToStr(id)

  override def publicMemberName(id: Identifier): String = PythonCompiler.publicMemberName(id)

  override def privateMemberName(id: Identifier): String = s"self.${idToStr(id)}"

  override def localTemporaryName(id: Identifier): String = s"_t_${idToStr(id)}"

  override def ksErrorName(err: KSError): String = PythonCompiler.ksErrorName(err)

  override def attrValidateExpr(
    attrId: Identifier,
    attrType: DataType,
    checkExpr: Ast.expr,
    err: KSError,
    errArgs: List[Ast.expr]
  ): Unit = {
    val errArgsStr = errArgs.map(translator.translate).mkString(", ")
    out.puts(s"if not ${translator.translate(checkExpr)}:")
    out.inc
    out.puts(s"raise ${ksErrorName(err)}($errArgsStr)")
    out.dec
  }

  def kstructNameFull: String = {
    ((config.autoRead, config.readWrite) match {
      case (_, true) => "ReadWrite"
      case (_, false) => ""
    }) + kstructName
  }

  def userType2class(t: UserType): String = {
    val name = t.classSpec.get.name
    val firstName = name.head
    val prefix = if (t.isOpaque && firstName != translator.provider.nowClass.name.head) {
      s"$firstName."
    } else {
      ""
    }
    s"$prefix${types2class(name)}"
  }
}

object PythonCompiler extends LanguageCompilerStatic
  with UpperCamelCaseClasses
  with StreamStructNames
  with ExceptionNames {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new PythonCompiler(tp, config)

  def idToStr(id: Identifier): String =
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => name
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => s"_m_$name"
      case RawIdentifier(innerId) => s"_raw_${idToStr(innerId)}"
      case OuterSizeIdentifier(innerId) => s"${idToStr(innerId)}__outer_size"
      case InnerSizeIdentifier(innerId) => s"${idToStr(innerId)}__inner_size"
    }

  def publicMemberName(id: Identifier): String =
    id match {
      case InstanceIdentifier(name) => name
      case _ => idToStr(id)
    }

  override def kstreamName: String = "KaitaiStream"
  override def kstructName: String = "KaitaiStruct"
  override def ksErrorName(err: KSError): String = err match {
    case EndOfStreamError => "EOFError"
    case ConversionError => "ValueError"
    case _ => s"kaitaistruct.${err.name}"
  }

  def types2class(name: List[String]): String = name.map(x => type2class(x)).mkString(".")
}
