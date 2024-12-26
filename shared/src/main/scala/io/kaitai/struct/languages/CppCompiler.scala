package io.kaitai.struct.languages

import io.kaitai.struct.CppRuntimeConfig._
import io.kaitai.struct._
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.{CppTranslator, TypeDetector}

class CppCompiler(
  typeProvider: ClassTypeProvider,
  config: RuntimeConfig
) extends LanguageCompiler(typeProvider, config)
    with ObjectOrientedLanguage
    with AllocateAndStoreIO
    with FixedContentsUsingArrayByteLiteral
    with UniversalDoc
    with SwitchIfOps
    with EveryReadIsExpression {
  import CppCompiler._

  val importListSrc = new CppImportList
  val importListHdr = new CppImportList

  override val translator = new CppTranslator(typeProvider, importListSrc, importListHdr, config)
  val outSrcHeader = new StringLanguageOutputWriter(indent)
  val outHdrHeader = new StringLanguageOutputWriter(indent)
  val outSrc = new StringLanguageOutputWriter(indent)
  val outHdr = new StringLanguageOutputWriter(indent)

  override def results(topClass: ClassSpec): Map[String, String] = {
    val className = topClass.nameAsStr
    Map(
      outFileNameSource(className) -> (outSrcHeader.result + importListSrc.result + outSrc.result),
      outFileNameHeader(className) -> (outHdrHeader.result + importListHdr.result + outHdr.result)
    )
  }

  sealed trait AccessMode
  case object PrivateAccess extends AccessMode
  case object PublicAccess extends AccessMode

  var accessMode: AccessMode = PublicAccess

  override def indent: String = "    "

  override def fileHeader(topClassName: String): Unit = {
    outSrcHeader.puts(s"// $headerComment")
    outSrcHeader.puts

    importListSrc.addLocal(outFileNameHeader(topClassName))

    if (config.cppConfig.usePragmaOnce) {
      outHdrHeader.puts("#pragma once")
    } else {
      outHdrHeader.puts(s"#ifndef ${defineName(topClassName)}")
      outHdrHeader.puts(s"#define ${defineName(topClassName)}")
    }
    outHdrHeader.puts
    outHdrHeader.puts(s"// $headerComment")
    outHdrHeader.puts
    // Forward declaration of the top-level class defined later in this header
    // file. It's important to do this before printing `importListHdr` because
    // it contains `#include`s of header files of external .ksy modules that
    // could circularly import this header file (which does nothing because of
    // header guards, though, so if the other header files that tried to include
    // this one refer to our top-level class by name, which is very likely, then
    // we need to ensure that the C++ compiler has already seen the following
    // forward declaration).
    outHdrHeader.puts(s"class ${type2class(topClassName)};")
    outHdrHeader.puts

    importListHdr.addKaitai("kaitai/kaitaistruct.h")
    importListHdr.addSystem("stdint.h")

    config.cppConfig.pointers match {
      case UniqueAndRawPointers =>
        importListHdr.addSystem("memory")
      case RawPointers =>
        // no extra includes
    }

    // API compatibility check
    val minVer = KSVersion.minimalRuntime.toInt
    outHdr.puts
    outHdr.puts(s"#if KAITAI_STRUCT_VERSION < ${minVer}L")
    outHdr.puts(
      "#error \"Incompatible Kaitai Struct C++/STL API: version " +
        KSVersion.minimalRuntime + " or later is required\""
    )
    outHdr.puts("#endif")

    config.cppConfig.namespace.foreach { (namespace) =>
      outSrc.puts(s"namespace $namespace {")
      outSrc.inc
      outHdr.puts(s"namespace $namespace {")
      outHdr.inc
    }
  }

  override def fileFooter(topClassName: String): Unit = {
    config.cppConfig.namespace.foreach { (_) =>
      outSrc.dec
      outSrc.puts("}")
      outHdr.dec
      outHdr.puts("}")
    }

    if (!config.cppConfig.usePragmaOnce) {
      outHdr.puts
      outHdr.puts(s"#endif  // ${defineName(topClassName)}")
    }
  }

  override def externalTypeDeclaration(extType: ExternalType): Unit =
    importListHdr.addLocal(outFileNameHeader(extType.name.head))

  override def classHeader(name: List[String]): Unit = {
    val className = types2class(List(name.last))

    outHdr.puts
    outHdr.puts(s"class $className : public $kstructName {")
    outHdr.inc
    accessMode = PrivateAccess
    ensureMode(PublicAccess)

    /*
    outHdr.puts(s"static ${type2class(name)} from_file(std::string ${attrReaderName("file_name")});")

    outSrc.puts
    outSrc.puts(s"${type2class(name)} ${type2class(name)}::from_file(std::string ${attrReaderName("file_name")}) {")
    outSrc.inc
    outSrc.puts("std::ifstream* ifs = new std::ifstream(file_name, std::ifstream::binary);")
    outSrc.puts("kaitai::kstream *ks = new kaitai::kstream(ifs);")
    outSrc.puts(s"return new ${type2class(name)}(ks);")
    outSrc.dec
    outSrc.puts("}")
    */
  }

  override def classFooter(name: List[String]): Unit = {
    outHdr.dec
    outHdr.puts("};")
  }

  override def classForwardDeclaration(name: List[String]): Unit = {
    outHdr.puts(s"class ${types2class(name)};")
  }

  def importDataType(dt: DataType) = {
    dt match {
      case ut: UserType =>
        val classSpec = ut.classSpec.get
        if (classSpec.isTopLevel)
          importListSrc.addLocal(outFileNameHeader(classSpec.name.head))
      case _ => // no extra imports required
    }
  }

  override def classConstructorHeader(name: List[String], parentType: DataType, rootClassName: List[String], isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    val (endianSuffixHdr, endianSuffixSrc)  = if (isHybrid) {
      (", int p_is_le = -1", ", int p_is_le")
    } else {
      ("", "")
    }

    val paramsArg = Utils.join(params.map { case (p) =>
      importDataType(p.dataType)
      s"${kaitaiType2NativeType(p.dataType)} ${paramName(p.id)}"
    }, "", ", ", ", ")

    val classNameBrief = types2class(List(name.last))

    // Parameter names
    val pIo = paramName(IoIdentifier)
    val pParent = paramName(ParentIdentifier)
    val pRoot = paramName(RootIdentifier)

    // Types
    val tIo = kaitaiType2NativeType(KaitaiStreamType)
    val tParent = kaitaiType2NativeType(parentType)
    val tRoot = kaitaiType2NativeType(CalcUserType(rootClassName, None))

    // Parent type might be declared somewhere else - in this case we need to include it
    importDataType(parentType)

    outHdr.puts
    outHdr.puts(s"$classNameBrief($paramsArg" +
      s"$tIo $pIo, " +
      s"$tParent $pParent = $nullPtr, " +
      s"$tRoot $pRoot = $nullPtr$endianSuffixHdr);"
    )

    outSrc.puts
    outSrc.puts(s"${types2class(name)}::$classNameBrief($paramsArg" +
      s"$tIo $pIo, " +
      s"$tParent $pParent, " +
      s"$tRoot $pRoot$endianSuffixSrc) : $kstructName($pIo) {"
    )
    outSrc.inc

    handleAssignmentSimple(ParentIdentifier, pParent)
    handleAssignmentSimple(RootIdentifier, if (name == rootClassName) {
      s"${pRoot} ? ${pRoot} : this"
    } else {
      pRoot
    })

    typeProvider.nowClass.meta.endian match {
      case Some(_: CalcEndian) | Some(InheritedEndian) =>
        ensureMode(PrivateAccess)
        outHdr.puts("int m__is_le;")
        handleAssignmentSimple(EndianIdentifier, if (isHybrid) "p_is_le" else "-1")
        ensureMode(PublicAccess)
      case _ =>
        // no _is_le variable
    }

    // Store parameters passed to us
    params.foreach((p) => handleAssignmentSimple(p.id, paramName(p.id)))
  }

  override def classConstructorFooter: Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def classDestructorHeader(name: List[String], parentType: DataType, topClassName: List[String]): Unit = {
    ensureMode(PrivateAccess)
    outHdr.puts("void _clean_up();")
    ensureMode(PublicAccess)
    outHdr.puts(s"~${types2class(List(name.last))}();")

    outSrc.puts
    outSrc.puts(s"${types2class(name)}::~${types2class(List(name.last))}() {")
    outSrc.inc
    outSrc.puts("_clean_up();")
    outSrc.dec
    outSrc.puts("}")
    outSrc.puts
    outSrc.puts(s"void ${types2class(name)}::_clean_up() {")
    outSrc.inc
  }

  override def classDestructorFooter = classConstructorFooter

  override def runRead(name: List[String]): Unit = {
    val wrapToTryCatch = (config.cppConfig.pointers == CppRuntimeConfig.RawPointers);
    if (wrapToTryCatch) {
      outSrc.puts
      outSrc.puts("try {")
      outSrc.inc
    }
    outSrc.puts("_read();")
    if (wrapToTryCatch) {
      outSrc.dec
      outSrc.puts("} catch(...) {")
      outSrc.inc
      outSrc.puts("_clean_up();")
      outSrc.puts("throw;")
      outSrc.dec
      outSrc.puts("}")
    }
  }

  override def runReadCalc(): Unit = {
    outSrc.puts
    outSrc.puts("if (m__is_le == -1) {")
    outSrc.inc
    importListSrc.addKaitai("kaitai/exceptions.h")
    outSrc.puts(s"throw ${ksErrorName(UndecidedEndiannessError)}" +
      "(\"" + typeProvider.nowClass.path.mkString("/", "/", "") + "\");")
    outSrc.dec
    outSrc.puts("} else if (m__is_le == 1) {")
    outSrc.inc
    outSrc.puts("_read_le();")
    outSrc.dec
    outSrc.puts("} else {")
    outSrc.inc
    outSrc.puts("_read_be();")
    outSrc.dec
    outSrc.puts("}")
  }

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = {
    val suffix = endian match {
      case Some(e) => s"_${e.toSuffix}"
      case None => ""
    }

    ensureMode(if (config.autoRead) PrivateAccess else PublicAccess)

    outHdr.puts(s"void _read$suffix();")
    outSrc.puts
    outSrc.puts(s"void ${types2class(typeProvider.nowClass.name)}::_read$suffix() {")
    outSrc.inc
  }

  override def readFooter(): Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    ensureMode(PrivateAccess)
    outHdr.puts(s"${kaitaiType2NativeType(attrType)} ${privateMemberName(attrName)};")
    declareNullFlag(attrName, isNullable)
  }

  def ensureMode(newMode: AccessMode): Unit = {
    if (accessMode != newMode) {
      outHdr.dec
      outHdr.puts
      outHdr.puts(newMode match {
        case PrivateAccess => "private:"
        case PublicAccess => "public:"
      })
      outHdr.inc
      accessMode = newMode
    }
  }

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    ensureMode(PublicAccess)
    outHdr.puts(s"${kaitaiType2NativeType(attrType.asNonOwning())} ${publicMemberName(attrName)}() const { return ${nonOwningPointer(attrName, attrType)}; }")
  }

  override def universalDoc(doc: DocSpec): Unit = {
    // All docstrings would be for public stuff, so it's safe to start it here
    ensureMode(PublicAccess)

    outHdr.puts
    outHdr.puts( "/**")

    doc.summary.foreach(docStr => outHdr.putsLines(" * ", docStr))

    doc.ref.foreach {
      case TextRef(text) =>
        outHdr.putsLines(" * ", s"\\sa $text")
      case UrlRef(url, text) =>
        outHdr.putsLines(" * ", s"\\sa $url $text")
    }

    outHdr.puts( " */")
  }

  override def attrInit(attr: AttrLikeSpec): Unit = {
    attr.dataTypeComposite match {
      case _: UserType | _: ArrayTypeInStream | OwnedKaitaiStreamType =>
        // data type will be pointer to user type, std::vector or stream, so we need to init it
        outSrc.puts(s"${privateMemberName(attr.id)} = $nullPtr;")
      case _ =>
        // no init required for value types
    }
  }

  override def attrDestructor(attr: AttrLikeSpec, id: Identifier): Unit = {
    val checkLazy = if (attr.isLazy) {
      Some(calculatedFlagForName(id))
    } else {
      None
    }

    val checkNull = if (attr.isNullableSwitchRaw) {
      Some(s"!${nullFlagForName(id)}")
    } else {
      None
    }

    val checks: List[String] = List(checkLazy, checkNull).flatten

    if (checks.nonEmpty) {
      outSrc.puts(s"if (${checks.mkString(" && ")}) {")
      outSrc.inc
    }

    (ExtraAttrs.forAttr(attr, this) ++ List(attr)).foreach { (attr) =>
      val innerType = attr.dataType match {
        case st: SwitchType => combineSwitchType(st)
        case t => t
      }
      destructMember(attr.id, innerType, attr.isArray)
    }

    if (checks.nonEmpty) {
      outSrc.dec
      outSrc.puts("}")
    }
  }

  def destructMember(id: Identifier, innerType: DataType, isArray: Boolean): Unit = {
    if (config.cppConfig.pointers != CppRuntimeConfig.RawPointers)
      return

    val ptr = privateMemberName(id)
    val innerNeedsDestruct = needsDestruction(innerType)

    if (!isArray && !innerNeedsDestruct)
      return

    outSrc.puts(s"if ($ptr) {")
    outSrc.inc

    if (isArray && innerNeedsDestruct)
      destructVector(kaitaiType2NativeType(innerType), ptr)

    outSrc.puts(s"delete $ptr; $ptr = $nullPtr;")
    outSrc.dec
    outSrc.puts("}")
  }

  def needsDestruction(t: DataType): Boolean = t match {
    case _: UserType | _: ArrayTypeInStream | KaitaiStructType | AnyType | OwnedKaitaiStreamType => true
    case _ => false
  }

  /**
    * Generates std::vector contents destruction loop.
    * @param elType element type, i.e. XXX in `std::vector&lt;XXX&gt;`
    * @param arrVar variable name that holds pointer to std::vector
    */
  def destructVector(elType: String, arrVar: String): Unit = {
    outSrc.puts(s"for (std::vector<$elType>::iterator it = $arrVar->begin(); it != $arrVar->end(); ++it) {")
    outSrc.inc
    outSrc.puts("delete *it;")
    outSrc.dec
    outSrc.puts("}")
  }

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {
    outSrc.puts("if (m__is_le == 1) {")
    outSrc.inc
    leProc()
    outSrc.dec
    outSrc.puts("} else {")
    outSrc.inc
    beProc()
    outSrc.dec
    outSrc.puts("}")
  }

  override def attrFixedContentsParse(attrName: Identifier, contents: String): Unit =
    outSrc.puts(s"${privateMemberName(attrName)} = $normalIO->ensure_fixed_contents($contents);")

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
        s"$kstreamName::process_zlib($srcExpr)"
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        s"$kstreamName::process_rotate_left($srcExpr, $expr)"
      case ProcessCustom(name, args) =>
        val procClass = name.map((x) => type2class(x)).mkString("::")
        val procName = s"_process_${idToStr(varSrc)}"

        importListSrc.addLocal(outFileNameHeader(name.last))

        val argList = args.map(expression).mkString(", ")
        var argListInParens = if (argList.nonEmpty) s"($argList)" else ""
        outSrc.puts(s"$procClass $procName$argListInParens;")
        s"$procName.decode($srcExpr)"
    }
    handleAssignment(varDest, expr, rep, false)
  }

  override def allocateIO(id: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(id)
    val ioId = IoStorageIdentifier(id)

    val args = rep match {
      case RepeatUntil(_) => translator.doName(Identifier.ITERATOR2)
      case _ => getRawIdExpr(id, rep)
    }

    val newStreamRaw = s"new $kstreamName($args)"

    val ioName = rep match {
      case NoRepeat =>
        val newStream = (
          if (config.cppConfig.pointers != CppRuntimeConfig.RawPointers)
            s"${kaitaiType2NativeType(OwnedKaitaiStreamType)}($newStreamRaw)"
          else
            newStreamRaw
        )
        outSrc.puts(s"${privateMemberName(ioId)} = $newStream;")
        config.cppConfig.pointers match {
          case RawPointers =>
            privateMemberName(ioId)
          case UniqueAndRawPointers =>
            s"${privateMemberName(ioId)}.get()"
        }
      case _ =>
        val localIO = s"io_${idToStr(id)}"
        outSrc.puts(s"$kstreamName* $localIO = $newStreamRaw;")
        if (config.cppConfig.pointers == CppRuntimeConfig.UniqueAndRawPointers) {
          outSrc.puts(s"${privateMemberName(ioId)}->emplace_back($localIO);")
        } else {
          outSrc.puts(s"${privateMemberName(ioId)}->push_back($localIO);")
        }
        localIO
    }

    ioName
  }

  def getRawIdExpr(varName: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(varName)
    rep match {
      case NoRepeat => memberName
      case _ => s"$memberName->at($memberName->size() - 1)"
    }
  }

  override def useIO(ioEx: Ast.expr): String = {
    outSrc.puts(s"$kstreamName *io = ${expression(ioEx)};")
    "io"
  }

  override def pushPos(io: String): Unit =
    outSrc.puts(s"std::streampos _pos = $io->pos();")

  override def seek(io: String, pos: Ast.expr): Unit =
    outSrc.puts(s"$io->seek(${expression(pos)});")

  override def popPos(io: String): Unit =
    outSrc.puts(s"$io->seek(_pos);")

  override def alignToByte(io: String): Unit =
    outSrc.puts(s"$io->align_to_byte();")

  override def instanceClear(instName: InstanceIdentifier): Unit =
    outSrc.puts(s"${calculatedFlagForName(instName)} = false;")

  override def instanceSetCalculated(instName: InstanceIdentifier): Unit =
    outSrc.puts(s"${calculatedFlagForName(instName)} = true;")

  override def condIfSetNull(instName: Identifier): Unit =
    outSrc.puts(s"${nullFlagForName(instName)} = true;")

  override def condIfSetNonNull(instName: Identifier): Unit =
    outSrc.puts(s"${nullFlagForName(instName)} = false;")

  override def condIfHeader(expr: Ast.expr): Unit = {
    outSrc.puts(s"if (${expression(expr)}) {")
    outSrc.inc
  }

  override def condIfFooter(expr: Ast.expr): Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def condRepeatInitAttr(id: Identifier, dataType: DataType): Unit = {
    importListHdr.addSystem("vector")

    outSrc.puts(s"${privateMemberName(id)} = ${newVector(dataType)};")
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType): Unit = {
    outSrc.puts("{")
    outSrc.inc
    outSrc.puts("int i = 0;")
    outSrc.puts(s"while (!$io->is_eof()) {")
    outSrc.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    outSrc.puts(s"${privateMemberName(id)}->push_back(${stdMoveWrap(expr)});")
  }

  override def condRepeatEosFooter: Unit = {
    outSrc.puts("i++;")
    outSrc.dec
    outSrc.puts("}")
    outSrc.dec
    outSrc.puts("}")
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, repeatExpr: Ast.expr): Unit = {
    val lenVar = s"l_${idToStr(id)}"
    outSrc.puts(s"const int $lenVar = ${expression(repeatExpr)};")
    outSrc.puts(s"for (int i = 0; i < $lenVar; i++) {")
    outSrc.inc
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit =
    handleAssignmentRepeatEos(id, expr)

  override def condRepeatExprFooter: Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, untilExpr: expr): Unit = {
    outSrc.puts("{")
    outSrc.inc
    outSrc.puts("int i = 0;")
    outSrc.puts(s"${kaitaiType2NativeType(dataType.asNonOwning())} ${translator.doName("_")};")
    outSrc.puts("do {")
    outSrc.inc
  }

  private val ReStdUniquePtr = "^std::unique_ptr<(.*?)>\\((.*?)\\)$".r

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val (typeDecl, tempVar) = if (isRaw) {
      ("std::string ", translator.doName(Identifier.ITERATOR2))
    } else {
      ("", translator.doName(Identifier.ITERATOR))
    }

    val (wrappedTempVar, rawPtrExpr) = if (config.cppConfig.pointers == UniqueAndRawPointers) {
      expr match {
        case ReStdUniquePtr(cppClass, innerExpr) =>
          (s"std::move(std::unique_ptr<$cppClass>($tempVar))", innerExpr)
        case _ =>
          (tempVar, expr)
      }
    } else {
      (tempVar, expr)
    }

    outSrc.puts(s"$typeDecl$tempVar = $rawPtrExpr;")

    outSrc.puts(s"${privateMemberName(id)}->push_back($wrappedTempVar);")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, untilExpr: expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    outSrc.puts("i++;")
    outSrc.dec
    outSrc.puts(s"} while (!(${expression(untilExpr)}));")
    outSrc.dec
    outSrc.puts("}")
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    outSrc.puts(s"${privateMemberName(id)} = $expr;")
  }

  override def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit =
    outSrc.puts(s"${kaitaiType2NativeType(dataType)} $id = $expr;")

  override def blockScopeHeader: Unit = {
    outSrc.puts("{")
    outSrc.inc
  }
  override def blockScopeFooter: Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    dataType match {
      case t: ReadableType =>
        s"$io->read_${t.apiCall(defEndian)}()"
      case blt: BytesLimitType =>
        s"$io->read_bytes(${expression(blt.size)})"
      case _: BytesEosType =>
        s"$io->read_bytes_full()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        if (terminator.length == 1) {
          val term = terminator.head & 0xff
          s"$io->read_bytes_term($term, $include, $consume, $eosError)"
        } else {
          s"$io->read_bytes_term_multi(${translator.doByteArrayLiteral(terminator)}, $include, $consume, $eosError)"
        }
      case BitsType1(bitEndian) =>
        s"$io->read_bits_int_${bitEndian.toSuffix}(1)"
      case BitsType(width: Int, bitEndian) =>
        s"$io->read_bits_int_${bitEndian.toSuffix}($width)"
      case t: UserType =>
        val addParams = Utils.join(t.args.map((a) => translator.translate(a)), "", ", ", ", ")
        val addArgs = if (t.isExternal(typeProvider.nowClass)) {
          ""
        } else {
          val parent = t.forcedParent match {
            case Some(USER_TYPE_NO_PARENT) => nullPtr
            case Some(fp) => translator.translate(fp)
            case None => "this"
          }
          val addEndian = t.classSpec.get.meta.endian match {
            case Some(InheritedEndian) => ", m__is_le"
            case _ => ""
          }
          s", $parent, ${privateMemberName(RootIdentifier)}$addEndian"
        }
        config.cppConfig.pointers match {
          case RawPointers =>
            s"new ${types2class(t.name)}($addParams$io$addArgs)"
          case UniqueAndRawPointers =>
            // C++14
            //s"std::make_unique<${types2class(t.name)}>($addParams$io$addArgs)"
            s"std::unique_ptr<${types2class(t.name)}>(new ${types2class(t.name)}($addParams$io$addArgs))"
        }
    }
  }

  def newVector(elType: DataType): String = {
    val cppElType = kaitaiType2NativeType(elType)
    config.cppConfig.pointers match {
      case RawPointers =>
        s"new std::vector<$cppElType>()"
      case UniqueAndRawPointers =>
        s"std::unique_ptr<std::vector<$cppElType>>(new std::vector<$cppElType>())"
        // TODO: C++14 with std::make_unique
    }
  }

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Seq[Byte]], include: Boolean) = {
    val expr1 = padRight match {
      case Some(padByte) => s"$kstreamName::bytes_strip_right($expr0, $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) =>
        if (term.length == 1) {
          val t = term.head & 0xff
          s"$kstreamName::bytes_terminate($expr1, $t, $include)"
        } else {
          s"$kstreamName::bytes_terminate_multi($expr1, ${translator.doByteArrayLiteral(term)}, $include)"
        }
      case None => expr1
    }
    expr2
  }

  override def userTypeDebugRead(id: String, dataType: DataType, assignType: DataType): Unit = {
    val expr = if (assignType != dataType) {
      s"static_cast<${kaitaiType2NativeType(dataType)}>($id)"
    } else {
      id
    }
    outSrc.puts(s"$expr->_read();")
  }

  override def tryFinally(tryBlock: () => Unit, finallyBlock: () => Unit): Unit = {
    outSrc.puts("try {")
    outSrc.inc
    tryBlock()
    outSrc.dec
    outSrc.puts("} catch(...) {")
    outSrc.inc
    finallyBlock()
    outSrc.puts("throw;")
    outSrc.dec
    outSrc.puts("}")
    finallyBlock()
  }

  override def switchRequiresIfs(onType: DataType): Boolean = onType match {
    case _: IntType | _: EnumType => false
    case _ => true
  }

  //<editor-fold desc="switching: true version">

  override def switchStart(id: Identifier, on: Ast.expr): Unit =
    outSrc.puts(s"switch (${expression(on)}) {")

  override def switchCaseFirstStart(condition: Ast.expr): Unit = {
    outSrc.puts(s"case ${expression(condition)}: {")
    outSrc.inc
  }

  override def switchCaseStart(condition: Ast.expr): Unit = {
    outSrc.puts(s"case ${expression(condition)}: {")
    outSrc.inc
  }

  override def switchCaseEnd(): Unit = {
    outSrc.puts("break;")
    outSrc.dec
    outSrc.puts("}")
  }

  override def switchElseStart(): Unit = {
    outSrc.puts("default: {")
    outSrc.inc
  }

  override def switchEnd(): Unit =
    outSrc.puts("}")

  //</editor-fold>

  //<editor-fold desc="switching: emulation with ifs">

  override def switchIfStart(id: Identifier, on: Ast.expr, onType: DataType): Unit = {
    outSrc.puts("{")
    outSrc.inc
    outSrc.puts(s"${kaitaiType2NativeType(onType)} on = ${expression(on)};")
  }

  override def switchIfCaseFirstStart(condition: Ast.expr): Unit = {
    outSrc.puts(s"if (on == ${expression(condition)}) {")
    outSrc.inc
  }

  override def switchIfCaseStart(condition: Ast.expr): Unit = {
    outSrc.puts(s"else if (on == ${expression(condition)}) {")
    outSrc.inc
  }

  override def switchIfCaseEnd(): Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def switchIfElseStart(): Unit = {
    outSrc.puts("else {")
    outSrc.inc
  }

  override def switchIfEnd(): Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  //</editor-fold>

  override def switchBytesOnlyAsRaw = true

  override def instanceDeclaration(attrName: InstanceIdentifier, attrType: DataType, isNullable: Boolean): Unit = {
    ensureMode(PrivateAccess)
    outHdr.puts(s"bool ${calculatedFlagForName(attrName)};")
    outHdr.puts(s"${kaitaiType2NativeType(attrType)} ${privateMemberName(attrName)};")
    declareNullFlag(attrName, isNullable)
  }

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    ensureMode(PublicAccess)
    outHdr.puts(s"${kaitaiType2NativeType(dataType.asNonOwning())} ${publicMemberName(instName)}();")

    outSrc.puts
    outSrc.puts(s"${kaitaiType2NativeType(dataType.asNonOwning(), true)} ${types2class(className)}::${publicMemberName(instName)}() {")
    outSrc.inc
  }

  override def instanceFooter: Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    outSrc.puts(s"if (${calculatedFlagForName(instName)})")
    outSrc.inc
    instanceReturn(instName, dataType)
    outSrc.dec
  }

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit =
    outSrc.puts(s"return ${nonOwningPointer(instName, attrType)};")

  override def instanceCalculate(instName: Identifier, dataType: DataType, value: Ast.expr): Unit = {
    if (attrDebugNeeded(instName))
      attrDebugStart(instName, dataType, None, NoRepeat)
    val valExpr = expression(value)
    val isOwningInExpr = dataType match {
      case ct: ComplexDataType => ct.isOwningInExpr
      case _ => false
    }
    val valExprConverted = if (isOwningInExpr) {
      config.cppConfig.pointers match {
        case RawPointers =>
          valExpr
        case UniqueAndRawPointers =>
          s"$valExpr.get()"
      }
    } else {
      valExpr
    }
    handleAssignmentSimple(instName, valExprConverted)
  }

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, EnumValueSpec)]): Unit = {
    val enumClass = types2class(List(enumName))

    outHdr.puts
    outHdr.puts(s"enum $enumClass {")
    outHdr.inc

    if (enumColl.size > 1) {
      enumColl.dropRight(1).foreach { case (id, label) =>
        outHdr.puts(s"${value2Const(enumName, label.name)} = ${translator.doIntLiteral(id)},")
      }
    }
    enumColl.last match {
      case (id, label) =>
        outHdr.puts(s"${value2Const(enumName, label.name)} = ${translator.doIntLiteral(id)}")
    }

    outHdr.dec
    outHdr.puts("};")

    outHdr.puts(s"static bool _is_defined_$enumClass($enumClass v);")
    importListHdr.addSystem("set")
    val inClassRef = types2class(curClass)
    val enumClassAbs = s"$inClassRef::$enumClass"
    val valuesSetAbsRef = s"$inClassRef::_values_$enumClass"
    ensureMode(PrivateAccess)
    // NOTE: declaration and definition must be separate in this case,
    // see https://stackoverflow.com/a/12856069
    outHdr.puts(s"static const std::set<$enumClass> _values_$enumClass;")
    if (config.cppConfig.useListInitializers) {
      outSrc.puts(s"const std::set<$enumClassAbs> $valuesSetAbsRef{")
      outSrc.inc
      enumColl.foreach { case (_, label) =>
        outSrc.puts(s"$inClassRef::${value2Const(enumName, label.name)},")
      }
      outSrc.dec
      outSrc.puts("};")
    } else {
      outHdr.puts(s"static std::set<$enumClass> _build_values_$enumClass();")

      outSrc.puts(s"std::set<$enumClassAbs> $inClassRef::_build_values_$enumClass() {")
      outSrc.inc
      outSrc.puts(s"std::set<$enumClassAbs> _t;")
      enumColl.foreach { case (_, label) =>
        outSrc.puts(s"_t.insert($inClassRef::${value2Const(enumName, label.name)});")
      }
      outSrc.puts("return _t;")
      outSrc.dec
      outSrc.puts("}")
      outSrc.puts(s"const std::set<$enumClassAbs> $valuesSetAbsRef = $inClassRef::_build_values_$enumClass();")
    }
    ensureMode(PublicAccess)
    outSrc.puts(s"bool $inClassRef::_is_defined_$enumClass($enumClassAbs v) {")
    outSrc.inc
    outSrc.puts(s"return $valuesSetAbsRef.find(v) != $valuesSetAbsRef.end();")
    outSrc.dec
    outSrc.puts("}")
  }

  override def classToString(toStringExpr: Ast.expr): Unit = {
    ensureMode(PublicAccess)
    // _to_string() method
    outHdr.puts(s"std::string _to_string() const;")
    outSrc.puts
    outSrc.puts(s"std::string ${types2class(typeProvider.nowClass.name)}::_to_string() const {")
    outSrc.inc
    outSrc.puts(s"return ${translator.translate(toStringExpr)};")
    outSrc.dec
    outSrc.puts("}")

    // operator<< that trivially calls ._to_string()
    outHdr.puts(s"friend std::ostream& operator<<(std::ostream& os, const ${types2class(typeProvider.nowClass.name)}& obj);")
    outSrc.puts
    outSrc.puts(s"std::ostream& operator<<(std::ostream& os, const ${types2class(typeProvider.nowClass.name)}& obj) {")
    outSrc.inc
    outSrc.puts("os << obj._to_string();")
    outSrc.puts("return os;")
    outSrc.dec
    outSrc.puts("}")
  }

  def value2Const(enumName: String, label: String) = Utils.upperUnderscoreCase(enumName + "_" + label)

  def defineName(className: String) = Utils.upperUnderscoreCase(className) + "_H_"

  /**
    * Returns name of a member that stores "calculated flag" for a given lazy
    * attribute. That is, if it's true, then calculation have already taken
    * place and we need to return already calculated member in a getter, or,
    * if it's false, we need to calculate / parse it first.
    * @param ksName attribute ID
    * @return calculated flag member name associated with it
    */
  def calculatedFlagForName(ksName: Identifier) =
    s"f_${idToStr(ksName)}"

  /**
    * Returns name of a member that stores "null flag" for a given attribute,
    * that is, if it's true, then associated attribute is null.
    * @param ksName attribute ID
    * @return null flag member name associated with it
    */
  def nullFlagForName(ksName: Identifier) =
    s"n_${idToStr(ksName)}"

  override def idToStr(id: Identifier): String = CppCompiler.idToStr(id)

  override def publicMemberName(id: Identifier): String = idToStr(id)

  override def privateMemberName(id: Identifier): String = CppCompiler.privateMemberName(id)


  override def localTemporaryName(id: Identifier): String = s"_t_${idToStr(id)}"

  override def paramName(id: Identifier): String = s"p_${idToStr(id)}"

  def declareNullFlag(attrName: Identifier, isNullable: Boolean) = {
    if (isNullable) {
      outHdr.puts(s"bool ${nullFlagForName(attrName)};")
      ensureMode(PublicAccess)
      outHdr.puts(s"bool _is_null_${idToStr(attrName)}() { ${publicMemberName(attrName)}(); return ${nullFlagForName(attrName)}; };")
      ensureMode(PrivateAccess)
    }
  }

  override def type2class(className: String): String = CppCompiler.type2class(className)

  def kaitaiType2NativeType(attrType: DataType, absolute: Boolean = false): String =
    CppCompiler.kaitaiType2NativeType(config.cppConfig, importListHdr, attrType, absolute)

  def nullPtr: String = config.cppConfig.pointers match {
    case RawPointers => "0"
    case UniqueAndRawPointers => "nullptr"
  }

  def nonOwningPointer(attrName: Identifier, attrType: DataType): String = {
    config.cppConfig.pointers match {
      case RawPointers =>
        privateMemberName(attrName)
      case UniqueAndRawPointers =>
        attrType match {
          case st: SwitchType =>
            nonOwningPointer(attrName, combineSwitchType(st))
          case t: ComplexDataType =>
            if (t.isOwning) {
              s"${privateMemberName(attrName)}.get()"
            } else {
              privateMemberName(attrName)
            }
          case _ =>
            privateMemberName(attrName)
        }
    }
  }

  def stdMoveWrap(expr: String): String = config.cppConfig.pointers match {
    case UniqueAndRawPointers => s"std::move($expr)"
    case _ => expr
  }

  override def ksErrorName(err: KSError): String = err match {
    case EndOfStreamError => "std::ifstream::failure"
    case UndecidedEndiannessError => "kaitai::undecided_endianness_error"
    case ConversionError => "std::invalid_argument"
    case validationErr: ValidationError =>
      val cppType = kaitaiType2NativeType(validationErr.dt, true)
      val cppErrName = validationErr match {
        case _: ValidationNotEqualError => "validation_not_equal_error"
        case _: ValidationLessThanError => "validation_less_than_error"
        case _: ValidationGreaterThanError => "validation_greater_than_error"
        case _: ValidationNotAnyOfError => "validation_not_any_of_error"
        case _: ValidationNotInEnumError => "validation_not_in_enum_error"
        case _: ValidationExprError => "validation_expr_error"
      }
      s"kaitai::$cppErrName<$cppType>"
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
    val enumSpec = et.enumSpec.get
    val inClassRef = types2class(enumSpec.name.dropRight(1))
    val enumNameStr = type2class(enumSpec.name.last)
    attrValidate(s"!$inClassRef::_is_defined_$enumNameStr(${translator.translate(valueExpr)})", err, errArgs)
  }

  private def attrValidate(failCondExpr: String, err: KSError, errArgs: List[Ast.expr]): Unit = {
    val errArgsStr = errArgs.map(translator.translate).mkString(", ")
    importListSrc.addKaitai("kaitai/exceptions.h")
    outSrc.puts(s"if ($failCondExpr) {")
    outSrc.inc
    outSrc.puts(s"throw ${ksErrorName(err)}($errArgsStr);")
    outSrc.dec
    outSrc.puts("}")
  }
}

object CppCompiler extends LanguageCompilerStatic
  with StreamStructNames {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new CppCompiler(tp, config)

  def typeToFileName(topClassName: String): String = topClassName
  def outFileNameSource(className: String): String = typeToFileName(className) + ".cpp"
  def outFileNameHeader(className: String): String = typeToFileName(className) + ".h"

  def idToStr(id: Identifier): String =
    id match {
      case SpecialIdentifier(name) => Utils.lowerUnderscoreCase(name)
      case NamedIdentifier(name) => Utils.lowerUnderscoreCase(name)
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => Utils.lowerUnderscoreCase(name)
      case RawIdentifier(inner) => s"_raw_${idToStr(inner)}"
      case IoStorageIdentifier(inner) => s"_io_${idToStr(inner)}"
    }

  def privateMemberName(id: Identifier): String = s"m_${idToStr(id)}"

  override def kstructName = "kaitai::kstruct"
  override def kstreamName = "kaitai::kstream"

  def kaitaiType2NativeType(config: CppRuntimeConfig, importListHdr: CppImportList, attrType: DataType, absolute: Boolean = false): String = {
    attrType match {
      case Int1Type(false) => "uint8_t"
      case IntMultiType(false, Width2, _) => "uint16_t"
      case IntMultiType(false, Width4, _) => "uint32_t"
      case IntMultiType(false, Width8, _) => "uint64_t"

      case Int1Type(true) => "int8_t"
      case IntMultiType(true, Width2, _) => "int16_t"
      case IntMultiType(true, Width4, _) => "int32_t"
      case IntMultiType(true, Width8, _) => "int64_t"

      case FloatMultiType(Width4, _) => "float"
      case FloatMultiType(Width8, _) => "double"

      case BitsType(_, _) => "uint64_t"

      case _: BooleanType => "bool"
      case CalcIntType => "int32_t"
      case CalcFloatType => "double"

      case _: StrType => "std::string"
      case _: BytesType => "std::string"

      case t: UserType =>
        val typeStr = types2class(if (absolute) {
          t.classSpec.get.name
        } else {
          t.name
        })
        config.pointers match {
          case RawPointers => s"$typeStr*"
          case UniqueAndRawPointers =>
            if (t.isOwning) s"std::unique_ptr<$typeStr>" else s"$typeStr*"
        }

      case t: EnumType =>
        types2class(if (absolute) {
          t.enumSpec.get.name
        } else {
          t.name
        })

      case at: ArrayType => {
        importListHdr.addSystem("vector")
        val vecType = s"std::vector<${kaitaiType2NativeType(config, importListHdr, at.elType, absolute)}>"
        (at, config.pointers) match {
          case (_: ArrayTypeInStream, UniqueAndRawPointers) =>
            s"std::unique_ptr<$vecType>"
          case _ =>
            s"$vecType*"
        }
      }
      case OwnedKaitaiStreamType => config.pointers match {
        case RawPointers => s"$kstreamName*"
        case UniqueAndRawPointers => s"std::unique_ptr<$kstreamName>"
      }
      case KaitaiStreamType => s"$kstreamName*"
      case KaitaiStructType => config.pointers match {
        case RawPointers => s"$kstructName*"
        case UniqueAndRawPointers => s"std::unique_ptr<$kstructName>"
      }
      case CalcKaitaiStructType(_) => s"$kstructName*"

      case st: SwitchType =>
        kaitaiType2NativeType(config, importListHdr, combineSwitchType(st), absolute)
    }
  }

  /**
    * C++ does not have a concept of AnyType, and common use case "lots of
    * incompatible UserTypes for cases + 1 BytesType for else" combined would
    * result in exactly AnyType - so we try extra hard to avoid that here with
    * this pre-filtering. In C++, "else" case with raw byte array would
    * be available through _raw_* attribute anyway.
    *
    * @param st switch type to combine into one overall type
    * @return
    */
  def combineSwitchType(st: SwitchType): DataType = {
    val ct1 = TypeDetector.combineTypes(
      st.cases.filterNot {
        case (caseExpr, _: BytesType) => caseExpr == SwitchType.ELSE_CONST
        case _ => false
      }.values
    )
    if (st.isOwning) {
      ct1
    } else {
      ct1.asNonOwning()
    }
  }

  def types2class(typeName: Ast.typeId) = {
    typeName.names.map(type2class).mkString(
      if (typeName.absolute) "::" else "",
      "::",
      ""
    )
  }

  def types2class(components: List[String]) =
    components.map(type2class).mkString("::")

  def type2class(name: String) = Utils.lowerUnderscoreCase(name) + "_t"
}
