package io.kaitai.struct.languages

import io.kaitai.struct._
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.{ZigTranslator, TypeDetector}

import scala.collection.mutable

class ZigCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with SingleOutputFile
    with UpperCamelCaseClasses
    with ObjectOrientedLanguage
    with EveryReadIsExpression
    with UniversalFooter
    with UniversalDoc
    with AllocateIOLocalVar
    with SwitchIfOps
    with NoNeedForFullClassPath {
  import ZigCompiler._

  val translator = new ZigTranslator(typeProvider, importList, config)

  override def universalFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def indent: String = "    "
  override def outFileName(topClassName: String): String = s"$topClassName.zig"

  override def outImports(topClass: ClassSpec) =
    importList.toList.mkString("", "\n", "\n")

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"// $headerComment")
    outHeader.puts

    // Used in every class
    importList.add("""const _imp_std = @import("std");""")
    importList.add("""const _imp_kaitai_struct = @import("kaitai_struct");""")
    out.puts
  }

  override def externalTypeDeclaration(extType: ExternalType): Unit =
    ZigCompiler.externalTypeDeclaration(extType, importList)

  override def classHeader(name: String): Unit = {
    out.puts(s"pub const ${type2class(name)} = struct {")
    out.inc

    val isInheritedEndian = typeProvider.nowClass.meta.endian match {
      case Some(InheritedEndian) => true
      case _ => false
    }
  }

  override def classFooter(name: List[String]): Unit = {
    // FIXME: declaring these fields here is ugly, but doing anything better wouldn't be easy with
    // the current architecture
    out.puts("_arena: *_imp_std.heap.ArenaAllocator,")
    out.puts(s"_io: *$kstreamName,")
    typeProvider.nowClass.meta.endian match {
      case Some(InheritedEndian) =>
        out.puts("_is_le: ?bool,")
      case Some(_: CalcEndian) =>
        out.puts("_is_le: ?bool = null,")
      case _ =>
        // no _is_le variable
    }
    out.dec
    out.puts("};")
  }

  override def classConstructorHeader(name: String, parentType: DataType, rootClassName: String, isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    val paramsArg = Utils.join(params.map((p) =>
      s"${paramName(p.id)}: ${kaitaiType2NativeType(p.dataType)}"
    ), ", ", ", ", "")

    val endianSuffix = if (isHybrid) ", _is_le: ?bool" else ""

    out.puts(s"pub fn create(_arena: *_imp_std.heap.ArenaAllocator, _io: *$kstreamName, _parent: ?${kaitaiType2NativeType(parentType)}, _root: ?*${type2class(rootClassName)}$endianSuffix$paramsArg) !*${type2class(name)} {")
    out.inc
    out.puts(s"const self = try _arena.allocator().create(${type2class(name)});")
    out.puts("self.* = .{")
    out.inc
    out.puts("._arena = _arena,")
    out.puts("._io = _io,")
    out.puts("._parent = _parent,")
    if (name == rootClassName) {
      out.puts("._root = _root orelse self,")
    } else {
      out.puts("._root = _root,")
    }
    if (isHybrid) {
      out.puts("._is_le = _is_le,")
    }
    out.dec
    out.puts("};")

    // Store parameters passed to us
    params.foreach((p) => handleAssignmentSimple(p.id, paramName(p.id)))
  }

  override def classConstructorFooter: Unit = {
    out.puts("return self;")
    universalFooter
    out.puts(s"fn _allocator(self: *const ${type2class(typeProvider.nowClass.name.last)}) _imp_std.mem.Allocator {")
    out.inc
    out.puts("return self._arena.allocator();")
    universalFooter
  }

  override def runRead(name: List[String]): Unit =
    out.puts("try self._read();")

  override def runReadCalc(): Unit = {
    out.puts("if (self._is_le == true) {")
    out.inc
    out.puts("try self._readLE();")
    out.dec
    out.puts("} else if (self._is_le == false) {")
    out.inc
    out.puts("try self._readBE();")
    out.dec
    out.puts("} else {")
    out.inc
    out.puts(s"return ${ksErrorName(UndecidedEndiannessError)};")
    out.dec
    out.puts("}")
  }

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean) = {
    val className = type2class(typeProvider.nowClass.name.last)
    endian match {
      case Some(e) =>
        out.puts(s"fn _read${Utils.upperUnderscoreCase(e.toSuffix)}(self: *$className) !void {")
      case None =>
        out.puts(s"${if (!config.autoRead) "pub " else ""}fn _read(self: *$className) !void {")
    }
    out.inc
    if (isEmpty)
      out.puts("_ = self;")
  }

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    // NOTE: in Zig, we won't declare the `_raw_*` fields and use local variables instead
    if (attrName.isInstanceOf[RawIdentifier])
      return

    // At the time of writing, `_root` and `_parent` are considered non-nullable, but in reality
    // they can always be `null`.
    val isNullableCorrected =
      if (attrName == RootIdentifier || attrName == ParentIdentifier) {
        true
      } else {
        isNullable
      }
    val defaultValue =
      if (attrName == RootIdentifier || attrName == ParentIdentifier) {
        ""
      } else if (isNullable) {
        " = null"
      } else {
        " = undefined"
      }
    out.puts(s"${idToStr(attrName)}: ${kaitaiType2NativeType(attrType, isNullableCorrected)}$defaultValue,")
  }

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {}

  override def universalDoc(doc: DocSpec): Unit = {
    out.puts

    doc.summary.foreach(summary => out.putsLines("/// ", summary))
    if (doc.ref.nonEmpty) {
      if (doc.summary.isDefined) {
        out.puts("///")
      }
      out.puts("/// See also:")
      out.puts("///")
    }
    doc.ref.foreach {
      case TextRef(text) =>
        out.putsLines("/// ", s"* $text", "  ")
      case UrlRef(url, text) =>
        out.putsLines("/// ", s"* [$text]($url)", "  ")
    }
  }

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {
    out.puts("if (self._is_le.?) {")
    out.inc
    leProc()
    out.dec
    out.puts("} else {")
    out.inc
    beProc()
    out.dec
    out.puts("}")
  }

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, rep: RepeatSpec): String = {
    val srcExpr = getRawIdExpr(varSrc, rep)
    val minArgs = s"self._allocator(), $srcExpr"

    proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "processXorOne"
          case _: BytesType => "processXorMany"
        }
        val xorValueStr = translator.detectType(xorValue) match {
          case _: IntType => translator.doCast(xorValue, Int1Type(false))
          case _ => expression(xorValue)
        }
        s"try $kstreamName.$procName($minArgs, $xorValueStr)"
      case ProcessZlib =>
        s"try $kstreamName.processZlib($minArgs)"
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        s"try $kstreamName.processRotateLeft($minArgs, $expr)"
      case ProcessCustom(name, args) =>
        val namespace = name.init.mkString(".")
        val procClass = namespace +
          (if (namespace.nonEmpty) "." else "") +
          type2class(name.last)
        val procName = s"_process_${idToStr(varSrc)}"
        out.puts(s"$procClass $procName = new $procClass(${args.map(expression).mkString(", ")});")
        s"$procName.decode($minArgs)"
    }
  }

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = {
    val ioName = idToStr(IoStorageIdentifier(varName))

    val args = getRawIdExpr(varName, rep)

    out.puts(s"const $ioName = try self._allocator().create($kstreamName);")
    out.puts(s"$ioName.* = $kstreamName.fromBytes($args);")
    ioName
  }

  def getRawIdExpr(varName: Identifier, rep: RepeatSpec): String =
    idToStr(varName)

  override def useIO(ioEx: expr): String = {
    out.puts(s"const io = ${expression(ioEx)};")
    "io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"const _pos = $io.pos();")
  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"try $io.seek(${expression(pos)});")

  override def popPos(io: String): Unit =
    out.puts(s"try $io.seek(_pos);")

  override def condIfHeader(expr: expr): Unit = {
    out.puts(s"if (${expression(expr)}) {")
    out.inc
  }

  override def condRepeatInitAttr(id: Identifier, dataType: DataType): Unit = {
    // NOTE: in Zig, `_raw_*` attributes don't exist as public class members and
    // are replaced by local temporary variables. In a field with repetition,
    // this local variable will be declared inside the loop and will represent
    // one item at a time, so there is no array that needs to be initialized.
    if (id.isInstanceOf[RawIdentifier])
      return
    out.puts(s"${privateMemberName(id)} = try self._allocator().create(_imp_std.ArrayList(${kaitaiType2NativeType(dataType)}));")
    out.puts(s"${privateMemberName(id)}.* = .empty;")
  }

  override def handleAssignment(id: Identifier, expr: String, rep: RepeatSpec, isRaw: Boolean, exprType: DataType, assignType: DataType): Unit = {
    if (id.isInstanceOf[RawIdentifier]) {
      // Instead of assigning the expression to the `_raw_*` public field, we'll
      // use a local variable in Zig
      out.puts(s"const ${idToStr(id)} = $expr;")
    } else {
      super.handleAssignment(id, expr, rep, isRaw, exprType, assignType)
    }
  }

  override def castIfNeeded(expr: String, exprType: DataType, targetType: DataType): String =
    targetType match {
      case st: SwitchType if switchUsesTaggedUnion(st) =>
        val unionFieldName = dataTypeToUnionFieldName(exprType)
        s".{ .$unionFieldName = $expr }"
      case _ => expr
    }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType): Unit = {
    out.puts("{")
    out.inc
    out.puts("var i: usize = 0;")
    out.puts(s"while (!try $io.isEof()) : (i += 1) {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    out.puts(s"try ${privateMemberName(id)}.append(self._allocator(), $expr);")
  }

  override def condRepeatEosFooter: Unit = {
    out.dec
    out.puts("}")
    out.dec
    out.puts("}")
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, repeatExpr: expr): Unit = {
    out.puts(s"for (0..${expression(repeatExpr)}) |i| {")
    out.inc
    // NOTE: Zig would refuse to compile the code with an "error: unused capture" if the `i`
    // variable wasn't used in any way. In hand-written code, it's easy to deal with that by
    // inserting `_ = i;` when you know that you currently don't use `i`. The problem is that we
    // don't know whether it will be used by any expression in the loop body or not, and if you try
    // to use the `i` variable after it has been discarded as `_ = i;`, you get
    // "error: pointless discard of capture". So we use an intermediate constant just to use the `i`
    // variable somehow, which we *can* discard, because we won't be using it.
    //
    // Note that Go also doesn't allow unused variables, but the `i` variable is still accessible
    // even after `_ = i` (see GoCompiler.condRepeatExprHeader).
    blockScopeHeader
    out.puts("const _maybe_unused = i;")
    out.puts("_ = _maybe_unused;")
    blockScopeFooter
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit =
    handleAssignmentRepeatEos(id, expr)

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, untilExpr: expr): Unit = {
    out.puts("{")
    out.inc
    out.puts("var i: usize = 0;")
    out.puts("while (true) : (i += 1) {")
    out.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val tmpName = translator.doLocalName(if (isRaw) Identifier.ITERATOR2 else Identifier.ITERATOR)
    out.puts(s"const $tmpName = $expr;")
    out.puts(s"try ${privateMemberName(id)}.append(self._allocator(), $tmpName);")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, untilExpr: expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.puts(s"if (${expression(untilExpr)}) {")
    out.inc
    out.puts("break;")
    out.dec
    out.puts("}")
    out.dec
    out.puts("}")
    out.dec
    out.puts("}")
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)} = $expr;")

  override def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit =
    out.puts(s"const $id = $expr;")

  override def blockScopeHeader: Unit = {
    out.puts("{")
    out.inc
  }
  override def blockScopeFooter: Unit = universalFooter

  override def parseExpr(dataType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    val expr = dataType match {
      case t: ReadableType =>
        s"$io.read${Utils.capitalize(t.apiCall(defEndian))}()"
      case blt: BytesLimitType =>
        s"$io.readBytes(self._allocator(), ${expression(blt.size)})"
      case _: BytesEosType =>
        s"$io.readBytesFull(self._allocator())"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        if (terminator.length == 1) {
          val term = terminator.head & 0xff
          s"$io.readBytesTerm(self._allocator(), $term, $include, $consume, $eosError)"
        } else {
          s"$io.readBytesTermMulti(self._allocator(), ${translator.doByteArrayLiteral(terminator)}, $include, $consume, $eosError)"
        }
      case BitsType1(bitEndian) =>
        s"$io.readBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}(1) != 0"
      case BitsType(width: Int, bitEndian) =>
        s"$io.readBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}($width)"
      case t: UserType =>
        val (parent, root) = if (t.isExternal(typeProvider.nowClass)) {
          ("null", "null")
        } else {
          val parent = t.forcedParent match {
            case Some(USER_TYPE_NO_PARENT) => "null"
            case Some(fp) => translator.translate(fp)
            case None => "self"
          }
          (parent, "self._root")
        }
        val addEndian = t.classSpec.get.meta.endian match {
          case Some(InheritedEndian) => ", self._is_le"
          case _ => ""
        }
        val addParams = Utils.join(t.args.map((a) => translator.translate(a)), ", ", ", ", "")
        s"${types2class(t.name, t.isExternal(typeProvider.nowClass))}.create(self._arena, $io, $parent, $root$addEndian$addParams)"
    }
    s"try $expr"
  }

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Seq[Byte]], include: Boolean) = {
    val expr1 = padRight match {
      case Some(padByte) if terminator.map(term => padByte != (term.last & 0xff)).getOrElse(true) =>
        s"$kstreamName.bytesStripRight($expr0, $padByte)"
      case _ => expr0
    }
    val expr2 = terminator match {
      case Some(term) =>
        if (term.length == 1) {
          val t = term.head & 0xff
          s"$kstreamName.bytesTerminate($expr1, $t, $include)"
        } else {
          s"$kstreamName.bytesTerminateMulti($expr1, ${translator.doByteArrayLiteral(term)}, $include)"
        }
      case None => expr1
    }
    expr2
  }

  override def userTypeDebugRead(id: String, dataType: DataType, assignType: DataType): Unit = {
    val expr = if (assignType != dataType && switchUsesTaggedUnion(assignType.asInstanceOf[SwitchType])) {
      val unionFieldName = dataTypeToUnionFieldName(dataType)
      s"$id.$unionFieldName"
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
    switchStart(id, on)

    // Pass 1: only normal case clauses
    var first = true

    cases.foreach { case (condition, result) =>
      condition match {
        case SwitchType.ELSE_CONST =>
          // skip for now
        case _ =>
          if (first) {
            switchCaseFirstStart(condition)
            first = false
          } else {
            switchCaseStart(condition)
          }
          normalCaseProc(result)
          switchCaseEnd()
      }
    }

    // Pass 2: always (!) produce an `else` clause, otherwise we would get
    // "error: switch must handle all possibilities" from the Zig compiler
    switchElseStart()
    cases.get(SwitchType.ELSE_CONST).foreach { (result) =>
      elseCaseProc(result)
    }
    switchElseEnd()

    switchEnd()
  }

  override def switchRequiresIfs(onType: DataType): Boolean = onType match {
    case _: IntType | _: EnumType => false
    case _ => true
  }

  //<editor-fold desc="switching: true version">

  val NAME_SWITCH_ON = Ast.expr.Name(Ast.identifier(Identifier.SWITCH_ON))

  override def switchStart(id: Identifier, on: Ast.expr): Unit = {
    out.puts(s"switch (${expression(on)}) {")
    out.inc
  }

  override def switchCaseFirstStart(condition: Ast.expr): Unit = switchCaseStart(condition)

  override def switchCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"${expression(condition)} => {")
    out.inc
  }

  override def switchCaseEnd(): Unit = {
    out.dec
    out.puts("},")
  }

  override def switchElseStart(): Unit = {
    out.puts("else => {")
    out.inc
  }

  override def switchEnd(): Unit = universalFooter

  //</editor-fold>

  //<editor-fold desc="switching: emulation with ifs">

  override def switchIfStart(id: Identifier, on: expr, onType: DataType): Unit = {
    out.puts("{")
    out.inc
    out.puts(s"const ${expression(NAME_SWITCH_ON)} = ${expression(on)};")
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
    out.puts(s"${idToStr(attrName)}: ?${kaitaiType2NativeType(attrType, isNullable)} = null,")
  }

  override def instanceHeader(className: String, instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"pub fn ${publicMemberName(instName)}(self: *${type2class(className)}) !${kaitaiType2NativeType(dataType, isNullable)} {")
    out.inc
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    out.puts(s"if (self.${idToStr(instName)}) |_v|")
    out.inc
    out.puts("return _v;")
    out.dec
  }

  override def instanceTempVarDeclaration(dataType: DataType, isNullable: Boolean): Unit =
    out.puts(s"var _v: ${kaitaiType2NativeType(dataType)} = undefined;")

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType, isNullable: Boolean): Unit = {
    if (isNullable) {
      out.puts("if (_n) {")
      out.inc
      out.puts(s"self.${idToStr(instName)} = @as(${kaitaiType2NativeType(attrType, isNullable)}, null);")
      out.puts("return null;")
      out.dec
      out.puts("} else {")
      out.inc
    }
    out.puts(s"self.${idToStr(instName)} = _v;")
    out.puts(s"return _v;")
    if (isNullable) {
      universalFooter
    }
  }

  override def condIfSetNull(instName: Identifier): Unit = {
    if (!instName.isInstanceOf[InstanceIdentifier])
      return
    out.puts("var _n = true;")
  }

  override def condIfSetNonNull(instName: Identifier): Unit = {
    if (!instName.isInstanceOf[InstanceIdentifier])
      return
    out.puts("_n = false;")
  }

  override def enumDeclaration(curClass: String, enumName: String, enumColl: Seq[(Long, String)]): Unit = {
    val enumClass = type2class(enumName)

    out.puts(s"pub const $enumClass = enum(i32) {")
    out.inc

    enumColl.foreach { case (id, label) =>
      out.puts(s"$label = ${translator.doIntLiteral(id)},")
    }
    out.puts("_,")

    out.dec
    out.puts("};")
  }

  override def switchTaggedUnionDeclaration(attrName: Identifier, cases: Map[Ast.expr, DataType]): Unit = {
    out.puts(s"pub const ${switchTaggedUnionName(attrName)} = union(enum) {")
    out.inc

    val fieldName2TypeMap = mutable.LinkedHashMap[String, DataType]()
    cases.foreach { case (_, caseType) =>
      val unionFieldName = dataTypeToUnionFieldName(caseType)
      fieldName2TypeMap.get(unionFieldName) match {
        case Some(existingType) =>
          if (!TypeDetector.canAssign(caseType, existingType)) {
            throw new NotImplementedError(
              s"type switching in attribute ${attrName} requires avoiding " +
                "union field name collisions in Zig, which is not yet implemented"
            )
          }
        case None =>
          fieldName2TypeMap.put(unionFieldName, caseType)
      }
    }

    fieldName2TypeMap.foreach { case (fieldName, dataType) =>
      out.puts(s"$fieldName: ${kaitaiType2NativeType(dataType)},")
    }

    out.dec
    out.puts("};")
  }

  override def switchUsesTaggedUnion(st: SwitchType): Boolean = ZigCompiler.switchUsesTaggedUnion(st)

  override def classToString(toStringExpr: Ast.expr): Unit = {
    out.puts
    out.puts("@Override")
    out.puts("public String toString() {")
    out.inc
    out.puts(s"return ${translator.translate(toStringExpr)};")
    out.dec
    out.puts("}")
  }

  override def idToStr(id: Identifier): String = ZigCompiler.idToStr(id)

  override def publicMemberName(id: Identifier): String =
    id match {
      case InstanceIdentifier(name) => Utils.lowerCamelCase(name)
      case _ => idToStr(id)
    }

  override def privateMemberName(id: Identifier): String = ZigCompiler.privateMemberName(id)

  override def localTemporaryName(id: Identifier): String = s"_t_${idToStr(id)}"

  def kaitaiType2NativeType(attrType: DataType): String =
    ZigCompiler.kaitaiType2NativeType(attrType, importList, typeProvider.nowClass)

  def kaitaiType2NativeType(attrType: DataType, isNullable: Boolean): String =
    ZigCompiler.kaitaiType2NativeType(attrType, importList, typeProvider.nowClass, isNullable)

  override def ksErrorName(err: KSError): String = err match {
    case EndOfStreamError => "error.EndOfStream"
    case ConversionError => "NumberFormatException"
    case _ => s"error.${err.name}"
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
    out.puts(s"if ($failCondExpr) {")
    out.inc
    out.puts(s"return ${ksErrorName(err)};")
    out.dec
    out.puts("}")
  }
}

object ZigCompiler extends LanguageCompilerStatic
  with UpperCamelCaseClasses
  with StreamStructNames {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new ZigCompiler(tp, config)

  def idToStr(id: Identifier): String =
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => name
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => s"_m_$name"
      case RawIdentifier(innerId) => s"_raw_${idToStr(innerId)}"
      case IoStorageIdentifier(innerId) => s"_io_${idToStr(innerId)}"
    }

  def privateMemberName(id: Identifier): String =
    id match {
      case _: InstanceIdentifier => "_v"
      case _ => s"self.${idToStr(id)}"
    }

  private def numericType2NativeType(nt: NumericType): String =
    nt match {
      case Int1Type(false) => "u8"
      case IntMultiType(false, Width2, _) => "u16"
      case IntMultiType(false, Width4, _) => "u32"
      case IntMultiType(false, Width8, _) => "u64"

      case Int1Type(true) => "i8"
      case IntMultiType(true, Width2, _) => "i16"
      case IntMultiType(true, Width4, _) => "i32"
      case IntMultiType(true, Width8, _) => "i64"

      case FloatMultiType(Width4, _) => "f32"
      case FloatMultiType(Width8, _) => "f64"

      case BitsType(_, _) => "u64"

      case CalcIntType => "i32"
      case CalcFloatType => "f64"
    }

  def kaitaiType2NativeType(attrType: DataType, importList: ImportList, curClass: ClassSpec): String = {
    attrType match {
      case nt: NumericType => numericType2NativeType(nt)
      case _: BooleanType => "bool"

      case _: StrType => "[]const u8"
      case _: BytesType => "[]const u8"

      case KaitaiStreamType | OwnedKaitaiStreamType => s"*$kstreamName"
      case AnyType | KaitaiStructType | CalcKaitaiStructType(_) => "*anyopaque"

      case ut: UserType => {
        val isExternal = ut.isExternal(curClass)
        if (isExternal) {
          externalTypeDeclaration(ExternalUserType(ut.classSpec.get), importList)
        }
        s"*${types2class(ut.name, isExternal)}"
      }
      case et: EnumType => {
        val isExternal = et.isExternal(curClass)
        if (isExternal) {
          externalTypeDeclaration(ExternalEnum(et.enumSpec.get), importList)
        }
        types2class(et.name, isExternal)
      }

      case at: ArrayType => s"*_imp_std.ArrayList(${kaitaiType2NativeType(at.elType, importList, curClass)})"

      case st: SwitchType =>
        if (switchUsesTaggedUnion(st)) {
          switchTaggedUnionName(st.attrId)
        } else {
          kaitaiType2NativeType(st.combinedType, importList, curClass)
        }
    }
  }

  def kaitaiType2NativeType(attrType: DataType, importList: ImportList, curClass: ClassSpec, isNullable: Boolean): String = {
    var nonNullable = kaitaiType2NativeType(attrType, importList, curClass)
    if (isNullable) {
      s"?$nonNullable"
    } else {
      nonNullable
    }
  }

  def switchUsesTaggedUnion(st: SwitchType): Boolean = {
    st.combinedType match {
      case KaitaiStructType | CalcKaitaiStructType(_) | AnyType =>
        true
      case _ =>
        false
    }
  }

  def dataTypeToUnionFieldName(dataType: DataType): String =
    dataType match {
      case nt: NumericType => numericType2NativeType(nt)
      case _: BooleanType => "bool"
      case _: StrType => "str"
      case _: BytesType => "bytes"
      case ut: UserType => ut.name.last
      // NOTE: at the time of writing, this is unreachable because the `enum` key is not compatible
      // with type switching
      case et: EnumType => et.name.last
    }

  def switchTaggedUnionName(id: Identifier): String = {
    val baseName = id match {
      case InstanceIdentifier(name) => name
      case _ => idToStr(id)
    }
    s"${type2class(baseName)}_switch"
  }

  /** @note Same as [[PythonCompiler.types2class]] */
  def types2class(name: List[String], isExternal: Boolean): String = {
    val prefix = if (isExternal) {
      s"_imp_${name.head}."
    } else {
      ""
    }
    prefix + name.map(x => type2class(x)).mkString(".")
  }

  def externalTypeDeclaration(extType: ExternalType, importList: ImportList): Unit = {
    val moduleName = extType.name.head
    importList.add(s"""const _imp_$moduleName = @import("$moduleName.zig");""")
  }

  override def kstreamName: String = "_imp_kaitai_struct.KaitaiStream"
  override def kstructName: String = ???
}
