package io.kaitai.struct.languages

import io.kaitai.struct.datatype.{DataType, FixedEndian, InheritedEndian, KSError}
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components.{ExceptionNames, _}
import io.kaitai.struct.translators.{PerlTranslator, TypeProvider}
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig, Utils}
import io.kaitai.struct.ExternalType

class PerlCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with SingleOutputFile
    with UniversalFooter
    with UpperCamelCaseClasses
    with AllocateIOLocalVar
    with FixedContentsUsingArrayByteLiteral
    with SwitchIfOps
    with EveryReadIsExpression {

  import PerlCompiler._

  override val translator = new PerlTranslator(typeProvider, importList)

  override def innerClasses = false

  override def universalFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def indent: String = "    "
  override def outFileName(topClassName: String): String = s"${type2class(topClassName)}.pm"

  override def outImports(topClass: ClassSpec) =
    importList.toList.map((x) => s"use $x;").mkString("", "\n", "\n")

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"# $headerComment")
    outHeader.puts

    importList.add("strict")
    importList.add("warnings")
    importList.add(s"$packageName ${KSVersion.minimalRuntime.toPerlVersion}")
  }

  override def fileFooter(topClassName: String): Unit = {
    out.puts
    out.puts("1;")
  }

  override def externalTypeDeclaration(extType: ExternalType): Unit =
    importList.add(type2class(extType.name.head))

  override def classHeader(name: List[String]): Unit = {
    out.puts
    out.puts("########################################################################")
    out.puts(s"package ${types2class(name)};")
    out.puts
    out.puts(s"our @ISA = '$kstructName';")
    out.puts
    out.puts("sub from_file {")
    out.inc
    out.puts("my ($class, $filename) = @_;")
    out.puts("my $fd;")
    out.puts
    out.puts("open($fd, '<', $filename) or return undef;")
    out.puts("binmode($fd);")
    out.puts(s"return new($$class, $kstreamName->new($$fd));")
    universalFooter
  }

  override def classFooter(name: List[String]): Unit = {}

  override def classConstructorHeader(name: List[String], parentType: DataType, rootClassName: List[String], isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    val endianSuffix = if (isHybrid) ", $_is_le" else ""

    val pRootValue = if (name == rootClassName) {
      "$_root || $self"
    } else {
      "$_root"
    }

    out.puts
    out.puts("sub new {")
    out.inc
    out.puts("my ($class, $_io, $_parent, $_root" + endianSuffix + ") = @_;")
    out.puts(s"my $$self = $kstructName->new($$_io);")
    out.puts
    out.puts("bless $self, $class;")
    handleAssignmentSimple(ParentIdentifier, "$_parent")
    handleAssignmentSimple(RootIdentifier, pRootValue)

    if (isHybrid)
      handleAssignmentSimple(EndianIdentifier, "$_is_le")

    out.puts
  }

  override def classConstructorFooter: Unit = {
    out.puts
    out.puts("return $self;")
    universalFooter
  }

  override def runRead(name: List[String]): Unit =
    out.puts("$self->_read();")

  override def runReadCalc(): Unit = {
    val isLe = privateMemberName(EndianIdentifier)

    out.puts(s"if (!(defined $isLe)) {")
    out.inc
    out.puts("die \"Unable to decide on endianness\";")
    out.dec
    out.puts(s"} elsif ($isLe) {")
    out.inc
    out.puts("$self->_read_le();")
    out.dec
    out.puts("} else {")
    out.inc
    out.puts("$self->_read_be();")
    out.dec
    out.puts("}")
  }

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = {
    val suffix = endian match {
      case Some(e) => s"_${e.toSuffix}"
      case None => ""
    }
    out.puts
    out.puts(s"sub _read$suffix {")
    out.inc
    out.puts("my ($self) = @_;")
    out.puts
  }

  override def readFooter(): Unit = universalFooter

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {}

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    attrName match {
      case RootIdentifier | ParentIdentifier =>
        // ignore, they are already defined in KaitaiStruct class
      case _ =>
        out.puts
        out.puts(s"sub ${publicMemberName(attrName)} {")
        out.inc

        out.puts("my ($self) = @_;")
        out.puts(s"return ${privateMemberName(attrName)};")

        universalFooter
    }
  }

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {
    out.puts(s"if (${privateMemberName(EndianIdentifier)}) {")
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
    out.puts(s"${privateMemberName(attrName)} = $normalIO->ensure_fixed_contents($contents);")
  }

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
        importList.add("Compress::Zlib")
        s"Compress::Zlib::uncompress($srcExpr)"
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        s"$kstreamName::process_rotate_left($srcExpr, $expr, 1)"
    }
    handleAssignment(varDest, expr, rep, false)
  }

  override def allocateIO(id: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(id)

    val args = rep match {
      case RepeatUntil(_) => translator.doName(Identifier.ITERATOR2)
      case _ => getRawIdExpr(id, rep)
    }

    val ioName = s"$$io_${idToStr(id)}"

    out.puts(s"my $ioName = $kstreamName->new($args);")
    ioName
  }

  def getRawIdExpr(varName: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(varName)
    rep match {
      case NoRepeat => memberName
      case RepeatExpr(_) => s"$memberName[$$i]"
      case _ => s"$memberName[-1]"
    }
  }

  override def useIO(ioEx: expr): String = {
    out.puts(s"my $$io = ${expression(ioEx)};")
    "$io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"my $$_pos = $io->pos();")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io->seek(${expression(pos)});")

  override def popPos(io: String): Unit =
    out.puts(s"$io->seek($$_pos);")

  override def alignToByte(io: String): Unit =
    out.puts(s"$io->align_to_byte();")

  override def condIfHeader(expr: Ast.expr): Unit = {
    out.puts(s"if (${expression(expr)}) {")
    out.inc
  }

  override def condRepeatInitAttr(id: Identifier, dataType: DataType): Unit =
    out.puts(s"${privateMemberName(id)} = [];")

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType): Unit = {
    out.puts(s"while (!$io->is_eof()) {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit =
    out.puts(s"push @{${privateMemberName(id)}}, $expr;")

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, repeatExpr: expr): Unit = {
    val nVar = s"$$n_${idToStr(id)}"
    out.puts(s"my $nVar = ${expression(repeatExpr)};")
    out.puts(s"for (my $$i = 0; $$i < $nVar; $$i++) {")
    out.inc
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit =
    handleAssignmentRepeatEos(id, expr)

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, untilExpr: expr): Unit = {
    blockScopeHeader
    out.puts(s"my ${translator.doName("_")};")
    out.puts("do {")
    out.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val (decl, tmpName) = if (isRaw) {
      ("my ", translator.doName(Identifier.ITERATOR2))
    } else {
      ("", translator.doName(Identifier.ITERATOR))
    }
    out.puts(s"$decl$tmpName = $expr;")
    out.puts(s"push @{${privateMemberName(id)}}, $tmpName;")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, untilExpr: expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.dec
    out.puts(s"} until (${expression(untilExpr)});")
    blockScopeFooter
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)} = $expr;")

  override def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit =
    out.puts(s"my $id = $expr;")

  override def blockScopeHeader: Unit = {
    out.puts("{")
    out.inc
  }
  override def blockScopeFooter: Unit = universalFooter

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
          s"$io->read_bytes_term($term, ${boolLiteral(include)}, ${boolLiteral(consume)}, ${boolLiteral(eosError)})"
        } else {
          s"$io->read_bytes_term_multi(${translator.doByteArrayLiteral(terminator)}, ${boolLiteral(include)}, ${boolLiteral(consume)}, ${boolLiteral(eosError)})"
        }
      case BitsType1(bitEndian) =>
        s"$io->read_bits_int_${bitEndian.toSuffix}(1)"
      case BitsType(width: Int, bitEndian) =>
        s"$io->read_bits_int_${bitEndian.toSuffix}($width)"
      case t: UserType =>
        val addArgs = if (t.isExternal(typeProvider.nowClass)) {
          ""
        } else {
          val parent = t.forcedParent match {
            case Some(fp) => translator.translate(fp)
            case None => "$self"
          }
          val addEndian = t.classSpec.get.meta.endian match {
            case Some(InheritedEndian) => s", ${privateMemberName(EndianIdentifier)}"
            case _ => ""
          }
          s", $parent, ${privateMemberName(RootIdentifier)}$addEndian"
        }
        s"${types2class(t.classSpec.get.name)}->new($io$addArgs)"
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
          s"$kstreamName::bytes_terminate($expr1, $t, ${boolLiteral(include)})"
        } else {
          s"$kstreamName::bytes_terminate_multi($expr1, ${translator.doByteArrayLiteral(term)}, ${boolLiteral(include)})"
        }
      case None => expr1
    }
    expr2
  }

  override def userTypeDebugRead(id: String, dataType: DataType, assignType: DataType): Unit =
    out.puts(s"$id->_read();")

  override def tryFinally(tryBlock: () => Unit, finallyBlock: () => Unit): Unit = {
    out.puts("my ($failed, $err);")
    out.puts("eval {")
    out.inc
    tryBlock()
    out.puts("1;")
    out.dec
    out.puts("} or do {")
    out.inc
    out.puts("$failed = 1;")
    out.puts("$err = $@;")
    out.dec
    out.puts("};")
    finallyBlock()
    out.puts("if ($failed) {")
    out.inc
    out.puts("die $err;")
    out.dec
    out.puts("}")
  }

  override def switchStart(id: Identifier, on: Ast.expr): Unit = {}
  override def switchCaseStart(condition: Ast.expr): Unit = {}
  override def switchCaseEnd(): Unit = {}
  override def switchElseStart(): Unit = {}
  override def switchEnd(): Unit = {}

  override def switchRequiresIfs(onType: DataType): Boolean = true

  override def switchIfStart(id: Identifier, on: Ast.expr, onType: DataType): Unit = {
    typeProvider._currentSwitchType = Some(translator.detectType(on))
    out.puts(s"my $$_on = ${expression(on)};")
  }

  override def switchIfCaseFirstStart(condition: Ast.expr): Unit = {
    out.puts(s"if (${expression(onComparisonExpr(condition))}) {")
    out.inc
  }

  override def switchIfCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"elsif (${expression(onComparisonExpr(condition))}) {")
    out.inc
  }
  override def switchIfCaseEnd(): Unit = universalFooter

  override def switchIfElseStart(): Unit = {
    out.puts(s"else {")
    out.inc
  }

  override def switchIfEnd(): Unit = {}

  /**
    * Generates comparison expression by a given condition expression, comparing
    * it to special local variable "_on".
    * @param condition condition to check for equality with "_on"
    * @return comparison expression of boolean type
    */
  def onComparisonExpr(condition: Ast.expr) =
    Ast.expr.Compare(Ast.expr.Name(Ast.identifier("_on")), Ast.cmpop.Eq, condition)

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    out.puts
    out.puts(s"sub ${instName.name} {")
    out.inc
    out.puts("my ($self) = @_;")
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    out.puts(s"return ${privateMemberName(instName)} if (${privateMemberName(instName)});")
  }

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit = {
    out.puts(s"return ${privateMemberName(instName)};")
  }

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, EnumValueSpec)]): Unit = {
    out.puts

    enumColl.foreach { case (id, label) =>
      out.puts(s"our ${enumValue(enumName, label.name)} = ${translator.doIntLiteral(id)};")
    }
  }

  override def classToString(toStringExpr: Ast.expr): Unit = {
    out.puts
    out.puts("use overload '\"\"' => \\&_to_string;")
    out.puts
    out.puts("sub _to_string {")
    out.inc
    out.puts("my ($self) = @_;")
    out.puts(s"return ${translator.translate(toStringExpr)}")
    out.dec
    out.puts("}")
  }

  override def idToStr(id: Identifier): String = PerlCompiler.idToStr(id)

  override def publicMemberName(id: Identifier): String = idToStr(id)

  override def privateMemberName(id: Identifier): String = PerlCompiler.privateMemberName(id)

  override def localTemporaryName(id: Identifier): String = s"$$_t_${idToStr(id)}"

  def boolLiteral(b: Boolean): String = translator.doBoolLiteral(b)

  override def ksErrorName(err: KSError): String = PerlCompiler.ksErrorName(err)
}

object PerlCompiler extends LanguageCompilerStatic
  with UpperCamelCaseClasses
  with StreamStructNames
  with ExceptionNames {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new PerlCompiler(tp, config)

  def idToStr(id: Identifier): String =
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => name
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => name
      case RawIdentifier(inner) => s"_raw_${idToStr(inner)}"
    }

  def privateMemberName(id: Identifier): String = s"$$self->{${idToStr(id)}}"

  def packageName: String = "IO::KaitaiStruct"
  override def kstreamName: String = s"$packageName::Stream"
  override def kstructName: String = s"$packageName::Struct"
  override def ksErrorName(err: KSError): String = ???

  def types2class(t: List[String]): String = t.map(type2class).mkString("::")

  def enumValue(enumName: String, label: String): String =
    s"$$${Utils.upperUnderscoreCase(enumName)}_${Utils.upperUnderscoreCase(label)}"
}
