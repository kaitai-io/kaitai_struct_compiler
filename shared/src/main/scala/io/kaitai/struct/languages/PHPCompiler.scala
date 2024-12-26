package io.kaitai.struct.languages

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype.{CalcEndian, DataType, FixedEndian, InheritedEndian, KSError, UndecidedEndiannessError, ValidationNotInEnumError}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{NoRepeat, RepeatEos, RepeatExpr, RepeatSpec, _}
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.PHPTranslator
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig, Utils}

class PHPCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with ObjectOrientedLanguage
    with UpperCamelCaseClasses
    with SingleOutputFile
    with AllocateIOLocalVar
    with UniversalFooter
    with UniversalDoc
    with FixedContentsUsingArrayByteLiteral
    with EveryReadIsExpression {

  import PHPCompiler._

  override def innerClasses = false

  override def innerEnums = false

  override val translator: PHPTranslator = new PHPTranslator(typeProvider, config)

  override def universalFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def indent: String = "    "
  override def outFileName(topClassName: String): String = s"${type2class(topClassName)}.php"

  override def fileHeader(topClassName: String): Unit = {
    out.puts("<?php")
    out.puts(s"// $headerComment")
  }

  override def classHeader(name: List[String]): Unit =
    classHeader(name, Some(kstructName))

  def classHeader(name: List[String], parentClass: Option[String]): Unit = {
    val nsPart = name.dropRight(1)
    val ns = if (config.phpNamespace.nonEmpty) {
      if (nsPart.nonEmpty) {
        config.phpNamespace + "\\" + types2classRel(nsPart)
      } else {
        config.phpNamespace
      }
    } else if (nsPart.nonEmpty) {
      types2classRel(nsPart)
    } else {
      ""
    }
    val space = if (ns.nonEmpty) " " else ""
    out.puts
    out.puts(s"namespace $ns$space{")
    out.inc

    val ext = parentClass match {
      case Some(x) => s" extends $x"
      case None => ""
    }

    out.puts(s"class ${type2class(name.last)}$ext {")
    out.inc
  }

  override def classFooter(name: List[String]): Unit = {
    out.dec
    out.puts("}")
    out.dec
    out.puts("}")
  }

  override def classConstructorHeader(name: List[String], parentType: DataType, rootClassName: List[String], isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    typeProvider.nowClass.meta.endian match {
      case Some(_: CalcEndian) | Some(InheritedEndian) =>
        out.puts("protected $_m__is_le;")
        out.puts
      case _ =>
        // no _is_le variable
    }

    val endianAdd = if (isHybrid) ", $is_le = null" else ""

    val paramsArg = Utils.join(params.map((p) =>
      s"${kaitaiType2NativeType(p.dataType)} ${paramName(p.id)}"
    ), "", ", ", ", ")

    // Parameter names
    val pIo = paramName(IoIdentifier)
    val pParent = paramName(ParentIdentifier)
    val pRoot = paramName(RootIdentifier)

    // Types
    val tIo = kstreamName
    val tParent = kaitaiType2NativeType(parentType)
    val tRoot = translator.types2classAbs(rootClassName)

    val pRootValue = if (name == rootClassName) {
      // We could technically use the [null coalescing operator
      // (`??`)](https://www.php.net/manual/en/migration70.new-features.php#migration70.new-features.null-coalesce-op)
      // available since PHP 7 (that's OK, we don't support any lower version
      // than that), which does approximately this, but also has an additional
      // feature of suppressing the error if the left-hand side is an undefined
      // variable, which we don't need here.
      s"$pRoot === null ? $$this : $pRoot"
    } else {
      pRoot
    }

    out.puts(
      s"public function __construct($paramsArg" +
      s"$tIo $pIo, " +
      s"$tParent $pParent = null, " +
      s"$tRoot $pRoot = null" + endianAdd + ") {"
    )
    out.inc
    out.puts(s"parent::__construct($pIo, $pParent, $pRootValue);")

    if (isHybrid)
      handleAssignmentSimple(EndianIdentifier, "$is_le")

    // Store parameters passed to us
    params.foreach((p) => handleAssignmentSimple(p.id, paramName(p.id)))
  }

  override def runRead(name: List[String]): Unit =
    out.puts("$this->_read();")

  override def runReadCalc(): Unit = {
    out.puts
    out.puts("if (is_null($this->_m__is_le)) {")
    out.inc
    out.puts(s"throw new ${PHPCompiler.ksErrorName(UndecidedEndiannessError)};")
    out.dec
    out.puts("} else if ($this->_m__is_le) {")
    out.inc
    out.puts("$this->_readLE();")
    out.dec
    out.puts("} else {")
    out.inc
    out.puts("$this->_readBE();")
    out.dec
    out.puts("}")
  }

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean) = {
    val suffix = endian match {
      case Some(e) => Utils.upperUnderscoreCase(e.toSuffix)
      case None => ""
    }
    val access = if (config.autoRead) "private" else "public"

    out.puts
    out.puts(s"$access function _read$suffix() {")
    out.inc
  }

  override def readFooter(): Unit = universalFooter

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    attrName match {
      case ParentIdentifier | RootIdentifier | IoIdentifier =>
        // just ignore it for now
      case _ =>
        out.puts(s"protected $$_m_${idToStr(attrName)};")
    }
  }

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    attrName match {
      case ParentIdentifier | RootIdentifier =>
        // just ignore it for now
      case _ =>
        out.puts(s"public function ${publicMemberName(attrName)}() { return ${privateMemberName(attrName)}; }")
    }
  }

  override def universalDoc(doc: DocSpec): Unit = {
    if (doc.summary.isDefined) {
      out.puts
      out.puts("/**")
      doc.summary.foreach((summary) => out.putsLines(" * ", summary))
      out.puts(" */")
    }
  }

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {
    out.puts("if ($this->_m__is_le) {")
    out.inc
    leProc()
    out.dec
    out.puts("} else {")
    out.inc
    beProc()
    out.dec
    out.puts("}")
  }

  override def attrFixedContentsParse(attrName: Identifier, contents: String): Unit =
    out.puts(s"${privateMemberName(attrName)} = $normalIO->ensureFixedContents($contents);")

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec): Unit = {
    val srcExpr = getRawIdExpr(varSrc, rep)

    val expr = proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "processXorOne"
          case _: BytesType => "processXorMany"
        }
        s"$kstreamName::$procName($srcExpr, ${expression(xorValue)})"
      case ProcessZlib =>
        s"$kstreamName::processZlib($srcExpr)"
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        s"$kstreamName::processRotateLeft($srcExpr, $expr, 1)"
      case ProcessCustom(name, args) =>
        val isAbsolute = name.length > 1
        val procClass = name.map((x) => type2class(x)).mkString(
          if (isAbsolute) "\\" else "", "\\", ""
        )
        out.puts(s"$$_process = new $procClass(${args.map(expression).mkString(", ")});")
        s"$$_process->decode($srcExpr)"
    }
    handleAssignment(varDest, expr, rep, false)
  }

  override def allocateIO(id: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(id)
    val ioName = s"$$_io_${idToStr(id)}"

    val args = rep match {
      case RepeatUntil(_) => translator.doLocalName(Identifier.ITERATOR2)
      case _ => getRawIdExpr(id, rep)
    }

    out.puts(s"$ioName = new $kstreamName($args);")
    ioName
  }

  def getRawIdExpr(varName: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(varName)
    rep match {
      case NoRepeat => memberName
      case _ => s"end($memberName)"
    }
  }

  override def useIO(ioEx: Ast.expr): String = {
    out.puts(s"$$io = ${expression(ioEx)};")
    "$io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"$$_pos = $io->pos();")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io->seek(${expression(pos)});")

  override def popPos(io: String): Unit =
    out.puts(s"$io->seek($$_pos);")

  override def alignToByte(io: String): Unit =
    out.puts(s"$io->alignToByte();")

  override def condIfHeader(expr: Ast.expr): Unit = {
    out.puts(s"if (${expression(expr)}) {")
    out.inc
  }

  override def condRepeatInitAttr(id: Identifier, dataType: DataType): Unit =
    out.puts(s"${privateMemberName(id)} = [];")

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType): Unit = {
    out.puts("$i = 0;")
    out.puts(s"while (!$io->isEof()) {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}[] = $expr;")
  }

  override def condRepeatEosFooter: Unit = {
    out.puts("$i++;")
    super.condRepeatEosFooter
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, repeatExpr: Ast.expr): Unit = {
    out.puts(s"$$n = ${expression(repeatExpr)};")
    out.puts("for ($i = 0; $i < $n; $i++) {")
    out.inc
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit =
    handleAssignmentRepeatEos(id, expr)

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, untilExpr: Ast.expr): Unit = {
    out.puts("$i = 0;")
    out.puts("do {")
    out.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val tmpName = translator.doLocalName(if (isRaw) Identifier.ITERATOR2 else Identifier.ITERATOR)
    out.puts(s"$tmpName = $expr;")
    out.puts(s"${privateMemberName(id)}[] = $tmpName;")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, untilExpr: Ast.expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.puts("$i++;")
    out.dec
    out.puts(s"} while (!(${expression(untilExpr)}));")
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)} = $expr;")
  }

  override def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit =
    out.puts(s"$id = $expr;")

  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    dataType match {
      case t: ReadableType =>
        s"$io->read${Utils.capitalize(t.apiCall(defEndian))}()"
      case blt: BytesLimitType =>
        s"$io->readBytes(${expression(blt.size)})"
      case _: BytesEosType =>
        s"$io->readBytesFull()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        if (terminator.length == 1) {
          val term = terminator.head & 0xff
          s"$io->readBytesTerm($term, $include, $consume, $eosError)"
        } else {
          s"$io->readBytesTermMulti(${translator.doByteArrayLiteral(terminator)}, $include, $consume, $eosError)"
        }
      case BitsType1(bitEndian) =>
        s"$io->readBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}(1) != 0"
      case BitsType(width: Int, bitEndian) =>
        s"$io->readBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}($width)"
      case t: UserType =>
        val addParams = Utils.join(t.args.map((a) => translator.translate(a)), "", ", ", ", ")
        val addArgs = if (t.isExternal(typeProvider.nowClass)) {
          ""
        } else {
          val parent = t.forcedParent match {
            case Some(USER_TYPE_NO_PARENT) => "null"
            case Some(fp) => translator.translate(fp)
            case None => "$this"
          }
          val addEndian = t.classSpec.get.meta.endian match {
            case Some(InheritedEndian) => s", ${privateMemberName(EndianIdentifier)}"
            case _ => ""
          }
          s", $parent, ${privateMemberName(RootIdentifier)}$addEndian"
        }
        s"new ${translator.types2classAbs(t.classSpec.get.name)}($addParams$io$addArgs)"
    }
  }

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Seq[Byte]], include: Boolean) = {
    val expr1 = padRight match {
      case Some(padByte) => s"$kstreamName::bytesStripRight($expr0, $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) =>
        if (term.length == 1) {
          val t = term.head & 0xff
          s"$kstreamName::bytesTerminate($expr1, $t, $include)"
        } else {
          s"$kstreamName::bytesTerminateMulti($expr1, ${translator.doByteArrayLiteral(term)}, $include)"
        }
      case None => expr1
    }
    expr2
  }

  override def userTypeDebugRead(id: String, dataType: DataType, assignType: DataType): Unit =
    out.puts(s"$id->_read();")

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

  override def switchStart(id: Identifier, on: Ast.expr): Unit = {
    val onType = translator.detectType(on)

    out.puts(s"switch (${expression(on)}) {")
    out.inc
  }

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

  override def switchEnd(): Unit = universalFooter

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"public function ${idToStr(instName)}() {")
    out.inc
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    out.puts(s"if (${privateMemberName(instName)} !== null)")
    out.inc
    instanceReturn(instName, dataType)
    out.dec
  }

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit = {
    out.puts(s"return ${privateMemberName(instName)};")
  }

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, EnumValueSpec)]): Unit = {
    val name = curClass ::: List(enumName)
    classHeader(name, None)
    enumColl.foreach { case (id, label) =>
      universalDoc(label.doc)
      out.puts(s"const ${value2Const(label.name)} = ${translator.doIntLiteral(id)};")
    }
    out.puts
    val arrayEntriesStr = enumColl.map { case (id, _) => s"${translator.doIntLiteral(id)} => true" }.mkString(", ")
    out.puts(s"private const _VALUES = [$arrayEntriesStr];")
    out.puts
    out.puts("public static function isDefined(int $v): bool {")
    out.inc
    out.puts("return isset(self::_VALUES[$v]);")
    out.dec
    out.puts("}")
    classFooter(name)
  }

  override def classToString(toStringExpr: Ast.expr): Unit = {
    out.puts
    out.puts("public function __toString() {")
    out.inc
    out.puts(s"return ${translator.translate(toStringExpr)};")
    out.dec
    out.puts("}")
  }

  def value2Const(label: String) = Utils.upperUnderscoreCase(label)

  override def idToStr(id: Identifier): String = PHPCompiler.idToStr(id)

  override def publicMemberName(id: Identifier) = idToStr(id)

  override def privateMemberName(id: Identifier): String = PHPCompiler.privateMemberName(id)

  override def localTemporaryName(id: Identifier): String = s"$$_t_${idToStr(id)}"

  override def paramName(id: Identifier): String = s"$$${idToStr(id)}"

  /**
    * Determine PHP data type corresponding to a KS data type. Currently unused due to
    * problems with nullable types (which were introduced only in PHP 7.1).
    *
    * @param attrType KS data type
    * @return PHP data type
    */
  def kaitaiType2NativeType(attrType: DataType): String = {
    attrType match {
      case _: IntType => "int"
      case _: FloatType => "float"

      case _: BooleanType => "bool"

      case _: StrType | _: BytesType => "string"

      case t: UserType => translator.types2classAbs(t.classSpec match {
        case Some(cs) => cs.name
        case None => t.name
      })
      case t: EnumType => "int"

      case _: ArrayType => "array"

      case KaitaiStructType | CalcKaitaiStructType(_) => kstructName
      case KaitaiStreamType | OwnedKaitaiStreamType => kstreamName
    }
  }

  override def ksErrorName(err: KSError): String = PHPCompiler.ksErrorName(err)

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
    val enumRef = translator.types2classAbs(enumSpec.name)
    attrValidate(s"!$enumRef::isDefined(${translator.translate(valueExpr)})", err, errArgs)
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

object PHPCompiler extends LanguageCompilerStatic
  with StreamStructNames
  with UpperCamelCaseClasses
  with ExceptionNames {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new PHPCompiler(tp, config)

  def idToStr(id: Identifier): String =
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => Utils.lowerCamelCase(name)
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => Utils.lowerCamelCase(name)
      case RawIdentifier(innerId) => s"_raw_${idToStr(innerId)}"
    }

  def privateMemberName(id: Identifier): String =
    id match {
      case IoIdentifier => s"$$this->_io"
      case RootIdentifier => s"$$this->_root"
      case ParentIdentifier => s"$$this->_parent"
      case _ => s"$$this->_m_${idToStr(id)}"
    }

  override def kstreamName: String = "\\Kaitai\\Struct\\Stream"

  override def kstructName: String = "\\Kaitai\\Struct\\Struct"

  override def ksErrorName(err: KSError): String = "\\Kaitai\\Struct\\Error\\" + err.name

  def types2classRel(names: List[String]) = names.map(type2class).mkString("\\")
}
