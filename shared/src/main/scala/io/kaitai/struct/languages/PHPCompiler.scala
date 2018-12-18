package io.kaitai.struct.languages

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype.{CalcEndian, DataType, FixedEndian, InheritedEndian}
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
    val ns = if (nsPart.nonEmpty) {
      config.phpNamespace + "\\" + types2classRel(nsPart)
    } else {
      config.phpNamespace
    }
    if (ns.nonEmpty) {
      out.puts
      out.puts(s"namespace $ns;")
    }
    out.puts

    val ext = parentClass match {
      case Some(x) => s" extends $x"
      case None => ""
    }

    out.puts(s"class ${type2class(name.last)}$ext {")
    out.inc
  }

  override def classFooter(name: List[String]): Unit = universalFooter

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

    out.puts(
      s"public function __construct($paramsArg" +
      s"$tIo $pIo, " +
      s"$tParent $pParent = null, " +
      s"$tRoot $pRoot = null" + endianAdd + ") {"
    )
    out.inc
    out.puts(s"parent::__construct($pIo, $pParent, $pRoot);")

    if (isHybrid)
      handleAssignmentSimple(EndianIdentifier, "$is_le")

    // Store parameters passed to us
    params.foreach((p) => handleAssignmentSimple(p.id, paramName(p.id)))
  }

  override def runRead(): Unit =
    out.puts("$this->_read();")

  override def runReadCalc(): Unit = {
    out.puts
    out.puts("if (is_null($this->_m__is_le)) {")
    out.inc
    out.puts("throw new \\RuntimeException(\"Unable to decide on endianness\");")
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
      case Some(e) => s"${e.toSuffix.toUpperCase}"
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

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit = {
    val srcName = privateMemberName(varSrc)
    val destName = privateMemberName(varDest)

    proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "processXorOne"
          case _: BytesType => "processXorMany"
        }
        out.puts(s"$destName = $kstreamName::$procName($srcName, ${expression(xorValue)});")
      case ProcessZlib =>
        out.puts(s"$destName = $kstreamName::processZlib($srcName);")
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        out.puts(s"$destName = $kstreamName::processRotateLeft($srcName, $expr, 1);")
      case ProcessCustom(name, args) =>
        val isAbsolute = name.length > 1
        val procClass = name.map((x) => type2class(x)).mkString(
          if (isAbsolute) "\\" else "", "\\", ""
        )
        out.puts(s"$$_process = new $procClass(${args.map(expression).mkString(", ")});")
        out.puts(s"$destName = $$_process->decode($srcName);")
    }
  }

  override def allocateIO(id: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(id)

    val args = rep match {
      case RepeatEos | RepeatExpr(_) => s"end($memberName)"
      case RepeatUntil(_) => translator.doLocalName(Identifier.ITERATOR2)
      case NoRepeat => memberName
    }

    out.puts(s"$$io = new $kstreamName($args);")
    "$io"
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

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = [];")
    out.puts(s"${privateMemberName(id)} = [];")
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

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: Ast.expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = [];")
    out.puts(s"${privateMemberName(id)} = [];")
    out.puts(s"$$n = ${expression(repeatExpr)};")
    out.puts("for ($i = 0; $i < $n; $i++) {")
    out.inc
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}[] = $expr;")
  }

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, untilExpr: Ast.expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = [];")
    out.puts(s"${privateMemberName(id)} = [];")
    out.puts("$i = 0;")
    out.puts("do {")
    out.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val tmpName = translator.doLocalName(if (isRaw) Identifier.ITERATOR2 else Identifier.ITERATOR)
    out.puts(s"$tmpName = $expr;")
    out.puts(s"${privateMemberName(id)}[] = $tmpName;")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, untilExpr: Ast.expr): Unit = {
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
        s"$io->readBytesTerm($terminator, $include, $consume, $eosError)"
      case BitsType1 =>
        s"$io->readBitsInt(1) != 0"
      case BitsType(width: Int) =>
        s"$io->readBitsInt($width)"
      case t: UserType =>
        val addParams = Utils.join(t.args.map((a) => translator.translate(a)), "", ", ", ", ")
        val addArgs = if (t.isOpaque) {
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

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean) = {
    val expr1 = padRight match {
      case Some(padByte) => s"$kstreamName::bytesStripRight($expr0, $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) => s"$kstreamName::bytesTerminate($expr1, $term, $include)"
      case None => expr1
    }
    expr2
  }

  override def userTypeDebugRead(id: String): Unit =
    out.puts(s"$id->_read();")

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
    classHeader(curClass ::: List(enumName), None)
    enumColl.foreach { case (id, label) =>
      universalDoc(label.doc)
      out.puts(s"const ${value2Const(label.name)} = $id;")
    }
    universalFooter
  }

  def value2Const(label: String) = label.toUpperCase

  def idToStr(id: Identifier): String = {
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => Utils.lowerCamelCase(name)
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => Utils.lowerCamelCase(name)
      case RawIdentifier(innerId) => "_raw_" + idToStr(innerId)
    }
  }

  override def privateMemberName(id: Identifier): String = {
    id match {
      case IoIdentifier => s"$$this->_io"
      case RootIdentifier => s"$$this->_root"
      case ParentIdentifier => s"$$this->_parent"
      case _ => s"$$this->_m_${idToStr(id)}"
    }
  }

  override def publicMemberName(id: Identifier) = idToStr(id)

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

      case ArrayType(_) => "array"

      case KaitaiStructType | CalcKaitaiStructType => kstructName
      case KaitaiStreamType => kstreamName
    }
  }
}

object PHPCompiler extends LanguageCompilerStatic
  with StreamStructNames
  with UpperCamelCaseClasses {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new PHPCompiler(tp, config)

  override def kstreamName: String = "\\Kaitai\\Struct\\Stream"

  override def kstructName: String = "\\Kaitai\\Struct\\Struct"

  def types2classRel(names: List[String]) = names.map(type2class).mkString("\\")
}
