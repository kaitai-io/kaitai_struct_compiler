package io.kaitai.struct.languages

import io.kaitai.struct._
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype.{CalcEndian, DataType, FixedEndian, InheritedEndian}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.{JavaTranslator, TypeDetector}

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
    with NoNeedForFullClassPath {
  import JavaCompiler._

  val translator = new JavaTranslator(typeProvider, importList)

  // Preprocess fromFileClass and make import
  val fromFileClass = {
    val pos = config.javaFromFileClass.lastIndexOf('.')
    if (pos < 0) {
      config.javaFromFileClass
    } else {
      importList.add(config.javaFromFileClass)
      config.javaFromFileClass.substring(pos + 1)
    }
  }

  override def universalFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def indent: String = "    "
  override def outFileName(topClassName: String): String =
    s"src/${config.javaPackage.replace('.', '/')}/${type2class(topClassName)}.java"

  override def outImports(topClass: ClassSpec) =
    "\n" + importList.toList.map((x) => s"import $x;").mkString("\n") + "\n"

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"// $headerComment")
    if (!config.javaPackage.isEmpty) {
      outHeader.puts
      outHeader.puts(s"package ${config.javaPackage};")
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

    if (debug) {
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
    if (!isInheritedEndian && !config.javaFromFileClass.isEmpty && typeProvider.nowClass.params.isEmpty) {
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

  override def runRead(): Unit =
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
    val readAccessAndType = if (debug) {
      "public"
    } else {
      "private"
    }
    val suffix = endian match {
      case Some(e) => s"${e.toSuffix.toUpperCase}"
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

    doc.summary.foreach((summary) => out.putsLines(" * ", summary))

    doc.ref match {
      case TextRef(text) =>
        out.putsLines(" * ", "@see \"" + text + "\"")
      case ref: UrlRef =>
        out.putsLines(" * ", s"@see ${ref.toAhref}")
      case NoRef =>
        // no reference => output nothing
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

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit = {
    val srcName = privateMemberName(varSrc)
    val destName = privateMemberName(varDest)

    proc match {
      case ProcessXor(xorValue) =>
        out.puts(s"$destName = $kstreamName.processXor($srcName, ${expression(xorValue)});")
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
        val namespace = name.init.mkString(".")
        val procClass = namespace +
          (if (namespace.nonEmpty) "." else "") +
          type2class(name.last)
        val procName = s"_process_${idToStr(varSrc)}"
        out.puts(s"$procClass $procName = new $procClass(${args.map(expression).mkString(", ")});")
        out.puts(s"$destName = $procName.decode($srcName);")
    }
  }

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = {
    val javaName = idToStr(varName)

    val ioName = s"_io_$javaName"

    val args = rep match {
      case RepeatEos | RepeatExpr(_) => s"$javaName.get($javaName.size() - 1)"
      case RepeatUntil(_) => translator.doName(Identifier.ITERATOR2)
      case NoRepeat => javaName
    }

    importList.add("io.kaitai.struct.ByteBufferKaitaiStream")
    out.puts(s"$kstreamName $ioName = new ByteBufferKaitaiStream($args);")
    ioName
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
      val name = attrId match {
        case _: RawIdentifier | _: SpecialIdentifier => return
        case _ => idToStr(attrId)
      }
      rep match {
        case NoRepeat =>
          out.puts("_attrStart.put(\"" + name + "\", " + io + ".pos());")
        case _: RepeatExpr | RepeatEos | _: RepeatUntil =>
          getOrCreatePosList("_arrStart", name, io)
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

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = new ArrayList<byte[]>();")
    out.puts(s"${privateMemberName(id)} = new ${kaitaiType2JavaType(ArrayType(dataType))}();")
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

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = new ArrayList<byte[]>((int) (${expression(repeatExpr)}));")
    out.puts(s"${idToStr(id)} = new ${kaitaiType2JavaType(ArrayType(dataType))}((int) (${expression(repeatExpr)}));")
    out.puts(s"for (int i = 0; i < ${expression(repeatExpr)}; i++) {")
    out.inc

    importList.add("java.util.ArrayList")
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}.add($expr);")
  }

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, untilExpr: expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = new ArrayList<byte[]>();")
    out.puts(s"${privateMemberName(id)} = new ${kaitaiType2JavaType(ArrayType(dataType))}();")
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

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, untilExpr: expr): Unit = {
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

  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    val expr = dataType match {
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
        val addArgs = if (t.isOpaque) {
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

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean) = {
    val expr1 = padRight match {
      case Some(padByte) => s"$kstreamName.bytesStripRight($expr0, (byte) $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) => s"$kstreamName.bytesTerminate($expr1, (byte) $term, $include)"
      case None => expr1
    }
    expr2
  }

  override def userTypeDebugRead(id: String): Unit =
    out.puts(s"$id._read();")

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
      case _: IntType | _: EnumType | _: StrType => false
      case _ => true
    }

    if (switchIfs) {
      out.puts("{")
      out.inc
      out.puts(s"${kaitaiType2JavaType(onType)} ${expression(NAME_SWITCH_ON)} = ${expression(on)};")
    } else {
      out.puts(s"switch (${expression(on)}) {")
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
      // Java is very specific about what can be used as "condition" in "case
      // condition:".
      val condStr = condition match {
        case Ast.expr.EnumByLabel(_, enumVal) =>
          // If switch is over a enum, only literal enum values are supported,
          // and they must be written as "MEMBER", not "SomeEnum.MEMBER".
          value2Const(enumVal.name)
        case _ =>
          expression(condition)
      }

      out.puts(s"case $condStr: {")
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
    if (switchIfs)
      out.dec
    out.puts("}")
  }

  override def instanceDeclaration(attrName: InstanceIdentifier, attrType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"private ${kaitaiType2JavaTypeBoxed(attrType)} ${idToStr(attrName)};")
  }

  override def instanceHeader(className: String, instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"public ${kaitaiType2JavaTypeBoxed(dataType)} ${idToStr(instName)}() {")
    out.inc
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier): Unit = {
    out.puts(s"if (${privateMemberName(instName)} != null)")
    out.inc
    instanceReturn(instName)
    out.dec
  }

  override def instanceReturn(instName: InstanceIdentifier): Unit = {
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
        out.puts(s"${value2Const(label)}($id),")
      }
    }
    enumColl.last match {
      case (id, label) =>
        out.puts(s"${value2Const(label)}($id);")
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

  def value2Const(s: String) = s.toUpperCase

  def idToStr(id: Identifier): String = {
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => Utils.lowerCamelCase(name)
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => Utils.lowerCamelCase(name)
      case RawIdentifier(innerId) => "_raw_" + idToStr(innerId)
    }
  }

  override def privateMemberName(id: Identifier): String = s"this.${idToStr(id)}"

  override def publicMemberName(id: Identifier) = idToStr(id)

  override def localTemporaryName(id: Identifier): String = s"_t_${idToStr(id)}"
}

object JavaCompiler extends LanguageCompilerStatic
  with UpperCamelCaseClasses
  with StreamStructNames {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new JavaCompiler(tp, config)

  def kaitaiType2JavaType(attrType: DataType): String = kaitaiType2JavaTypePrim(attrType)

  def kaitaiType2JavaType(attrType: DataType, isNullable: Boolean): String =
    if (isNullable) {
      kaitaiType2JavaTypeBoxed(attrType)
    } else {
      kaitaiType2JavaTypePrim(attrType)
    }

  /**
    * Determine Java data type corresponding to a KS data type. A "primitive" type (i.e. "int", "long", etc) will
    * be returned if possible.
    *
    * @param attrType KS data type
    * @return Java data type
    */
  def kaitaiType2JavaTypePrim(attrType: DataType): String = {
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

      case BitsType(_) => "long"

      case _: BooleanType => "boolean"
      case CalcIntType => "int"
      case CalcFloatType => "double"

      case _: StrType => "String"
      case _: BytesType => "byte[]"

      case AnyType => "Object"
      case KaitaiStreamType => kstreamName
      case KaitaiStructType => kstructName

      case t: UserType => types2class(t.name)
      case EnumType(name, _) => types2class(name)

      case ArrayType(_) => kaitaiType2JavaTypeBoxed(attrType)

      case st: SwitchType => kaitaiType2JavaTypePrim(st.combinedType)
    }
  }

  /**
    * Determine Java data type corresponding to a KS data type. A non-primitive type (i.e. "Integer", "Long", etc) will
    * be returned, to be used when proper objects should be used.
    *
    * @param attrType KS data type
    * @return Java data type
    */
  def kaitaiType2JavaTypeBoxed(attrType: DataType): String = {
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

      case BitsType(_) => "Long"

      case _: BooleanType => "Boolean"
      case CalcIntType => "Integer"
      case CalcFloatType => "Double"

      case _: StrType => "String"
      case _: BytesType => "byte[]"

      case AnyType => "Object"
      case KaitaiStreamType => kstreamName
      case KaitaiStructType => kstructName

      case t: UserType => types2class(t.name)
      case EnumType(name, _) => types2class(name)

      case ArrayType(inType) => s"ArrayList<${kaitaiType2JavaTypeBoxed(inType)}>"

      case SwitchType(_, cases) => kaitaiType2JavaTypeBoxed(TypeDetector.combineTypes(cases.values))
    }
  }

  def types2class(names: List[String]) = names.map(x => type2class(x)).mkString(".")

  override def kstreamName: String = "KaitaiStream"
  override def kstructName: String = "KaitaiStruct"
}
