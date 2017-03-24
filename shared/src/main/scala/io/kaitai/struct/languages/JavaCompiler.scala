package io.kaitai.struct.languages

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.{JavaTranslator, TypeDetector, TypeProvider}
import io.kaitai.struct._

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

  override def getStatic = JavaCompiler

  override def universalFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def indent: String = "    "
  override def outFileName(topClassName: String): String =
    s"src/${config.javaPackage.replace('.', '/')}/${type2class(topClassName)}.java"

  override def fileHeader(topClassName: String): Unit = {
    out.puts(s"// $headerComment")
    if (!config.javaPackage.isEmpty) {
      out.puts
      out.puts(s"package ${config.javaPackage};")
    }
    out.puts
    out.puts(s"import io.kaitai.struct.$kstructName;")
    out.puts(s"import io.kaitai.struct.$kstreamName;")
    out.puts
    out.puts("import java.io.IOException;")
    out.puts("import java.util.Arrays;")
    out.puts("import java.util.ArrayList;")
    out.puts("import java.util.HashMap;")
    out.puts("import java.util.Map;")
    out.puts("import java.nio.charset.Charset;")

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
    }

    out.puts(s"public static ${type2class(name)} fromFile(String fileName) throws IOException {")
    out.inc
    out.puts(s"return new ${type2class(name)}(new $kstreamName(fileName));")
    out.dec
    out.puts("}")
  }

  override def classConstructorHeader(name: String, parentClassName: String, rootClassName: String): Unit = {
    out.puts
    out.puts(s"public ${type2class(name)}($kstreamName _io) {")
    out.inc
    out.puts("super(_io);")
    if (name == rootClassName)
      out.puts("this._root = this;")
    if (!debug)
      out.puts("_read();")
    out.dec
    out.puts("}")

    out.puts
    out.puts(s"public ${type2class(name)}($kstreamName _io, ${type2class(parentClassName)} _parent) {")
    out.inc
    out.puts("super(_io);")
    out.puts("this._parent = _parent;")
    if (name == rootClassName)
      out.puts("this._root = this;")
    if (!debug)
      out.puts("_read();")
    out.dec
    out.puts("}")

    out.puts
    out.puts(s"public ${type2class(name)}($kstreamName _io, ${type2class(parentClassName)} _parent, ${type2class(rootClassName)} _root) {")
    out.inc
    out.puts("super(_io);")
    out.puts("this._parent = _parent;")
    out.puts("this._root = _root;")
    if (!debug)
      out.puts("_read();")
    out.dec
    out.puts("}")

    val readAccessAndType = if (debug) {
      "public"
    } else {
      "private"
    }
    out.puts(s"$readAccessAndType void _read() {")
    out.inc
  }

  override def classConstructorFooter: Unit = {
    universalFooter
  }

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, condSpec: ConditionalSpec): Unit = {
    out.puts(s"private ${kaitaiType2JavaType(attrType, condSpec)} ${idToStr(attrName)};")
  }

  override def attributeReader(attrName: Identifier, attrType: DataType, condSpec: ConditionalSpec): Unit = {
    out.puts(s"public ${kaitaiType2JavaType(attrType, condSpec)} ${idToStr(attrName)}() { return ${idToStr(attrName)}; }")
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

    out.puts(s"$kstreamName $ioName = new $kstreamName($args);")
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
    out.puts(s"while (!$io.isEof()) {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}.add($expr);")
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = new ArrayList<byte[]>(Long.valueOf(${expression(repeatExpr)}).intValue());")
    out.puts(s"${idToStr(id)} = new ${kaitaiType2JavaType(ArrayType(dataType))}(Long.valueOf(${expression(repeatExpr)}).intValue());")
    out.puts(s"for (int i = 0; i < ${expression(repeatExpr)}; i++) {")
    out.inc
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

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, untilExpr: expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.dec
    out.puts(s"} while (!(${expression(untilExpr)}));")
    out.dec
    out.puts("}")
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)} = $expr;")

  override def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit =
    out.puts(s"${kaitaiType2JavaType(dataType)} $id = $expr;")

  override def parseExpr(dataType: DataType, io: String): String = {
    dataType match {
      case t: ReadableType =>
        s"$io.read${Utils.capitalize(t.apiCall)}()"
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
          s", $parent, _root"
        }
        s"new ${types2class(t.name)}($io$addArgs)"
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

  override def switchStart(id: Identifier, on: Ast.expr): Unit =
    out.puts(s"switch (${expression(on)}) {")

  override def switchCaseStart(condition: Ast.expr): Unit = {
    // Java is very specific about what can be used as "condition" in "case
    // condition:".
    val condStr = condition match {
      case Ast.expr.EnumByLabel(enumName, enumVal) =>
        // If switch is over a enum, only literal enum values are supported,
        // and they must be written as "MEMBER", not "SomeEnum.MEMBER".
        value2Const(enumVal.name)
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

  override def switchEnd(): Unit =
    out.puts("}")

  override def instanceDeclaration(attrName: InstanceIdentifier, attrType: DataType, condSpec: ConditionalSpec): Unit = {
    out.puts(s"private ${kaitaiType2JavaTypeBoxed(attrType)} ${idToStr(attrName)};")
  }

  override def instanceHeader(className: String, instName: InstanceIdentifier, dataType: DataType): Unit = {
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

  override def instanceCalculate(instName: InstanceIdentifier, dataType: DataType, value: expr): Unit = {
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
}

object JavaCompiler extends LanguageCompilerStatic
  with UpperCamelCaseClasses
  with StreamStructNames {
  override def getTranslator(tp: TypeProvider, config: RuntimeConfig) = new JavaTranslator(tp)

  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new JavaCompiler(tp, config)

  def kaitaiType2JavaType(attrType: DataType): String = kaitaiType2JavaTypePrim(attrType)

  def kaitaiType2JavaType(attrType: DataType, condSpec: ConditionalSpec): String =
    if (condSpec.ifExpr.nonEmpty) {
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
      case CalcIntType => "long"
      case CalcFloatType => "double"

      case _: StrType => "String"
      case _: BytesType => "byte[]"

      case AnyType => "Object"
      case KaitaiStreamType => kstreamName
      case KaitaiStructType => kstructName

      case t: UserType => types2class(t.name)
      case EnumType(name, _) => types2class(name)

      case ArrayType(_) => kaitaiType2JavaTypeBoxed(attrType)

      case SwitchType(_, cases) => kaitaiType2JavaTypePrim(TypeDetector.combineTypes(cases.values))
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
      case CalcIntType => "Long"
      case CalcFloatType => "Double"

      case _: StrType => "String"
      case _: BytesType => "byte[]"

      case AnyType => "Object"
      case KaitaiStreamType => kstreamName
      case KaitaiStructType => kstructName

      case t: UserType => type2class(t.name.last)
      case EnumType(name, _) => types2class(name)

      case ArrayType(inType) => s"ArrayList<${kaitaiType2JavaTypeBoxed(inType)}>"

      case SwitchType(_, cases) => kaitaiType2JavaTypeBoxed(TypeDetector.combineTypes(cases.values))
    }
  }

  def types2class(names: List[String]) = names.map(x => type2class(x)).mkString(".")

  override def kstreamName: String = "KaitaiStream"
  override def kstructName: String = "KaitaiStruct"
}
