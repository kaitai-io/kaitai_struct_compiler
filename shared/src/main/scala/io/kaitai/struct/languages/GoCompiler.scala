package io.kaitai.struct.languages

import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig, Utils}
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.{GoTranslator, JavaTranslator, TypeDetector, TypeProvider}

class GoCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with SingleOutputFile
    with UpperCamelCaseClasses
    with ObjectOrientedLanguage
    with EveryReadIsExpression
    with UniversalFooter
    with UniversalDoc
    with AllocateIOLocalVar
    with FixedContentsUsingArrayByteLiteral {
  import GoCompiler._

  override def innerClasses = false

  override def getStatic = GoCompiler

  override def universalFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def indent: String = "\t"
  override def outFileName(topClassName: String): String =
    s"src/${config.goPackage}/$topClassName.go"

  override def fileHeader(topClassName: String): Unit = {
    out.puts(s"// $headerComment")
    if (!config.goPackage.isEmpty) {
      out.puts
      out.puts(s"package ${config.goPackage}")
    }
    out.puts
    out.puts("import \"github.com/tarm/kaitai_struct_go_runtime/kaitai\"")

    out.puts
  }

  override def classHeader(name: List[String]): Unit = {
    out.puts(s"type ${types2class(name)} struct {")
    out.inc
  }

  override def classFooter(name: List[String]): Unit = universalFooter

  override def classConstructorHeader(name: List[String], parentType: DataType, rootClassName: List[String]): Unit = {
    out.puts
    out.puts(
      s"func (this *${types2class(name)}) Read(" +
        s"io *$kstreamName, " +
        s"parent *${kaitaiType2NativeType(parentType)}, " +
        s"root *${types2class(rootClassName)}) (err error) {"
    )
    out.inc
    out.puts(s"${privateMemberName(IoIdentifier)} = io")
    out.puts(s"${privateMemberName(ParentIdentifier)} = parent")
    out.puts(s"${privateMemberName(RootIdentifier)} = root")
    out.puts
  }

  override def classConstructorFooter: Unit = {
    out.puts("return err")
    universalFooter
  }

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, condSpec: ConditionalSpec): Unit =
    out.puts(s"${idToStr(attrName)} ${kaitaiType2NativeType(attrType)}")

  override def attributeReader(attrName: Identifier, attrType: DataType, condSpec: ConditionalSpec): Unit = {
    out.puts(s"func (this *${types2class(typeProvider.nowClass.name)}) ${publicMemberName(attrName)}() ${kaitaiType2NativeType(attrType)} {")
    out.inc
    out.puts(s"return this.${idToStr(attrName)}")
    universalFooter
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

  override def useIO(ioEx: Ast.expr): String = {
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

  override def condIfHeader(expr: Ast.expr): Unit = {
    out.puts(s"if (${expression(expr)}) {")
    out.inc
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = new ArrayList<byte[]>();")
    out.puts(s"${privateMemberName(id)} = new ${kaitaiType2NativeType(ArrayType(dataType))}();")
    out.puts(s"while (!$io.isEof()) {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}.add($expr);")
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: Ast.expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = new ArrayList<byte[]>((int) (${expression(repeatExpr)}));")
    out.puts(s"${privateMemberName(id)} = make(${kaitaiType2NativeType(ArrayType(dataType))}, ${expression(repeatExpr)})")
    out.puts(s"for i := range ${privateMemberName(id)} {")
    out.inc
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}.add($expr);")
  }

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, untilExpr: Ast.expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = new ArrayList<byte[]>();")
    out.puts(s"${privateMemberName(id)} = new ${kaitaiType2NativeType(ArrayType(dataType))}();")
    out.puts("{")
    out.inc
    out.puts(s"${kaitaiType2NativeType(dataType)} ${translator.doName("_")};")
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

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, untilExpr: Ast.expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.dec
    out.puts(s"} while (!(${expression(untilExpr)}));")
    out.dec
    out.puts("}")
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    out.puts(s"if ${privateMemberName(id)}, err = $expr; err != nil {")
    out.inc
    out.puts("return err")
    universalFooter
  }

  override def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit =
    out.puts(s"${kaitaiType2NativeType(dataType)} $id = $expr;")

  override def parseExpr(dataType: DataType, io: String): String = {
    dataType match {
      case t: ReadableType =>
        s"$io.Read${Utils.capitalize(t.apiCall)}()"
      case blt: BytesLimitType =>
        s"$io.ReadBytes(${expression(blt.size)})"
      case _: BytesEosType =>
        s"$io.ReadBytesFull()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        s"$io.ReadBytesTerm($terminator, $include, $consume, $eosError)"
      case BitsType1 =>
        s"$io.ReadBitsInt(1) != 0"
      case BitsType(width: Int) =>
        s"$io.ReadBitsInt($width)"
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
        s"${types2class(t.name)}($io$addArgs)"
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
    out.puts(s"${calculatedFlagForName(attrName)} bool")
    out.puts(s"${idToStr(attrName)} ${kaitaiType2NativeType(attrType)}")
  }

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType): Unit = {
    out.puts(s"func (this *${types2class(className)}) ${publicMemberName(instName)}() (v ${kaitaiType2NativeType(dataType)}, err error) {")
    out.inc
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier): Unit = {
    out.puts(s"if (this.${calculatedFlagForName(instName)}) {")
    out.inc
    instanceReturn(instName)
    universalFooter
  }

  override def instanceReturn(instName: InstanceIdentifier): Unit = {
    out.puts(s"return ${privateMemberName(instName)}, nil")
  }

  override def instanceSetCalculated(instName: InstanceIdentifier): Unit =
    out.puts(s"${calculatedFlagForName(instName)} = true")

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, String)]): Unit = {
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

  override def publicMemberName(id: Identifier): String = {
    id match {
      case IoIdentifier => "_IO"
      case RootIdentifier => "_Root"
      case ParentIdentifier => "_Parent"
      case NamedIdentifier(name) => Utils.upperCamelCase(name)
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => Utils.upperCamelCase(name)
      case RawIdentifier(innerId) => "_raw_" + idToStr(innerId)
    }
  }

  def calculatedFlagForName(id: Identifier) = s"_f_${idToStr(id)}"
}

object GoCompiler extends LanguageCompilerStatic
  with UpperCamelCaseClasses
  with StreamStructNames {
  override def getTranslator(tp: TypeProvider, config: RuntimeConfig) = new GoTranslator(tp)

  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new GoCompiler(tp, config)

  /**
    * Determine Go data type corresponding to a KS data type.
    *
    * @param attrType KS data type
    * @return Go data type
    */
  def kaitaiType2NativeType(attrType: DataType): String = {
    attrType match {
      case Int1Type(false) => "uint8"
      case IntMultiType(false, Width2, _) => "uint16"
      case IntMultiType(false, Width4, _) => "uint32"
      case IntMultiType(false, Width8, _) => "uint64"

      case Int1Type(true) => "int8"
      case IntMultiType(true, Width2, _) => "int16"
      case IntMultiType(true, Width4, _) => "int32"
      case IntMultiType(true, Width8, _) => "int64"

      case FloatMultiType(Width4, _) => "float32"
      case FloatMultiType(Width8, _) => "float64"

      case BitsType(_) => "long"

      case _: BooleanType => "boolean"
      case CalcIntType => "int"
      case CalcFloatType => "float64"

      case _: StrType => "string"
      case _: BytesType => "byte[]"

      case AnyType => "Object"
      case KaitaiStreamType => "*" + kstreamName
      case KaitaiStructType => kstructName

      case t: UserType => "*" + types2class(t.classSpec match {
        case Some(cs) => cs.name
        case None => t.name
      })
      case EnumType(name, _) => types2class(name)

      case ArrayType(inType) => s"[]${kaitaiType2NativeType(inType)}"

      case SwitchType(_, cases) => kaitaiType2NativeType(TypeDetector.combineTypes(cases.values))
    }
  }

  def types2class(names: List[String]) = names.map(x => type2class(x)).mkString("_")

  override def kstreamName: String = "kaitai.Stream"
  override def kstructName: String = "interface{}"
}
