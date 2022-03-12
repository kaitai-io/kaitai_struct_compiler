package io.kaitai.struct.languages

import io.kaitai.struct._
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.{CSharpTranslator, TypeDetector}

class CSharpCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with UpperCamelCaseClasses
    with ObjectOrientedLanguage
    with SingleOutputFile
    with AllocateIOLocalVar
    with EveryReadIsExpression
    with UniversalDoc
    with FixedContentsUsingArrayByteLiteral
    with SwitchIfOps
    with NoNeedForFullClassPath {
  import CSharpCompiler._

  val translator = new CSharpTranslator(typeProvider, importList)

  override def indent: String = "    "
  override def outFileName(topClassName: String): String = s"${type2class(topClassName)}.cs"

  override def outImports(topClass: ClassSpec) =
    importList.toList.map((x) => s"using $x;").mkString("", "\n", "\n")

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"// $headerComment")
    outHeader.puts

    var ns = "Kaitai"
    if (!config.dotNetNamespace.isEmpty)
      ns = config.dotNetNamespace

    if (ns != "Kaitai")
      importList.add("Kaitai")

    out.puts
    out.puts(s"namespace $ns")
    out.puts(s"{")
    out.inc
  }

  override def fileFooter(topClassName: String): Unit = {
    out.dec
    out.puts("}")
  }

  override def classHeader(name: String): Unit = {

    out.puts(s"public partial class ${type2class(name)} : $kstructName")
    out.puts(s"{")
    out.inc

    // `FromFile` is generated only for parameterless types
    if (typeProvider.nowClass.params.isEmpty) {
      out.puts(s"public static ${type2class(name)} FromFile(string fileName)")
      out.puts(s"{")
      out.inc
      out.puts(s"return new ${type2class(name)}(new $kstreamName(fileName));")
      out.dec
      out.puts("}")
      out.puts
    }
  }

  override def classFooter(name: String): Unit = fileFooter(name)

  override def classConstructorHeader(name: String, parentType: DataType, rootClassName: String, isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    typeProvider.nowClass.meta.endian match {
      case Some(_: CalcEndian) | Some(InheritedEndian) =>
        out.puts(s"private bool? ${privateMemberName(EndianIdentifier)};")
      case _ =>
        // no _is_le variable
    }

    val addEndian = if (isHybrid) ", bool? isLe = null" else ""

    val pIo = paramName(IoIdentifier)
    val pParent = paramName(ParentIdentifier)
    val pRoot = paramName(RootIdentifier)

    val paramsArg = Utils.join(params.map((p) =>
      s"${kaitaiType2NativeType(p.dataType)} ${paramName(p.id)}"
    ), "", ", ", ", ")

    out.puts(
      s"public ${type2class(name)}($paramsArg" +
        s"$kstreamName $pIo, " +
        s"${kaitaiType2NativeType(parentType)} $pParent = null, " +
        s"${type2class(rootClassName)} $pRoot = null$addEndian) : base($pIo)"
    )
    out.puts(s"{")
    out.inc
    handleAssignmentSimple(ParentIdentifier, pParent)

    handleAssignmentSimple(
      RootIdentifier,
      if (name == rootClassName) s"$pRoot ?? this" else pRoot
    )

    if (isHybrid)
      handleAssignmentSimple(EndianIdentifier, "isLe")

    // Store parameters passed to us
    params.foreach((p) => handleAssignmentSimple(p.id, paramName(p.id)))
  }

  override def classConstructorFooter: Unit = fileFooter(null)

  override def runRead(name: List[String]): Unit =
    out.puts("_read();")

  override def runReadCalc(): Unit = {
    out.puts
    out.puts(s"if (${privateMemberName(EndianIdentifier)} == null) {")
    out.inc
    out.puts(s"throw new ${ksErrorName(UndecidedEndiannessError)}();")
    importList.add("System")
    out.dec
    out.puts(s"} else if (${privateMemberName(EndianIdentifier)} == true) {")
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
    val readAccessAndType = if (!config.autoRead) {
      "public"
    } else {
      "private"
    }
    val suffix = endian match {
      case Some(e) => Utils.upperUnderscoreCase(e.toSuffix)
      case None => ""
    }
    out.puts(s"$readAccessAndType void _read$suffix()")
    out.puts("{")
    out.inc
  }

  override def readFooter(): Unit = fileFooter("")

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"private ${kaitaiType2NativeTypeNullable(attrType, isNullable)} ${privateMemberName(attrName)};")
  }

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"public ${kaitaiType2NativeTypeNullable(attrType, isNullable)} ${publicMemberName(attrName)} { get { return ${privateMemberName(attrName)}; } }")
  }

  override def universalDoc(doc: DocSpec): Unit = {
    out.puts
    doc.summary.foreach { summary =>
      out.puts("/// <summary>")
      out.putsLines("/// ", XMLUtils.escape(summary))
      out.puts("/// </summary>")
    }

    doc.ref.foreach { docRef =>
      out.puts("/// <remarks>")

      val refStr = docRef match {
        case TextRef(text) => XMLUtils.escape(text)
        case ref: UrlRef => ref.toAhref
      }

      out.putsLines("/// ", s"Reference: $refStr")
      out.puts("/// </remarks>")
    }
  }

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {
    out.puts(s"if (${privateMemberName(EndianIdentifier)} == true) {")
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
    out.puts(s"${privateMemberName(attrName)} = $normalIO.EnsureFixedContents($contents);")

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec): Unit = {
    val srcExpr = getRawIdExpr(varSrc, rep)

    val expr = proc match {
      case ProcessXor(xorValue) =>
        s"$normalIO.ProcessXor($srcExpr, ${expression(xorValue)})"
      case ProcessZlib =>
        s"$normalIO.ProcessZlib($srcExpr)"
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        s"$normalIO.ProcessRotateLeft($srcExpr, $expr, 1)"
      case ProcessCustom(name, args) =>
        val procClass = types2class(name)
        val procName = s"_process_${idToStr(varSrc)}"
        out.puts(s"$procClass $procName = new $procClass(${args.map(expression).mkString(", ")});")
        s"$procName.Decode($srcExpr)"
    }
    handleAssignment(varDest, expr, rep, false)
  }

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = {
    val privateVarName = privateMemberName(varName)

    val ioName = s"io_$privateVarName"

    val args = rep match {
      case RepeatUntil(_) => translator.doName(Identifier.ITERATOR2)
      case _ => getRawIdExpr(varName, rep)
    }

    out.puts(s"var $ioName = new $kstreamName($args);")
    ioName
  }

  def getRawIdExpr(varName: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(varName)
    rep match {
      case NoRepeat => memberName
      case _ => s"$memberName[$memberName.Count - 1]"
    }
  }

  override def useIO(ioEx: expr): String = {
    out.puts(s"$kstreamName io = ${expression(ioEx)};")
    "io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"long _pos = $io.Pos;")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io.Seek(${expression(pos)});")

  override def popPos(io: String): Unit =
    out.puts(s"$io.Seek(_pos);")

  override def alignToByte(io: String): Unit =
    out.puts(s"$io.AlignToByte();")

  override def instanceClear(instName: InstanceIdentifier): Unit = {
    out.puts(s"${flagForInstName(instName)} = false;")
  }

  override def instanceSetCalculated(instName: InstanceIdentifier): Unit = {
    out.puts(s"${flagForInstName(instName)} = true;")
  }

  override def condIfHeader(expr: expr): Unit = {
    out.puts(s"if (${expression(expr)}) {")
    out.inc
  }

  override def condIfFooter(expr: expr): Unit = fileFooter(null)

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw): Unit = {
    importList.add("System.Collections.Generic")

    if (needRaw.level >= 1)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = new List<byte[]>();")
    if (needRaw.level >= 2)
      out.puts(s"${privateMemberName(RawIdentifier(RawIdentifier(id)))} = new List<byte[]>();")

    out.puts(s"${privateMemberName(id)} = new ${kaitaiType2NativeType(ArrayTypeInStream(dataType))}();")
    out.puts("{")
    out.inc
    out.puts("var i = 0;")
    out.puts(s"while (!$io.IsEof) {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}.Add($expr);")
  }

  override def condRepeatEosFooter: Unit = {
    out.puts("i++;")
    out.dec
    out.puts("}")
    out.dec
    out.puts("}")
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw, repeatExpr: expr): Unit = {
    importList.add("System.Collections.Generic")

    if (needRaw.level >= 1)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = new List<byte[]>((int) (${expression(repeatExpr)}));")
    if (needRaw.level >= 2)
      out.puts(s"${privateMemberName(RawIdentifier(RawIdentifier(id)))} = new List<byte[]>((int) (${expression(repeatExpr)}));")
    out.puts(s"${privateMemberName(id)} = new ${kaitaiType2NativeType(ArrayTypeInStream(dataType))}((int) (${expression(repeatExpr)}));")
    out.puts(s"for (var i = 0; i < ${expression(repeatExpr)}; i++)")
    out.puts("{")
    out.inc
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}.Add($expr);")
  }

  override def condRepeatExprFooter: Unit = fileFooter(null)

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw, untilExpr: expr): Unit = {
    importList.add("System.Collections.Generic")

    if (needRaw.level >= 1)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = new List<byte[]>();")
    if (needRaw.level >= 2)
      out.puts(s"${privateMemberName(RawIdentifier(RawIdentifier(id)))} = new List<byte[]>();")
    out.puts(s"${privateMemberName(id)} = new ${kaitaiType2NativeType(ArrayTypeInStream(dataType))}();")
    out.puts("{")
    out.inc
    out.puts("var i = 0;")
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
    out.puts(s"${privateMemberName(id)}.Add($tempVar);")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw, untilExpr: expr): Unit = {
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
    out.puts(s"${kaitaiType2NativeType(dataType)} $id = $expr;")

  override def blockScopeHeader: Unit = {
    out.puts("{")
    out.inc
  }
  override def blockScopeFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    dataType match {
      case t: ReadableType =>
        s"$io.Read${Utils.capitalize(t.apiCall(defEndian))}()"
      case blt: BytesLimitType =>
        s"$io.ReadBytes(${expression(blt.size)})"
      case _: BytesEosType =>
        s"$io.ReadBytesFull()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        s"$io.ReadBytesTerm($terminator, $include, $consume, $eosError)"
      case BitsType1(bitEndian) =>
        s"$io.ReadBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}(1) != 0"
      case BitsType(width: Int, bitEndian) =>
        s"$io.ReadBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}($width)"
      case t: UserType =>
        val addParams = Utils.join(t.args.map((a) => translator.translate(a)), "", ", ", ", ")
        val addArgs = if (t.isOpaque) {
          ""
        } else {
          val parent = t.forcedParent match {
            case Some(USER_TYPE_NO_PARENT) => "null"
            case Some(fp) => translator.translate(fp)
            case None => "this"
          }
          val addEndian = t.classSpec.get.meta.endian match {
            case Some(InheritedEndian) => s", ${privateMemberName(EndianIdentifier)}"
            case _ => ""
          }
          s", $parent, ${privateMemberName(RootIdentifier)}$addEndian"
        }
        s"new ${types2class(t.name)}($addParams$io$addArgs)"
    }
  }

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean) = {
    val expr1 = padRight match {
      case Some(padByte) => s"$kstreamName.BytesStripRight($expr0, $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) => s"$kstreamName.BytesTerminate($expr1, $term, $include)"
      case None => expr1
    }
    expr2
  }

  override def userTypeDebugRead(id: String, dataType: DataType, assignType: DataType): Unit = {
    val expr = if (assignType != dataType) {
      s"((${kaitaiType2NativeType(dataType)}) ($id))"
    } else {
      id
    }
    out.puts(s"$expr._read();")
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
    out.puts(s"case ${expression(condition)}: {")
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

  //</editor-fold>

  //<editor-fold desc="switching: emulation with ifs">

  override def switchIfStart(id: Identifier, on: Ast.expr, onType: DataType): Unit = {
    out.puts("{")
    out.inc
    out.puts(s"${kaitaiType2NativeType(onType)} ${expression(NAME_SWITCH_ON)} = ${expression(on)};")
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
    out.puts(s"if (${switchCmpExpr(condition)})")
    out.puts("{")
    out.inc
  }

  override def switchIfCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"else if (${switchCmpExpr(condition)})")
    out.puts("{")
    out.inc
  }

  override def switchIfCaseEnd(): Unit = {
    out.dec
    out.puts("}")
  }

  override def switchIfElseStart(): Unit = {
    out.puts("else")
    out.puts("{")
    out.inc
  }

  override def switchIfEnd(): Unit = {
    out.dec
    out.puts("}")
  }

  //</editor-fold>

  override def instanceDeclaration(attrName: InstanceIdentifier, attrType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"private bool ${flagForInstName(attrName)};")
    out.puts(s"private ${kaitaiType2NativeTypeNullable(attrType, isNullable)} ${privateMemberName(attrName)};")
  }

  override def instanceHeader(className: String, instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"public ${kaitaiType2NativeTypeNullable(dataType, isNullable)} ${publicMemberName(instName)}")
    out.puts("{")
    out.inc
    out.puts("get")
    out.puts("{")
    out.inc
  }

  override def instanceFooter: Unit = {
    out.dec
    out.puts("}")
    out.dec
    out.puts("}")
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    out.puts(s"if (${flagForInstName(instName)})")
    out.inc
    instanceReturn(instName, dataType)
    out.dec
  }

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit = {
    out.puts(s"return ${privateMemberName(instName)};")
  }

  override def instanceCalculate(instName: Identifier, dataType: DataType, value: expr): Unit =
    // Perform explicit cast as unsigned integers can't be directly assigned to the default int type
    handleAssignmentSimple(instName, s"(${kaitaiType2NativeType(dataType)}) (${expression(value)})")

  def flagForInstName(ksName: Identifier) = s"f_${idToStr(ksName)}"

  override def enumDeclaration(curClass: String, enumName: String, enumColl: Seq[(Long, String)]): Unit = {
    val enumClass = type2class(enumName)

    out.puts
    out.puts(s"public enum $enumClass")
    out.puts(s"{")
    out.inc

    enumColl.foreach { case (id, label) =>
      out.puts(s"${Utils.upperCamelCase(label)} = ${translator.doIntLiteral(id)},")
    }

    out.dec
    out.puts("}")
  }

  def idToStr(id: Identifier): String = {
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => Utils.lowerCamelCase(name)
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => Utils.lowerCamelCase(name)
      case RawIdentifier(innerId) => "_raw_" + idToStr(innerId)
    }
  }

  override def publicMemberName(id: Identifier): String = {
    id match {
      case SpecialIdentifier(name) => s"M${Utils.upperCamelCase(name)}"
      case NamedIdentifier(name) => Utils.upperCamelCase(name)
      case NumberedIdentifier(idx) => s"${NumberedIdentifier.TEMPLATE.capitalize}_$idx"
      case InstanceIdentifier(name) => Utils.upperCamelCase(name)
      case RawIdentifier(innerId) => s"M_Raw${publicMemberName(innerId)}"
    }
  }

  override def privateMemberName(id: Identifier): String = {
    id match {
      case SpecialIdentifier(name) => s"m${Utils.lowerCamelCase(name)}"
      case _ => s"_${idToStr(id)}"
    }
  }

  override def localTemporaryName(id: Identifier): String = s"_t_${idToStr(id)}"

  override def paramName(id: Identifier): String = s"p_${idToStr(id)}"

  override def ksErrorName(err: KSError): String = CSharpCompiler.ksErrorName(err)

  override def attrValidateExpr(
    attrId: Identifier,
    attrType: DataType,
    checkExpr: Ast.expr,
    err: KSError,
    errArgs: List[Ast.expr]
  ): Unit = {
    val errArgsStr = errArgs.map(translator.translate).mkString(", ")
    out.puts(s"if (!(${translator.translate(checkExpr)}))")
    out.puts("{")
    out.inc
    out.puts(s"throw new ${ksErrorName(err)}($errArgsStr);")
    out.dec
    out.puts("}")
  }
}

object CSharpCompiler extends LanguageCompilerStatic
  with StreamStructNames
  with UpperCamelCaseClasses
  with ExceptionNames {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new CSharpCompiler(tp, config)

  /**
    * Determine .NET data type corresponding to a KS data type.
    *
    * @param attrType KS data type
    * @return .NET data type
    */
  def kaitaiType2NativeType(attrType: DataType): String = {
    attrType match {
      case Int1Type(false) => "byte"
      case IntMultiType(false, Width2, _) => "ushort"
      case IntMultiType(false, Width4, _) => "uint"
      case IntMultiType(false, Width8, _) => "ulong"

      case Int1Type(true) => "sbyte"
      case IntMultiType(true, Width2, _) => "short"
      case IntMultiType(true, Width4, _) => "int"
      case IntMultiType(true, Width8, _) => "long"

      case FloatMultiType(Width4, _) => "float"
      case FloatMultiType(Width8, _) => "double"

      case BitsType(_, _) => "ulong"

      case CalcIntType => "int"
      case CalcFloatType => "double"
      case _: BooleanType => "bool"

      case _: StrType => "string"
      case _: BytesType => "byte[]"

      case AnyType => "object"
      case KaitaiStructType | CalcKaitaiStructType => kstructName
      case KaitaiStreamType | OwnedKaitaiStreamType => kstreamName

      case t: UserType => types2class(t.name)
      case EnumType(name, _) => types2class(name)

      case at: ArrayType => s"List<${kaitaiType2NativeType(at.elType)}>"

      case st: SwitchType => kaitaiType2NativeType(st.combinedType)
    }
  }

  def kaitaiType2NativeTypeNullable(t: DataType, isNullable: Boolean): String = {
    val r = kaitaiType2NativeType(t)
    if (isNullable) {
      t match {
        case _: NumericType | _: BooleanType => s"$r?"
        case _ => r
      }
    } else {
      r
    }
  }

  def types2class(typeName: Ast.typeId): String =
    // FIXME: handle absolute
    types2class(typeName.names)
  def types2class(names: Iterable[String]) = names.map(type2class).mkString(".")

  override def kstructName = "KaitaiStruct"
  override def kstreamName = "KaitaiStream"
  override def ksErrorName(err: KSError): String = err match {
    case EndOfStreamError => "EndOfStreamException"
    case _ => err.name
  }
}
