package io.kaitai.struct.languages

import io.kaitai.struct.datatype.DataType.{CalcIntType, _}
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.TypeScriptTranslator
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig, Utils}

import scala.::

class TypeScriptCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with ObjectOrientedLanguage
    with UpperCamelCaseClasses
    with SingleOutputFile
    with UniversalDoc
    with AllocateIOLocalVar
    with EveryReadIsExpression
    with SwitchIfOps
    with FixedContentsUsingArrayByteLiteral {

  import TypeScriptCompiler._

  override val translator = new TypeScriptTranslator(typeProvider)

  override def indent: String = "  "

  override def innerEnums: Boolean = false

  override def innerClasses: Boolean = false

  override def normalIO: String = s"this.${publicMemberName(IoIdentifier)}"

  override def outFileName(topClassName: String): String = s"${Utils.lowerKebabCase(topClassName)}.ts"

  // override def outImports(topClass: ClassSpec) = {
  //   importList.toList.map(it => s"import {${it.split('/').last}} from '$it'").mkString("\n") + "\n"
  // }

  override def importFile(file: String): Unit = {
    val fileName = file.split("/").last
    val fullPath = "./" + Utils.lowerKebabCase(fileName)

    out.puts(s"import {${Utils.upperCamelCase(fileName)}} from '${fullPath}'")
  }

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts("/*")
    outHeader.puts(s" * $headerComment")
    outHeader.puts(" */")

    outHeader.puts("// @ts-ignore")
    outHeader.puts("import KaitaiStream from 'kaitai-struct/KaitaiStream'")
  }

  override def opaqueClassDeclaration(classSpec: ClassSpec): Unit = {
    val className = type2class(classSpec.name.head)
    // FIXME
    out.puts(s"import ${className} from './opaque'")
  }

  override def classHeader(name: List[String]): Unit = {
    out.puts
    if (name.size > 1) {
      out.puts(s"export namespace ${types2class(name.dropRight(1))} {")
      out.inc
    }
    out.puts(s"export class ${type2class(name.last)} {")
    out.inc
  }

  override def classFooter(name: List[String]): Unit = {
    out.dec
    out.puts("}")
    if (name.length > 1) {
      out.dec
      out.puts("}")
    }
  }

  override def classConstructorHeader(name: List[String], parentClassName: DataType, rootClassName: List[String], isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    if (config.readStoresPos) {
      out.puts("_debug: any = {};")
    }
    if (!isHybrid) {
      out.puts(s"${Identifier.IS_LE}?: boolean;")
      out.puts
      ""
    }

    out.puts(s"constructor(")
    out.inc
    out.puts(s"readonly ${Identifier.IO}: $kstreamName,")
    out.puts(s"readonly ${Identifier.PARENT}: ${kaitaiType2NativeType(parentClassName, isNullable = false)},")
    out.puts(s"readonly ${Identifier.ROOT}: ${types2class(rootClassName)},")
    if (isHybrid) {
      out.puts(s"public ${Identifier.IS_LE}: boolean,")
    }
    params.foreach { p =>
      out.puts(s"${publicMemberName(p.id)}: ${kaitaiType2NativeType(p.dataType, isNullable = false)},")
    }
    out.dec
    out.puts(") {")

    out.inc
    if (rootClassName == name) {
      out.puts(s"this.${Identifier.ROOT} ??= this;")
    }

    params.foreach { p =>
      out.puts(s"this.${publicMemberName(p.id)} = ${publicMemberName(p.id)};")
    }

    out.puts
  }

  override def classConstructorFooter: Unit = {
    out.dec
    out.puts("}")
    out.puts
  }

  override def runRead(name: List[String]): Unit = {
    out.puts("this._read();")
  }

  override def runReadCalc(): Unit = {
    out.puts
    out.puts(s"if (this._is_le === true) {")
    out.inc
    out.puts("this._readLE();")
    out.dec
    out.puts(s"} else if (this.${Identifier.IS_LE} === false) {")
    out.inc
    out.puts("this._readBE();")
    out.dec
    out.puts("} else {")
    out.inc
    out.puts("throw new KaitaiStream.UndecidedEndiannessError();")
    out.dec
    out.puts("}")
  }

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean) = {
    val suffix = endian match {
      case Some(e) => Utils.upperUnderscoreCase(e.toSuffix)
      case None => ""
    }
    out.puts(s"_read$suffix() {")
    out.inc
  }

  override def readFooter() = {
    out.dec
    out.puts("}")
    out.puts
  }

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
  }

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    val name = publicMemberName(attrName)
    if (name == Identifier.ROOT || name == Identifier.PARENT) return
    out.puts(s"$name: ${kaitaiType2NativeType(attrType, isNullable)};")
  }

  override def universalDoc(doc: DocSpec): Unit = {
    // JSDoc docstring style: http://usejsdoc.org/about-getting-started.html
    out.puts
    out.puts("/**")

    doc.summary.foreach(summary => out.putsLines(" * ", summary))

    // http://usejsdoc.org/tags-see.html
    doc.ref.foreach {
      case TextRef(text) =>
        out.putsLines(" * ", s"@see $text")
      case UrlRef(url, text) =>
        out.putsLines(" * ", s"@see {@link $url|$text}")
    }

    out.puts(" */")
  }

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {
    out.puts(s"if (this.${Identifier.IS_LE}) {")
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
    out.puts(s"this.${privateMemberName(attrName)} = " +
      s"$normalIO.ensureFixedContents($contents);")
  }

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec): Unit = {
    val srcExpr = getRawIdExpr(varSrc, rep)

    val expr = proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "processXorOne"
          case _: BytesType => "processXorMany"
        }
        s"$kstreamName.$procName($srcExpr, ${expression(xorValue)})"
      case ProcessZlib =>
        s"$kstreamName.processZlib($srcExpr)"
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        s"$kstreamName.processRotateLeft($srcExpr, $expr, 1)"
      case ProcessCustom(name, args) =>
        val nameInit = name.init
        val pkgName = if (nameInit.isEmpty) "" else nameInit.mkString("-") + "/"
        val procClass = type2class(name.last)

        importList.add(s"$pkgName$procClass")

        s"new $procClass(${args.map(expression).mkString(", ")}).decode($srcExpr)"
    }
    handleAssignment(varDest, expr, rep, false)
  }

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = {
    val langName = idToStr(varName)

    val ioName = s"${Identifier.IO}_$langName"

    val args = getRawIdExpr(varName, rep)

    out.puts(s"let $ioName = new $kstreamName($args);")
    ioName
  }

  def getRawIdExpr(varName: Identifier, rep: RepeatSpec): String = {
    val memberName = s"this.${publicMemberName(varName)}"
    rep match {
      case NoRepeat => memberName
      case RepeatExpr(_) => s"$memberName[i]"
      case _ => s"$memberName[$memberName.length - 1]"
    }
  }

  override def useIO(ioEx: expr): String = {
    out.puts(s"let io = ${expression(ioEx)};")
    "io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"let _pos = $io.pos;")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io.seek(${expression(pos)});")

  override def popPos(io: String): Unit =
    out.puts(s"$io.seek(_pos);")

  override def alignToByte(io: String): Unit =
    out.puts(s"$io.alignToByte();")

  override def attrDebugStart(attrId: Identifier, attrType: DataType, io: Option[String], rep: RepeatSpec): Unit = {
    if (!attrDebugNeeded(attrId))
      return

    val debugName = attrDebugName(attrId, rep, false)

    val ioProps = io match {
      case None => ""
      case Some(x) => s"start: $x.pos, ioOffset: $x.byteOffset"
    }

    val enumNameProps = attrType match {
      case t: EnumType => s"""enumName: \"${types2class(t.enumSpec.get.name)}\""""
      case _ => ""
    }

    out.puts(s"$debugName = { $ioProps${if (ioProps != "" && enumNameProps != "") ", " else ""}$enumNameProps };")
  }

  override def attrDebugEnd(attrId: Identifier, attrType: DataType, io: String, rep: RepeatSpec): Unit = {
    if (!attrDebugNeeded(attrId))
      return
    val debugName = attrDebugName(attrId, rep, true)

    out.puts(s"$debugName.end = $io.pos;")
  }

  override def condIfHeader(expr: expr): Unit = {
    out.puts(s"if (${expression(expr)}) {")
    out.inc
  }

  // TODO: replace this with UniversalFooter
  override def condIfFooter(expr: expr): Unit = {
    out.dec
    out.puts("}")
  }

  override def condRepeatCommonInit(id: Identifier, dataType: DataType, needRaw: NeedRaw): Unit = {
    if (needRaw.level >= 1)
      out.puts(s"this.${publicMemberName(RawIdentifier(id))} = [];")
    if (needRaw.level >= 2)
      out.puts(s"this.${publicMemberName(RawIdentifier(RawIdentifier(id)))} = [];")
    out.puts(s"this.${
      id match {
        case InstanceIdentifier(_) => privateMemberName(id)
        case _ => publicMemberName(id)
      }
    } = [];")
    if (config.readStoresPos)
      out.puts(s"this._debug.${idToStr(id)}.arr = [];")
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType): Unit = {
    out.puts("let i = 0;")
    out.puts(s"while (!$io.isEof()) {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    out.puts(s"this.${
      id match {
        case InstanceIdentifier(_) => privateMemberName(id)
        case _ => publicMemberName(id)
      }
    }.push($expr);")
  }

  override def condRepeatEosFooter: Unit = {
    out.puts("i++;")
    out.dec
    out.puts("}")
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, repeatExpr: expr): Unit = {
    out.puts(s"for (let i = 0; i < ${expression(repeatExpr)}; i++) {")
    out.inc
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit =
    handleAssignmentRepeatEos(id, expr)

  override def condRepeatExprFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, untilExpr: expr): Unit = {
    out.puts("{")
    out.inc
    out.puts(s"let ${Identifier.ITERATOR}, ${Identifier.ITERATOR2};")
    out.puts("do {")
    out.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val tmpName = translator.doName(if (isRaw) Identifier.ITERATOR2 else Identifier.ITERATOR)
    out.puts(s"let $tmpName = $expr;")
    out.puts(s"this.${
      id match {
        case InstanceIdentifier(_) => privateMemberName(id)
        case _ => publicMemberName(id)
      }
    }.push($tmpName);")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, untilExpr: expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.dec
    out.puts(s"} while (!(${expression(untilExpr)}));")
    out.dec
    out.puts("}")
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    // Weird type errors can occur here, so just ignore whatever
    out.puts(s"this.${
      id match {
        case InstanceIdentifier(_) => privateMemberName(id)
        case _ => publicMemberName(id)
      }
    } = ($expr) as any")
  }

  override def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit =
    out.puts(s"let $id = $expr;")

  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    dataType match {
      case t: ReadableType =>
        s"$io.read${Utils.capitalize(t.apiCall(defEndian))}()"
      case blt: BytesLimitType =>
        s"$io.readBytes(${expression(blt.size)})"
      case _: BytesEosType =>
        s"$io.readBytesFull()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        s"$io.readBytesTerm($terminator, $include, $consume, $eosError)"
      case BitsType1(bitEndian) =>
        s"$io.readBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}(1) != 0"
      case BitsType(width: Int, bitEndian) =>
        s"$io.readBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}($width)"
      case t: UserType =>
        val parent = t.forcedParent match {
          case Some(USER_TYPE_NO_PARENT) => "null"
          case Some(fp) => translator.translate(fp)
          case None => "this"
        }
        val root = if (t.isOpaque) "null" else "this._root"
        val addEndian = t.classSpec.get.meta.endian match {
          case Some(InheritedEndian) => ", this._is_le"
          case _ => ""
        }
        val addParams = Utils.join(t.args.map((a) => translator.translate(a)), ", ", ", ", "")
        s"new ${kaitaiType2NativeType(t, isNullable = false)}($io, $parent, $root$addEndian$addParams)"
    }
  }

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean) = {
    val expr1 = padRight match {
      case Some(padByte) => s"$kstreamName.bytesStripRight($expr0, $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) => s"$kstreamName.bytesTerminate($expr1, $term, $include)"
      case None => expr1
    }
    expr2
  }

  override def userTypeDebugRead(id: String, dataType: DataType, assignType: DataType): Unit = {
    out.puts(s"$id._read();")
  }

  override def switchRequiresIfs(onType: DataType): Boolean = onType match {
    case _: IntType | _: BooleanType | _: EnumType | _: StrType => false
    case _ => true
  }

  //<editor-fold desc="switching: true version">

  override def switchStart(id: Identifier, on: Ast.expr): Unit = {
    out.puts(s"switch (${expression(on)}) {")
    out.inc
  }

  override def switchCaseFirstStart(condition: Ast.expr): Unit =
    switchCaseStart(condition)

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

  override def switchEnd(): Unit = {
    out.dec
    out.puts("}")
  }

  //</editor-fold>

  //<editor-fold desc="switching: emulation with ifs">

  val NAME_SWITCH_ON = Ast.expr.Name(Ast.identifier(Identifier.SWITCH_ON))

  override def switchIfStart(id: Identifier, on: Ast.expr, onType: DataType): Unit = {
    out.puts("{")
    out.inc
    out.puts(s"let ${expression(NAME_SWITCH_ON)} = ${expression(on)};")
  }

  private def switchCmpExpr(condition: Ast.expr): String =
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
    out.puts(s"private ${privateMemberName(attrName)}: ${kaitaiType2NativeType(attrType, isNullable)};")
  }

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"get ${publicMemberName(instName)}(): ${kaitaiType2NativeType(dataType, isNullable)} {")
    out.inc
  }

  override def instanceFooter: Unit = {
    out.dec
    out.puts("}")
    out.puts
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    out.puts(s"if (typeof this.${privateMemberName(instName)} !== 'undefined')")
    out.inc
    instanceReturn(instName, dataType)
    out.dec
  }

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit = {
    out.puts(s"return this.${privateMemberName(instName)};")
  }

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, EnumValueSpec)]): Unit = {
    out.puts(s"export namespace ${types2class(curClass)} {")
    out.inc
    out.puts(s"export const enum ${type2class(enumName)} {")
    out.inc

    enumColl.foreach { case (id, label) =>
      out.puts(s"${enumValue(enumName, label.name)} = ${translator.doIntLiteral(id)},")
    }

    out.dec
    out.puts("}")
    out.dec
    out.puts("}")
  }

  def enumValue(enumName: String, label: String) = Utils.upperUnderscoreCase(label)

  override def debugClassSequence(seq: List[AttrSpec]) = {
    //val seqStr = seq.map((attr) => "\"" + idToStr(attr.id) + "\"").mkString(", ")
    //out.puts(s"SEQ_FIELDS = [$seqStr]")
  }

  override def classToString(toStringExpr: Ast.expr): Unit = {
    val className = type2class(translator.provider.nowClass.name.last)

    out.puts
    out.puts(s"toString() {")
    out.inc
    out.puts(s"return ${translator.translate(toStringExpr)};")
    out.dec
    out.puts("}")
  }

  def idToStr(id: Identifier): String = TypeScriptCompiler.idToStr(id)

  override def publicMemberName(id: Identifier): String = idToStr(id)

  override def privateMemberName(id: Identifier): String = s"_${idToStr(id)}"

  override def localTemporaryName(id: Identifier): String = s"_t_${idToStr(id)}"

  override def ksErrorName(err: KSError): String = TypeScriptCompiler.ksErrorName(err)

  override def attrValidateExpr(
                                 attrId: Identifier,
                                 attrType: DataType,
                                 checkExpr: Ast.expr,
                                 err: KSError,
                                 errArgs: List[Ast.expr]
                               ): Unit = {
    val errArgsStr = errArgs.map(translator.translate).mkString(", ")
    out.puts(s"if (!(${translator.translate(checkExpr)})) {")
    out.inc
    out.puts(s"throw new ${ksErrorName(err)}($errArgsStr);")
    out.dec
    out.puts("}")
  }

  private
  def attrDebugNeeded(attrId: Identifier) = attrId match {
    case _: NamedIdentifier | _: NumberedIdentifier | _: InstanceIdentifier => true
    case _: RawIdentifier | _: SpecialIdentifier => false
  }

  def attrDebugName(attrId: Identifier, rep: RepeatSpec, end: Boolean) = {
    val arrIndexExpr = rep match {
      case NoRepeat => ""
      case _: RepeatExpr => ".arr[i]"
      case RepeatEos | _: RepeatUntil => s".arr[this.${privateMemberName(attrId)}.length${if (end) " - 1" else ""}]"
    }

    s"this._debug.${idToStr(attrId)}$arrIndexExpr"
  }

  override def blockScopeHeader: Unit = {
    out.puts("{")
    out.inc
  }

  override def blockScopeFooter(): Unit = {
    out.dec
    out.puts("}")
  }

  def kaitaiType2NativeType(attrType: DataType, isNullable: Boolean): String = {
    val typeString = attrType match {
      case Int1Type(false) => "number"
      case IntMultiType(false, Width2, _) => "number"
      case IntMultiType(false, Width4, _) => "number"
      case IntMultiType(false, Width8, _) => "number"

      case Int1Type(true) => "number"
      case IntMultiType(true, Width2, _) => "number"
      case IntMultiType(true, Width4, _) => "number"
      case IntMultiType(true, Width8, _) => "number"

      case FloatMultiType(Width4, _) => "number"
      case FloatMultiType(Width8, _) => "number"

      case BitsType(_, _) => "number"

      case _: BooleanType => "boolean"
      case CalcIntType => "number"
      case CalcFloatType => "number"

      case AnyType => "any"

      case _: StrType => "string"
      case _: BytesType => "string"

      case t: UserType => if (t.classSpec.isEmpty) {
        types2class(t.name)
      } else {
        types2class(t.classSpec.get.name)
      }

      case t: EnumType => if (t.enumSpec.isEmpty) {
        types2class(t.name)
      } else {
        types2class(t.enumSpec.get.name)
      }

      case ArrayTypeInStream(inType) => s"Array<${kaitaiType2NativeType(inType, isNullable)}>"
      case CalcArrayType(inType) => s"Array<${kaitaiType2NativeType(inType, isNullable)}>"
      case OwnedKaitaiStreamType => kstreamName
      case KaitaiStreamType => kstreamName
      case KaitaiStructType => kstructName
      case CalcKaitaiStructType => kstructName

      case st: SwitchType => st.cases.map(it => kaitaiType2NativeType(it._2, isNullable)).mkString(" | ")
    }

    if (isNullable) {
      typeString + " | undefined"
    } else {
      typeString
    }
  }
}

object TypeScriptCompiler extends LanguageCompilerStatic
  with UpperCamelCaseClasses
  with StreamStructNames
  with ExceptionNames {
  override def getCompiler(
                            tp: ClassTypeProvider,
                            config: RuntimeConfig
                          ): LanguageCompiler = new TypeScriptCompiler(tp, config)

  def idToStr(id: Identifier): String =
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => Utils.lowerCamelCase(name)
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => Utils.lowerCamelCase(name)
      case RawIdentifier(innerId) => s"_raw_${idToStr(innerId)}"
    }

  def publicMemberName(id: Identifier): String =
    id match {
      case InstanceIdentifier(name) => Utils.lowerCamelCase(name)
      case _ => idToStr(id)
    }

  override def kstreamName: String = "KaitaiStream"

  // FIXME: probably KaitaiStruct will emerge some day in TypeScript runtime, but for now it is unused
  override def kstructName: String = "unknown"

  override def ksErrorName(err: KSError): String = err match {
    case EndOfStreamError => s"KaitaiStream.EOFError"
    case _ => s"KaitaiStream.${err.name}"
  }

  def types2class(types: List[String]): String = types.map(type2class).mkString(".")
}
