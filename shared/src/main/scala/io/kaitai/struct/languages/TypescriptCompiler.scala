package io.kaitai.struct.languages

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.JavaScriptTranslator
import io.kaitai.struct.{ClassTypeProvider, ImportList, RuntimeConfig, StringLanguageOutputWriter, Utils}

class TypescriptCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with ObjectOrientedLanguage
    with UpperCamelCaseClasses
    with SingleOutputFile
    with UniversalDoc
    with AllocateIOLocalVar
    with EveryReadIsExpression
    with SwitchIfOps
    with FixedContentsUsingArrayByteLiteral {

  import TypescriptCompiler._

  override def innerClasses: Boolean = false
  override def innerEnums: Boolean = false

  override def outFileName(topClassName: String): String = s"${type2class(topClassName)}.d.ts"

  override def outImports(topClass: ClassSpec) = {
    importList.toList.map(i => s"import ${i} from './${i}';").mkString(" ")
  }

  override val translator = new JavaScriptTranslator(typeProvider)

  override def indent: String = "  "

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"// $headerComment")
    outHeader.puts
  }

  override def fileFooter(name: String): Unit = {
    out.puts(s"export = ${type2class(name)};")
    out.puts(s"export as namespace ${type2class(name)};")
  }

  override def opaqueClassDeclaration(classSpec: ClassSpec): Unit = {
    val className = type2class(classSpec.name.head)
    importList.add(className)
  }

  def beginNamespace(names: List[String]): Unit = {
    names.zipWithIndex.foreach { case (name, i) =>
      out.puts(s"${if (i == 0) "declare " else  ""}namespace ${type2class(name)} {")
      out.inc
    }
  }

  def endNamespace(names: List[String]): Unit = {
    1.to(names.length).foreach { _ =>
      out.dec
      out.puts("}")
    }
  }

  override def classHeader(name: List[String]): Unit = {
    val namespace = name.init
    beginNamespace(namespace)

    val shortClassName = type2class(name.last)

    out.puts(s"${if (name.length == 1) "declare " else ""}class ${shortClassName} {")
    out.inc
    out.puts("constructor(io: any, parent?: any, root?: any);");
    out.puts(s"__type: '${shortClassName}';")
  }

  override def classFooter(name: List[String]): Unit = {
    out.dec
    out.puts("}")
    endNamespace(name.init)
    out.puts
  }

  def kaitaiType2NativeType(attrType: DataType): String = {
    attrType match {
      case _: Int1Type |
           _: IntMultiType |
           _: IntMultiType |
           _: IntMultiType |
           _: IntMultiType |
           _: IntMultiType |
           _: IntMultiType |
           _: FloatMultiType |
           _: BitsType |
           CalcIntType |
           CalcFloatType => "number"

      case _: BooleanType => "boolean"

      case _: StrType => "string"
      case _: BytesType => "Uint8Array"

      case t: UserType => t.classSpec match {
        case Some(cs) => types2class(cs.name)
        case None => types2class(t.name)
      }

      case t: EnumType => t.enumSpec match {
        case Some(cs) => types2class(cs.name)
        case None => types2class(t.name)
      }

      case at: ArrayType => s"${kaitaiType2NativeType(at.elType)}[]"

      case KaitaiStreamType | OwnedKaitaiStreamType => "any"
      case KaitaiStructType | CalcKaitaiStructType => "any"

      case st: SwitchType => s"${st.cases.values.map(kaitaiType2NativeType).mkString(" | ")} | undefined"
    }
  }

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    attrName match {
      case ParentIdentifier | RootIdentifier | IoIdentifier =>
      // just ignore it for now
      case _ =>
        out.puts(s"${idToStr(attrName)}: ${kaitaiType2NativeType(attrType)};")
    }
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

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, EnumValueSpec)]): Unit = {
    beginNamespace(curClass)

    out.puts(s"enum ${type2class(enumName)} {")
    out.inc

    // Name to ID mapping
    enumColl.foreach { case (id, label) =>
      out.puts(s"${enumValue(enumName, label.name)} = $id,")
    }

    out.dec
    out.puts("}")

    endNamespace(curClass)
    out.puts
  }

  def enumValue(enumName: String, label: String) = Utils.upperUnderscoreCase(label)

  /**
   * Renders identifier to a string, specifically for a given
   * language and settings. This usually includes things like
   * case and separator conversion and does *not* include things
   * like prepending "@" or "this." or "self." that might be
   * used to access private member.
   *
   * @param id identifier to render
   * @return identifier as string
   */
  override def idToStr(id: Identifier): String = {
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => Utils.lowerCamelCase(name)
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => s"_m_${Utils.lowerCamelCase(name)}"
      case RawIdentifier(innerId) => "_raw_" + idToStr(innerId)
    }
  }

  override def privateMemberName(id: Identifier): String = ""

  override def publicMemberName(id: Identifier): String = {
    id match {
      case NamedIdentifier(name) => Utils.lowerCamelCase(name)
      case InstanceIdentifier(name) => Utils.lowerCamelCase(name)
    }
  }

  override def localTemporaryName(id: Identifier): String = ""

  override def ksErrorName(err: KSError): String = TypescriptCompiler.ksErrorName(err)

  override def classConstructorHeader(name: List[String], parentType: DataType, rootClassName: List[String], isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {}

  override def classConstructorFooter: Unit = {}

  override def runRead(name: List[String]): Unit = {}

  override def runReadCalc(): Unit = {}

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = {}

  override def readFooter(): Unit = {}

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {}

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {}

  override def condIfHeader(expr: expr): Unit = {}

  override def condIfFooter(expr: expr): Unit = {}

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw): Unit = {}

  override def condRepeatEosFooter: Unit = {}

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw, repeatExpr: expr): Unit = {}

  override def condRepeatExprFooter: Unit = {}

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw, repeatExpr: expr): Unit = {}

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: NeedRaw, repeatExpr: expr): Unit = {}

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec): Unit = {}

  override def useIO(ioEx: expr): String = ""

  override def pushPos(io: String): Unit = {}

  override def seek(io: String, pos: expr): Unit = {}

  override def popPos(io: String): Unit = {}

  override def alignToByte(io: String): Unit = {}

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"${publicMemberName(instName)}: ${kaitaiType2NativeType(dataType)};")
  }

  override def instanceFooter: Unit = {}

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {}

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit = {}

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = ""

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {

  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit = {

  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
  }

  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = ""

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean): String = ""

  /**
   * Determines if this particular implementation of switches would be ok with true
   * built-in `switch` mechanism, or it will require `if`-based emulation.
   *
   * @param onType type we'll be switching over
   * @return true if `if`-based emulation is required
   */
  override def switchRequiresIfs(onType: DataType): Boolean = false

  override def switchIfStart(id: Identifier, on: expr, onType: DataType): Unit = {}

  override def switchIfCaseStart(condition: expr): Unit = {}

  override def switchIfCaseEnd(): Unit = {}

  override def switchIfElseStart(): Unit = {}

  override def switchIfEnd(): Unit = {}

  override def attrFixedContentsParse(attrName: Identifier, contents: String): Unit = {}

  override def switchStart(id: Identifier, on: expr): Unit = {}

  override def switchCaseStart(condition: expr): Unit = {}

  override def switchCaseEnd(): Unit = {}

  override def switchElseStart(): Unit = {}

  override def switchEnd(): Unit = {}
}

object TypescriptCompiler extends LanguageCompilerStatic
  with UpperCamelCaseClasses
  with StreamStructNames
  with ExceptionNames {
  override def getCompiler(
                            tp: ClassTypeProvider,
                            config: RuntimeConfig
                          ): LanguageCompiler = new TypescriptCompiler(tp, config)

  override def kstreamName: String = "KaitaiStream"

  // FIXME: probably KaitaiStruct will emerge some day in JavaScript runtime, but for now it is unused
  override def kstructName: String = ???

  override def ksErrorName(err: KSError): String = err match {
    case EndOfStreamError => s"KaitaiStream.EOFError"
    case _ => s"KaitaiStream.${err.name}"
  }

  def types2class(types: List[String]): String = types.map(type2class).mkString(".")
}
