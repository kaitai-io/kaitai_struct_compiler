package io.kaitai.struct.languages

import io.kaitai.struct._
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype.{CalcEndian, DataType, FixedEndian, InheritedEndian}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.{ObjcTranslator, TypeDetector}
import io.kaitai.struct.RuntimeConfig

class ObjcCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with ObjectOrientedLanguage
    with AllocateAndStoreIO
    with FixedContentsUsingArrayByteLiteral
    with UniversalDoc
    with EveryReadIsExpression {
  import ObjcCompiler._

  val importListSrc = new ImportList
  val importListHdr = new ImportList

  override val translator = new ObjcTranslator(typeProvider, importListSrc)
  val outSrcHeader = new StringLanguageOutputWriter(indent)
  val outHdrHeader = new StringLanguageOutputWriter(indent)
  val outSrc = new StringLanguageOutputWriter(indent)
  val outHdr = new StringLanguageOutputWriter(indent)

  override def results(topClass: ClassSpec): Map[String, String] = {
    val fn = topClass.nameAsStr
    Map(
      s"$fn.m" -> (outSrcHeader.result + importListToStr(importListSrc) + outSrc.result),
      s"$fn.h" -> (outHdrHeader.result + importListToStr(importListHdr) + outHdr.result)
    )
  }

  override def innerClasses = false
  override def innerEnums = false

  private def importListToStr(importList: ImportList): String =
    importList.toList.map((x) => s"#import <$x>").mkString("", "\n", "\n")
  override def indent: String = "    "

  // Members declared in io.kaitai.struct.languages.components.AllocateAndStoreIO
  override def allocateIO(id: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(id)
    val ioId = IoStorageIdentifier(id)

    val args = rep match {
      case RepeatEos | RepeatExpr(_) => s"$memberName->at($memberName->size() - 1)"
      case RepeatUntil(_) => translator.doName(Identifier.ITERATOR2)
      case NoRepeat => memberName
    }

    val newStream = s"[$kstreamName streamWithData:$args]"

    val ioName = rep match {
      case NoRepeat =>
        outSrc.puts(s"${privateMemberName(ioId)} = $newStream;")
        privateMemberName(ioId)
      case _ =>
        val localIO = s"io_${idToStr(id)}"
        outSrc.puts(s"$kstreamName* $localIO = $newStream;")
        outSrc.puts(s"${privateMemberName(ioId)}->push_back($localIO);")
        localIO
    }

    ioName
  }

  // Members declared in io.kaitai.struct.languages.components.EveryReadIsExpression
  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean): String = {
    val expr1 = padRight match {
      case Some(padByte) => s"[$expr0 KSBytesStripRightPadByte:$padByte]"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) => s"[$expr1 KSBytesTerminateTerm:$term include:$include]"
      case None => expr1
    }
    expr2
  }
  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    outSrc.puts(s"handleAssignmentRepeatEos")
  }
  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit = {
    outSrc.puts(s"handleAssignmentRepeatExpr")
  }
  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    outSrc.puts(s"handleAssignmentRepeatUntil")
  }
  override def handleAssignmentSimple(id: Identifier, dataType: Option[DataType], expr: String): Unit = {
    outHdr.puts("// comment: handleAssignmentInstance")
    outSrc.puts("// comment: handleAssignmentInstance")

    id match {
      case _: InstanceIdentifier => {
        dataType match {
          case Some(_: UserType) => outSrc.puts(s"self.${publicMemberName(id)} = $expr;")
          case Some(_: NumericType) | Some(_: BooleanType) => outSrc.puts(s"self.${publicMemberName(id)} = @($expr);")
          case _ => outSrc.puts(s"self.${publicMemberName(id)} = $expr;")
          }
        }
        case _ => outSrc.puts(s"self.${publicMemberName(id)} = $expr;")
     }
  }
  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    dataType match {
      case t: ReadableType =>
        s"($io).read_${t.apiCall(defEndian)}"
      case blt: BytesLimitType =>
        s"[$io read_bytes:(${expression(blt.size)})]"
      case _: BytesEosType =>
        s"($io).read_bytes_full"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        s"[$io read_bytes_term:$terminator include:$include consume:$consume eosErr:$eosError]"
      case BitsType1 =>
        s"[$io read_bits_int:1"
      case BitsType(width: Int) =>
        s"[$io read_bits_int:$width]"
      case t: UserType =>
        val addArgs = if (t.isOpaque) {
          ""
        } else {
          val parent = t.forcedParent match {
            case Some(USER_TYPE_NO_PARENT) => nullPtr
            case Some(fp) => translator.translate(fp)
            case None => "self"
          }
          val addEndian = t.classSpec.get.meta.endian match {
            case Some(InheritedEndian) => " withEndian: m__is_le"
            case _ => ""
          }
          s" withStruct:$parent withRoot:${privateMemberName(RootIdentifier)}$addEndian"
        }
        s"[${types2class(t.classSpec.get.name)} initialize:$io$addArgs]"
    }
  }

  // Members declared in io.kaitai.struct.languages.components.FixedContentsUsingArrayByteLiteral
  override def attrFixedContentsParse(attrName: Identifier, contents: String): Unit = {
    outSrc.puts(s"attrFixedContentsParse")
  }

  // Members declared in io.kaitai.struct.languages.components.LanguageCompiler
  override def alignToByte(io: String): Unit = {
    outSrc.puts(s"alignToByte")
  }
  override def attrParseHybrid(leProc: () => Unit,beProc: () => Unit): Unit = {
    outSrc.puts(s"attrParseHybrid(leProc: () => Unit,beProc: ")
  }
  override def attrProcess(proc: io.kaitai.struct.format.ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit = {
    outSrc.puts(s"attrProcess")
  }
  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    attrName match {
      case _: InstanceIdentifier | _: NamedIdentifier =>
        outHdr.puts("// comment: attributeDeclaration: InstanceIdentifier or NamedIdentifier")
        outSrc.puts("// comment: attributeDeclaration: InstanceIdentifier or NamedIdentifier")
        outHdr.puts(s"@property (strong,nonatomic) ${kaitaiType2NativeType(attrType, true)}${publicMemberName(attrName)};")
      case _ => {
        outHdr.puts(s"// comment: attributeDeclaration: $attrName")
        outSrc.puts(s"// comment: attributeDeclaration: $attrName")
        outHdr.puts(s"@property (strong,nonatomic) ${kaitaiType2NativeType(attrType, false)}${publicMemberName(attrName)};")
      }
    }
  }
  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    outHdr.puts("// comment: attributeReader")
    outSrc.puts("// comment: attributeReader")
  }
  override def classConstructorFooter: Unit = {
    outHdr.puts("// comment: classConstructorFooter")
    outSrc.puts("// comment: classConstructorFooter")
    outSrc.dec
    outSrc.puts(s"}")
    outSrc.puts(s"return self;")
    outSrc.dec
    outSrc.puts(s"}")
    outSrc.puts
  }

  override def classConstructorHeader(name: List[String], parentType: DataType, rootClassName: List[String], isHybrid: Boolean, params: List[io.kaitai.struct.format.ParamDefSpec]): Unit = {
    val (endianSuffixHdr, endianSuffixSrc)  = if (isHybrid) {
      (" withHybrid:(int)p_is_le", " withHybrid:p_is_le")
    } else {
      ("", "")
    }
    val paramsArg = Utils.join(params.map((p) =>
      s"${kaitaiType2NativeType(p.dataType)} ${paramName(p.id)}"
    ), "", ", ", ", ")

 //   val classNameBrief = types2class(List(name.last))

    // Parameter names
    val pIo = paramName(IoIdentifier)
    val pParent = paramName(ParentIdentifier)
    val pRoot = paramName(RootIdentifier)

    // Types
    val tIo = kaitaiType2NativeType(KaitaiStreamType)
    val tParent = kaitaiType2NativeType(parentType)
    val tRoot = kaitaiType2NativeType(CalcUserType(rootClassName, None))

    outHdr.puts("// comment: classConstructorHeader")
    outHdr.puts
    outHdr.puts(s"+ (instancetype) initialize:$paramsArg" +
      s"($tIo)$pIo " +
      s"withStruct:($tParent)$pParent " +
      s"withRoot:($tRoot)$pRoot$endianSuffixHdr;"
    )
    outHdr.puts(s"+ (instancetype) structWith:$paramsArg" +
      s"($tIo)$pIo$endianSuffixHdr;"
    )

    outSrc.puts("// comment: classConstructorHeader")
    outSrc.puts(s"+ (instancetype) initialize:$paramsArg" +
      s"($tIo)$pIo " +
      s"withStruct:($tParent)$pParent " +
      s"withRoot:($tRoot)$pRoot$endianSuffixHdr {")
    val className = types2class(name)
    outSrc.inc
    outSrc.puts(s"return [[$className alloc] initWith:$pIo withStruct:$pParent withRoot:$pRoot$endianSuffixSrc];")
    outSrc.dec
    outSrc.puts(s"}")
    outSrc.puts

    outSrc.puts(s"+ (instancetype) structWith:$paramsArg" +
      s"($tIo)$pIo$endianSuffixHdr {")
    outSrc.inc
    outSrc.puts(s"return [[$className alloc] initWith:$pIo withStruct:nil withRoot:nil$endianSuffixSrc];")
    outSrc.dec
    outSrc.puts(s"}")
    outSrc.puts

    outSrc.puts(s"- (instancetype) initWith:$paramsArg" +
      s"($tIo)$pIo " +
      s"withStruct:($tParent)$pParent " +
      s"withRoot:($tRoot)$pRoot$endianSuffixHdr {")
    outSrc.inc
    outSrc.puts(s"self = [super initWith:$pIo withStruct:$pParent withRoot:$pRoot$endianSuffixSrc];")
    outSrc.puts(s"if (self) {")
    outSrc.inc
  }

  override def opaqueClassDeclaration(classSpec: ClassSpec): Unit = {
    outHdr.puts("// comment: opaqueClassDeclaration")
    outSrc.puts("// comment: opaqueClassDeclaration")
    classForwardDeclaration(classSpec.name)
  }
  override def classForwardDeclaration(name: List[String]): Unit = {
    outHdrHeader.puts("// comment: classForwardDeclaration")
    outSrcHeader.puts("// comment: classForwardDeclaration")
    outHdrHeader.puts(s"@class ${types2class(name)};")
  }
  override def classFooter(name: List[String]): Unit = {
    outHdr.puts("// comment: classFooter")
    outSrc.puts("// comment: classFooter")
    outHdr.puts("@end")
    outSrc.puts("@end")
  }
  override def classHeader(name: List[String]): Unit = {
    val className = types2class(name)

    outHdr.puts("// comment: classHeader")
    outHdr.puts
    //classForwardDeclaration(name)
    outHdr.puts(s"@interface $className : $kstructName")

    outSrc.puts("// comment: classHeader")
    outSrc.puts
    outSrc.puts(s"@implementation $className")
    outSrc.puts(s"@dynamic _root;")
    outSrc.puts(s"@dynamic _parent;")
  }
  override def condIfFooter(expr: Ast.expr): Unit = {
    outHdr.puts("// comment: condIfFooter")
    outSrc.puts("// comment: condIfFooter")
    outSrc.dec
    outSrc.puts("}")
  }
  override def condIfHeader(expr: Ast.expr): Unit = {
    outHdr.puts("// comment: condIfHeader")
    outSrc.puts("// comment: condIfHeader")
    outSrc.puts(s"if (${expression(expr)}) {")
    outSrc.inc
  }
  override def condRepeatEosFooter: Unit = {
    outSrc.puts(s"condRepeatEosFooter")
  }
  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean): Unit = {
    outSrc.puts(s"condRepeatEosHeader")
  }
  override def condRepeatExprFooter: Unit = {
    outSrc.puts(s"condRepeatExprFooter")
  }
  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: Ast.expr): Unit = {
    outSrc.puts(s"condRepeatExprHeader")
  }
  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: Ast.expr): Unit = {
    outSrc.puts(s"condRepeatUntilFooter")
  }
  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: Ast.expr): Unit = {
    outSrc.puts(s"condRepeatUntilHeader")
  }
  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, io.kaitai.struct.format.EnumValueSpec)]): Unit = {
    outSrc.puts(s"enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[")
  }
  override def fileHeader(topClassName: String): Unit = {
    outSrcHeader.puts(s"// comment: fileHeader")
    outSrcHeader.puts(s"// $headerComment")
    outSrcHeader.puts
    outSrcHeader.puts("#import \"" + outFileName(topClassName) + ".h\"")

    outHdrHeader.puts(s"// comment: fileHeader")
    outHdrHeader.puts(s"#ifndef ${defineName(topClassName)}")
    outHdrHeader.puts(s"#define ${defineName(topClassName)}")

    outHdrHeader.puts
    outHdrHeader.puts(s"// $headerComment")
    outHdrHeader.puts
    outHdrHeader.puts("#import \"kaitai/kaitaistruct.h\"")
    outHdrHeader.puts

    // API compatibility check
    outHdr.puts("// comment: fileHeader")
    val minVer = KSVersion.minimalRuntime.toInt
    outHdr.puts
    outHdr.puts(s"#if KAITAI_STRUCT_VERSION < ${minVer}L")
    outHdr.puts(
      "#error \"Incompatible Kaitai Struct Objective C/Cocoa API: version " +
        KSVersion.minimalRuntime + " or later is required\""
    )
    outHdr.puts("#endif")
  }
  override def fileFooter(topClassName: String): Unit = {
    outHdr.puts("// comment: fileFooter")
    outSrc.puts("// comment: fileFooter")

    outHdr.puts
    outHdr.puts(s"#endif  // ${defineName(topClassName)}")
  }
  override def instanceCheckCacheAndReturn(instName: io.kaitai.struct.format.InstanceIdentifier, dataType: DataType): Unit = {
    outHdr.puts("// comment: instanceCheckCacheAndReturn")
    outSrc.puts("// comment: instanceCheckCacheAndReturn")
    outSrc.puts(s"if (${instancePrivateMemberName(instName)})")
    outSrc.inc
    outSrc.puts(s"return ${instancePrivateMemberName(instName)};")
    outSrc.dec
  }
  override def instanceFooter: Unit = {
    outHdr.puts("// comment: instanceFooter")
    outSrc.puts("// comment: instanceFooter")
    outSrc.dec
    outSrc.puts("}")
  }
  override def instanceHeader(className: List[String], instName: io.kaitai.struct.format.InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    outHdr.puts("// comment: instanceHeader")
    outSrc.puts("// comment: instanceHeader")
    outSrc.puts(s"-(${kaitaiType2NativeType(dataType, true)}) ${publicMemberName(instName)} {")
    outSrc.inc
  }
  override def instanceReturn(instName: io.kaitai.struct.format.InstanceIdentifier, attrType: DataType): Unit = {
    outHdr.puts("// comment: instanceReturn")
    outSrc.puts("// comment: instanceReturn")
    outSrc.puts(s"return ${instancePrivateMemberName(instName)};")
  }
  override def outFileName(topClassName: String): String = topClassName
  override def popPos(io: String): Unit =
    outSrc.puts(s"[$io seek:_pos];")
  override def pushPos(io: String): Unit =
    outSrc.puts(s"unsigned long long _pos = $io.pos;")

  override def readFooter(): Unit = {
    outHdr.puts("// comment: readFooter")
    outSrc.puts("// comment: readFooter")
    outSrc.dec
    outSrc.puts("}")
  }
  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = {
    outHdr.puts("// comment: readHeader")
    outSrc.puts("// comment: readHeader")
    val suffix = endian match {
      case Some(e) => s"_${e.toSuffix}"
      case None => ""
    }

    outSrc.puts
    outSrc.puts(s"- (void) _read$suffix {")
    outSrc.inc
  }
  override def runRead(): Unit = {
    outHdr.puts("// comment: runRead")
    outSrc.puts("// comment: runRead")
    outSrc.puts("[self _read];")
  }
  override def runReadCalc(): Unit = {
    outSrc.puts
    outSrc.puts("if (m__is_le == -1) {")
    outSrc.inc
    importListSrc.add("stdexcept")
    outSrc.puts("[NSException raise:@\"unable to decide on endianness\" format @\"\"]")
    outSrc.dec
    outSrc.puts("} else if (m__is_le == 1) {")
    outSrc.inc
    outSrc.puts("[self _read_le];")
    outSrc.dec
    outSrc.puts("} else {")
    outSrc.inc
    outSrc.puts("[self _read_be];")
    outSrc.dec
    outSrc.puts("}")
  }
  override def seek(io: String, pos: Ast.expr): Unit =
    outSrc.puts(s"[$io seek:${expression(pos)}];")

  override def type2class(className: String): String = s"type2class"
  override def useIO(ioEx: Ast.expr): String = s"useIO"

  // Members declared in io.kaitai.struct.languages.components.ObjectOrientedLanguage
  override def idToStr(id: Identifier): String = {
    id match {
      case RawIdentifier(inner) => s"_raw_${idToStr(inner)}"
      case IoStorageIdentifier(inner) => s"_io_${idToStr(inner)}"
      case si: SpecialIdentifier => si.name
      case ni: NamedIdentifier => ni.name
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case ni: InstanceIdentifier => ni.name
    }
  }
  override def localTemporaryName(id: Identifier): String = s"localTemporaryName"
  override def privateMemberName(id: Identifier): String = s"self.${idToStr(id)}"
  def instancePrivateMemberName(id: Identifier): String = s"_${idToStr(id)}"
//  def instancePrivateMemberAccessName(id: Identifier): String = s"self._p_${idToStr(id)}"
  override def publicMemberName(id: Identifier): String = idToStr(id)

  // Members declared in io.kaitai.struct.languages.components.SwitchOps
  override def switchCaseEnd(): Unit = {
    outSrc.puts(s"switchCaseEnd")
  }
  override def switchCaseStart(condition: Ast.expr): Unit = {
    outSrc.puts(s"switchCaseStart")
  }
  override def switchElseStart(): Unit = {
    outSrc.puts(s"switchElseStart")
  }
  override def switchEnd(): Unit = {
    outSrc.puts(s"switchEnd")
  }
  override def switchStart(id: Identifier, on: Ast.expr): Unit = {
    outSrc.puts(s"switchStart")
  }

  // Members declared in io.kaitai.struct.languages.components.UniversalDoc
  override def universalDoc(doc: DocSpec): Unit = {
    outHdr.puts("// comment: universalDoc")
    outSrc.puts("// comment: universalDoc")
    outHdr.puts
    outHdr.puts( "/**")

    doc.summary.foreach(docStr => outHdr.putsLines(" * ", docStr))

    doc.ref.foreach {
      case TextRef(text) =>
        outHdr.putsLines(" * ", s"\\sa $text")
      case UrlRef(url, text) =>
        outHdr.putsLines(" * ", s"\\sa $url $text")
    }

    outHdr.puts( " */")
  }

  def defineName(className: String) = className.toUpperCase + "_H_"

  def kaitaiType2NativeType(attrType: DataType, absolute: Boolean = false): String =
    ObjcCompiler.kaitaiType2NativeType(attrType, absolute)

  override def paramName(id: Identifier): String = s"p_${idToStr(id)}"

  def nullPtr: String = "0"
}

object ObjcCompiler extends LanguageCompilerStatic with StreamStructNames {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new ObjcCompiler(tp, config)

  override def kstructName = "kstruct"
  override def kstreamName = "kstream"

  def kaitaiType2NativeType(attrType: DataType, absolute: Boolean = false): String = {
    attrType match {
      case Int1Type(false) => "NSNumber *"
      case IntMultiType(false, Width2, _) => "NSNumber *"
      case IntMultiType(false, Width4, _) => "NSNumber *"
      case IntMultiType(false, Width8, _) => "NSNumber *"

      case Int1Type(true) => "NSNumber *"
      case IntMultiType(true, Width2, _) => "NSNumber *"
      case IntMultiType(true, Width4, _) => "NSNumber *"
      case IntMultiType(true, Width8, _) => "NSNumber *"

      case FloatMultiType(Width4, _) => "NSNumber *"
      case FloatMultiType(Width8, _) => "NSNumber *"

      case BitsType(_) => "NSNumber *"

      case _: BooleanType => "NSNumber *"
      case CalcIntType => "NSNumber *"
      case CalcFloatType => "NSNumber *"

      case _: StrType => "NSString *"
      case _: BytesType => "NSData *"

      case t: UserType =>
        types2class(if (absolute) {
          t.classSpec.get.name
        } else {
          t.name
        }) + " *"

      case t: EnumType =>
        types2class(if (absolute) {
          t.enumSpec.get.name
        } else {
          t.name
        })

      case ArrayType(inType) => s"NSMutableArray <${kaitaiType2NativeType(inType, absolute)}> *"
      case CalcArrayType(inType) => s"NSMutableArray <${kaitaiType2NativeType(inType, absolute)}> *"

      case KaitaiStreamType => s"$kstreamName *"
      case KaitaiStructType => s"$kstructName *"
      case CalcKaitaiStructType => s"$kstructName *"

      case st: SwitchType =>
        kaitaiType2NativeType(combineSwitchType(st), absolute)
    }
  }

  /**
    * C does not have a concept of AnyType, and common use case "lots of
    * incompatible UserTypes for cases + 1 BytesType for else" combined would
    * result in exactly AnyType - so we try extra hard to avoid that here with
    * this pre-filtering. In C, "else" case with raw byte array would
    * be available through _raw_* attribute anyway.
    *
    * @param st switch type to combine into one overall type
    * @return
    */
  def combineSwitchType(st: SwitchType): DataType = {
    val ct1 = TypeDetector.combineTypes(
      st.cases.filterNot {
        case (caseExpr, _) => caseExpr == SwitchType.ELSE_CONST
      }.values
    )
    if (st.isOwning) {
      ct1
    } else {
      ct1.asNonOwning
    }
  }

  def types2class(components: List[String]) =
    type2class(components.reverse.mkString("_"))

  def type2class(name: String) = name + "_t"
}