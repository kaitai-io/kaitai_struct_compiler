package io.kaitai.struct.languages

import io.kaitai.struct._
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype.{CalcEndian, DataType, FixedEndian, InheritedEndian}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.{ObjcTranslator, TypeDetector}
import io.kaitai.struct.RuntimeConfig

class ObjcCompiler(
  val typeProvider: ClassTypeProvider,
  config: RuntimeConfig
) extends LanguageCompiler(typeProvider, config)
    with ObjectOrientedLanguage
    with AllocateAndStoreIO
    with FixedContentsUsingArrayByteLiteral
    with UniversalDoc
    with SwitchIfOps
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

  override def switchRequiresIfs(onType: DataType): Boolean = onType match {
    case _: IntType => false
    case _ => true
  }

  override def switchIfStart(id: Identifier, on: Ast.expr, onType: DataType): Unit = {
    outSrc.puts("{")
    outSrc.inc
    outSrc.puts(s"${kaitaiType2NativeType(onType)}on = ${expression(on)};")
  }

  override def switchIfCaseFirstStart(condition: Ast.expr): Unit = {
    outHdr.puts("// comment: switchIfCaseFirstStart")
    outSrc.puts(s"// comment: switchIfCaseFirstStart: $condition")
    outSrc.puts(s"if ([on isEqualToData:${expression(condition)}]) {")
    outSrc.inc
  }

  override def switchIfCaseStart(condition: Ast.expr): Unit = {
    outHdr.puts("// comment: switchIfCaseStart")
    outSrc.puts(s"// comment: switchIfCaseStart: $condition")
    outSrc.puts(s"else if ([on isEqualToData:${expression(condition)}]) {")
    outSrc.inc
  }

  override def switchIfCaseEnd(): Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def switchIfElseStart(): Unit = {
    outSrc.puts("else {")
    outSrc.inc
  }

  override def switchIfEnd(): Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

//  override def typeProvider: io.kaitai.struct.ClassTypeProvider = typeProvider

  override def innerClasses = false
  override def innerEnums = true

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
    outHdr.puts("// comment: handleAssignmentRepeatEos")
    outSrc.puts("// comment: handleAssignmentRepeatEos")

    outSrc.puts(s"[${privateMemberName(id)} addObject:$expr];")
  }
  override def handleAssignmentRepeatExpr(id: Identifier, dataType: Option[DataType], expr: String): Unit = {
    outHdr.puts("// comment: handleAssignmentRepeatExpr")
    outSrc.puts(s"// comment: handleAssignmentRepeatExpr, i: $id, d: $dataType, e: $expr")
    dataType match {
      case Some(_: NumericType) | Some(_: BooleanType) => outSrc.puts(s"[${privateMemberName(id)} addObject:@($expr)];")
      case _ => outSrc.puts(s"[${privateMemberName(id)} addObject:$expr];")
    }
  }
  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    outHdr.puts("// comment: handleAssignmentRepeatUntil")
    outSrc.puts("// comment: handleAssignmentRepeatUntil")
    val (typeDecl, tempVar) = if (isRaw) {
      ("std::string ", translator.doName(Identifier.ITERATOR2))
    } else {
      ("", translator.doName(Identifier.ITERATOR))
    }

    val (wrappedTempVar, rawPtrExpr) = (tempVar, expr)

    outSrc.puts(s"$typeDecl$tempVar = $rawPtrExpr;")

    outSrc.puts(s"[${privateMemberName(id)} addObject:$wrappedTempVar];")
  }
  override def handleAssignmentSimple(id: Identifier, dataType: Option[DataType], expr: String): Unit = {
    outHdr.puts("// comment: handleAssignmentInstance")
    outSrc.puts(s"// comment: handleAssignmentInstance: Identifier: $id, DataType: $dataType")
    id match {
      case SpecialIdentifier(n) if (n == "_is_le") => outSrc.puts(s"self.${publicMemberName(id)} = $expr;")
      case _ => dataType match {
        case Some(_: NumericType) | Some(_: BooleanType) => outSrc.puts(s"self.${publicMemberName(id)} = @($expr);")
        case _ => outSrc.puts(s"self.${publicMemberName(id)} = $expr;")
      }
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
        s"[$io read_bits_int:1]"
      case BitsType(width: Int) =>
        s"[$io read_bits_int:$width]"
      case t: UserType =>
        val addParams = Utils.join(t.args.zipWithIndex.map { case (a, i) =>
          s"withParam$i:@(${translator.translate(a)})"}," ", " ", "")
        val addArgs = if (t.isOpaque) {
          ""
        } else {
          val parent = t.forcedParent match {
            case Some(USER_TYPE_NO_PARENT) => nullPtr
            case Some(fp) => translator.translate(fp)
            case None => "self"
          }
          val addEndian = t.classSpec.get.meta.endian match {
            case Some(InheritedEndian) => " withEndian: self._is_le"
            case _ => ""
          }
          s" withStruct:$parent withRoot:${privateMemberName(RootIdentifier)}$addEndian"
        }
        s"[${types2class(t.classSpec.get.name)} initialize:$io$addArgs$addParams]"
    }
  }

  // Members declared in io.kaitai.struct.languages.components.FixedContentsUsingArrayByteLiteral
  override def attrFixedContentsParse(attrName: Identifier, contents: String): Unit = {
    outHdr.puts("// comment: attrFixedContentsParse")
    outSrc.puts("// comment: attrFixedContentsParse")
    outSrc.puts(s"${privateMemberName(attrName)} = [$normalIO ensure_fixed_contents:$contents];")
  }

  // Members declared in io.kaitai.struct.languages.components.LanguageCompiler
  override def alignToByte(io: String): Unit = {
    outSrc.puts(s"[self.${idToStr(IoIdentifier)} alignToByte];")
  }
  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {
    outSrc.puts("if (self._is_le == YES) {")
    outSrc.inc
    leProc()
    outSrc.dec
    outSrc.puts("} else {")
    outSrc.inc
    beProc()
    outSrc.dec
    outSrc.puts("}")
  }
  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit = {
    outHdr.puts("// comment: attrProcess")
    outSrc.puts("// comment: attrProcess")
    val srcName = privateMemberName(varSrc)
    val destName = privateMemberName(varDest)

    proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "KSProcessXorOneWithKey"
          case _: BytesType => "KSProcessXorManyWithKey"
        }
        outSrc.puts(s"$destName = [$srcName $procName:${expression(xorValue)}];")
      case ProcessZlib =>
        outSrc.puts(s"$destName = [$srcName KSProcess_zlib];")
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        outSrc.puts(s"$destName = [$srcName KSProcessRotateLeftWithAmount:$expr];")
      case ProcessCustom(name, args) =>
//        val procClass = name.map((x) => type2class(x)).mkString("::")
        val procClass = type2class(name.last)
        val procName = s"_process_${idToStr(varSrc)}"

        importListSrc.add(name.last + ".h")

        outSrc.puts(s"$procClass *$procName = [[$procClass alloc] initWith:${args.map(expression).mkString(", ")}];")
        outSrc.puts(s"$destName = [$procName decode:$srcName];")
    }
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
    outHdr.puts(s"// comment: classConstructorHeader: $params")
    outSrc.puts(s"// comment: classConstructorHeader: $params")

    val (endianSuffixHdr, endianSuffixSrc)  = if (isHybrid) {
      (" withEndian:(int)p_is_le", " withEndian:p_is_le")
    } else {
      ("", "")
    }

    val classParamsArg = Utils.join(params.zipWithIndex.map { case (p, i) =>
      s"withParam$i:(${kaitaiType2NativeType(p.dataType)})${paramName(p.id)}"}," ", " ", "")

    val initParamsArg = Utils.join(params.zipWithIndex.map { case (p, i) =>
      s"withParam$i:${paramName(p.id)}"}," ", " ", "")

    val assignParamsArg = Utils.join(params.map((p) =>
      s"self.${publicMemberName(p.id)} = ${paramName(p.id)};"
    ), "", "\r", "")


    // Parameter names
    val pIo = paramName(IoIdentifier)
    val pParent = paramName(ParentIdentifier)
    val pRoot = paramName(RootIdentifier)

    // Types
    val tIo = kaitaiType2NativeType(KaitaiStreamType)
    val tParent = kaitaiType2NativeType(parentType)
    val tRoot = kaitaiType2NativeType(CalcUserType(rootClassName, None))

    outHdr.puts
    outHdr.puts(s"+ (instancetype) initialize:" +
      s"($tIo)$pIo " +
      s"withStruct:($tParent)$pParent " +
      s"withRoot:($tRoot)$pRoot$endianSuffixHdr$classParamsArg;"
    )
    outHdr.puts(s"+ (instancetype) initialize:" +
      s"($tIo)$pIo$endianSuffixHdr$classParamsArg;"
    )

    outSrc.puts(s"+ (instancetype) initialize:" +
      s"($tIo)$pIo " +
      s"withStruct:($tParent)$pParent " +
      s"withRoot:($tRoot)$pRoot$endianSuffixHdr$classParamsArg {")
    val className = types2class(name)
    outSrc.inc
    outSrc.puts(s"return [[$className alloc] initWith:$pIo withStruct:$pParent withRoot:$pRoot$endianSuffixSrc$initParamsArg];")
    outSrc.dec
    outSrc.puts(s"}")
    outSrc.puts

    outSrc.puts(s"+ (instancetype) initialize:" +
      s"($tIo)$pIo$endianSuffixHdr$classParamsArg {")
    outSrc.inc
    outSrc.puts(s"return [[$className alloc] initWith:$pIo withStruct:nil withRoot:nil$endianSuffixSrc$initParamsArg];")
    outSrc.dec
    outSrc.puts(s"}")
    outSrc.puts

    outSrc.puts(s"- (instancetype) initWith:" +
      s"($tIo)$pIo " +
      s"withStruct:($tParent)$pParent " +
      s"withRoot:($tRoot)$pRoot$endianSuffixHdr$classParamsArg {")
    outSrc.inc
    outSrc.puts(s"self = [super initWith:$pIo withStruct:$pParent withRoot:$pRoot$endianSuffixSrc];")
    outSrc.puts(s"if (self) {")
    outSrc.inc
    outSrc.puts(assignParamsArg)

    typeProvider.nowClass.meta.endian match {
      case Some(_: CalcEndian) | Some(InheritedEndian) =>
        outHdr.puts("@property int _is_le;")
      case _ =>
        // no _is_le variable
    }
  }

  override def opaqueClassDeclaration(classSpec: ClassSpec): Unit = {
    outHdr.puts("// comment: opaqueClassDeclaration")
    outSrc.puts("// comment: opaqueClassDeclaration")
    classForwardDeclaration(classSpec.name)
    outHdr.puts("#import \"" + outFileName(classSpec.name.head) + ".h\"")
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
    outSrc.puts(s"@dynamic _is_le;")
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
    outHdr.puts("// comment: condRepeatEosFooter")
    outSrc.puts("// comment: condRepeatEosFooter")

    outSrc.puts("i++;")
    outSrc.dec
    outSrc.puts("}")
    outSrc.dec
    outSrc.puts("}")
  }
  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean): Unit = {
    outHdr.puts("// comment: condRepeatEosHeader")
    outSrc.puts("// comment: condRepeatEosHeader")

    outSrc.puts(s"${privateMemberName(id)} = ${newVector(dataType, None)};")
    outSrc.puts("{")
    outSrc.inc
    outSrc.puts("int i = 0;")
    outSrc.puts(s"while (!($io).isEof) {")
    outSrc.inc
  }
  override def condRepeatExprFooter: Unit = {
    outHdr.puts("// comment: condRepeatExprFooter")
    outSrc.puts("// comment: condRepeatExprFooter")
    outSrc.dec
    outSrc.puts("}")
  }
  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: Ast.expr): Unit = {
    outHdr.puts("// comment: condRepeatExprHeader")
    outSrc.puts("// comment: condRepeatExprHeader")
    val lenVar = s"l_${idToStr(id)}"
    outSrc.puts(s"int $lenVar = ${expression(repeatExpr)};")
    if (needRaw) {
      val rawId = privateMemberName(RawIdentifier(id))
      outSrc.puts(s"$rawId = ${newVector(CalcBytesType, Some(lenVar))};")
      val ioId = privateMemberName(IoStorageIdentifier(RawIdentifier(id)))
      outSrc.puts(s"$ioId = ${newVector(KaitaiStreamType, Some(lenVar))};")
    }
    outSrc.puts(s"${privateMemberName(id)} = ${newVector(dataType, Some(lenVar))};")
    outSrc.puts(s"for (int i = 0; i < $lenVar; i++) {")
    outSrc.inc
  }

  def newVector(elType: DataType, length: Option[String]): String = {
    length match {
      case Some(t) => s"[NSMutableArray arrayWithCapacity:$t]"
      case _ => s"[NSMutableArray array]"
    }
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, untilExpr: Ast.expr): Unit = {
    outHdr.puts("// comment: condRepeatUntilFooter")
    outSrc.puts("// comment: condRepeatUntilFooter")
    typeProvider._currentIteratorType = Some(dataType)
    outSrc.puts("i++;")
    outSrc.dec
    outSrc.puts(s"} while (!(${expression(untilExpr)}));")
    outSrc.dec
    outSrc.puts("}")
  }
  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, untilExpr: Ast.expr): Unit = {
    outHdr.puts("// comment: condRepeatUntilHeader")
    outSrc.puts("// comment: condRepeatUntilHeader")
    if (needRaw) {
      outSrc.puts(s"${privateMemberName(RawIdentifier(id))} = ${newVector(CalcBytesType, None)};")
      outSrc.puts(s"${privateMemberName(IoStorageIdentifier(RawIdentifier(id)))} = ${newVector(KaitaiStreamType, None)};")
    }
    outSrc.puts(s"${privateMemberName(id)} = ${newVector(dataType, None)};")
    outSrc.puts("{")
    outSrc.inc
    outSrc.puts("int i = 0;")
    outSrc.puts(s"${kaitaiType2NativeType(dataType.asNonOwning, true)} ${translator.doName("_")};")
    outSrc.puts("do {")
    outSrc.inc
  }
  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, EnumValueSpec)]): Unit = {
    outHdr.puts("// comment: enumDeclaration")
    outSrc.puts("// comment: enumDeclaration")

    val enumInstName = enumPropertyName(enumName)
    val enumPriInstName = enumPrivatePropertyName(curClass :+ enumName)

    outHdr.puts(s"@property (class,strong,nonatomic,readonly) NSDictionary *$enumInstName;")
    outHdr.puts
    outHdr.puts
    outSrc.puts(s"static NSDictionary *$enumPriInstName = nil;")
    outSrc.puts(s"+ (NSDictionary *)$enumInstName {")
    outSrc.inc
    outSrc.puts(s" if($enumPriInstName == nil) {")
    outSrc.inc
    outSrc.puts(s"$enumPriInstName = @{")
    outSrc.inc

    if (enumColl.size > 1) {
      enumColl.dropRight(1).foreach { case (id, label) =>
        outSrc.puts("@\"" + s"${label.name}" + "\"" + s" : @($id),")
      }
    }
    enumColl.last match {
      case (id, label) =>
        outSrc.puts("@\"" + s"${label.name}" + "\"" + s" : @($id)")
    }
    outSrc.dec
    outSrc.puts("};")
    outSrc.dec
    outSrc.puts("}")
    outSrc.puts(s"return $enumPriInstName;")
    outSrc.dec
    outSrc.puts("}")
  }

  def enumPropertyName(s: String): String = "_" + s
  def enumPrivatePropertyName(s: List[String]): String = "_" + s.reverse.mkString("_")

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
    outSrc.puts("if (self._is_le == -1) {")
    outSrc.inc
    outSrc.puts("[NSException raise:@\"unable to decide on endianness\" format:@\"\"];")
    outSrc.dec
    outSrc.puts("} else if (self._is_le == YES) {")
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

  override def type2class(className: String): String = ObjcCompiler.type2class(className)
  override def useIO(ioEx: Ast.expr): String = {
    outSrc.puts(s"$kstreamName *io = ${expression(ioEx)};")
    "io"
  }

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
    outHdr.puts("// comment: switchCaseEnd")
    outSrc.puts("// comment: switchCaseEnd")
    outSrc.puts("break;")
    outSrc.dec
    outSrc.puts("}")
  }
  override def switchCaseStart(condition: Ast.expr): Unit = {
    outHdr.puts("// comment: switchCaseStart")
    outSrc.puts("// comment: switchCaseStart")
    outSrc.puts(s"case ${expression(condition)}: {")
    outSrc.inc
  }
  override def switchElseStart(): Unit = {
    outHdr.puts("// comment: switchElseStart")
    outSrc.puts("// comment: switchElseStart")
    outSrc.puts("default: {")
    outSrc.inc
  }
  override def switchEnd(): Unit = {
    outHdr.puts("// comment: switchEnd")
    outSrc.puts("// comment: switchEnd")
    outSrc.puts("}")
  }
  override def switchStart(id: Identifier, on: Ast.expr): Unit = {
    outHdr.puts("// comment: switchStart")
    outSrc.puts("// comment: switchStart")
    outSrc.puts(s"switch (${expression(on)}) {")
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

  def nullPtr: String = "nil"
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

      case _: EnumType => "NSDictionary *"

      case ArrayType(inType) => s"NSMutableArray <${kaitaiType2NativeType(inType, absolute)}> *"
      case CalcArrayType(inType) => s"NSMutableArray <${kaitaiType2NativeType(inType, absolute)}> *"

      case KaitaiStreamType => s"$kstreamName *"
      case KaitaiStructType => s"$kstructName *"
      case CalcKaitaiStructType => s"$kstructName *"

      case st: SwitchType => "id "
      case AnyType => "id "
    }
  }

  def types2class(components: List[String]) =
    type2class(components.reverse.mkString("_"))

  def type2class(name: String) = name + "_t"
}
