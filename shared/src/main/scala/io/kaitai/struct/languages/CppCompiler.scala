package io.kaitai.struct.languages

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.{BaseTranslator, CppTranslator, TypeProvider}
import io.kaitai.struct.{ClassCompiler, LanguageOutputWriter, RuntimeConfig, TypeProcessor}

class CppCompiler(config: RuntimeConfig, outSrc: LanguageOutputWriter, outHdr: LanguageOutputWriter)
  extends LanguageCompiler(config, outSrc)
    with ObjectOrientedLanguage
    with AllocateAndStoreIO
    with FixedContentsUsingArrayByteLiteral
    with UniversalDoc
    with EveryReadIsExpression {
  import CppCompiler._

  override def getStatic = CppCompiler

  sealed trait AccessMode
  case object PrivateAccess extends AccessMode
  case object PublicAccess extends AccessMode

  var accessMode: AccessMode = PublicAccess

  override def fileHeader(topClassName: String): Unit = {
    outSrc.puts(s"// $headerComment")
    outSrc.puts
    outSrc.puts("#include \"" + outFileName(topClassName) + ".h\"")
    outSrc.puts
    outSrc.puts("#include <iostream>")
    outSrc.puts("#include <fstream>")

    outHdr.puts(s"#ifndef ${defineName(topClassName)}")
    outHdr.puts(s"#define ${defineName(topClassName)}")
    outHdr.puts
    outHdr.puts(s"// $headerComment")
    outHdr.puts
    outHdr.puts("#include <kaitai/kaitaistruct.h>")
    outHdr.puts("#include <kaitai/kaitaistream.h>")
    outHdr.puts
    outHdr.puts("#include <stdint.h>")
    outHdr.puts("#include <vector>") // TODO: add only if required
    outHdr.puts("#include <sstream>") // TODO: add only if required
  }

  override def fileFooter(topClassName: String): Unit = {
    outHdr.puts
    outHdr.puts(s"#endif  // ${defineName(topClassName)}")
  }

  override def opaqueClassDeclaration(classSpec: ClassSpec): Unit = {
    classForwardDeclaration(classSpec.name)
    outSrc.puts("#include \"" + outFileName(classSpec.name.head) + ".h\"")
  }

  override def classHeader(name: List[String]): Unit = {
    outHdr.puts
    outHdr.puts(s"class ${type2class(List(name.last))} : public $kstructName {")
    outHdr.inc
    accessMode = PrivateAccess
    ensureMode(PublicAccess)

    /*
    outHdr.puts(s"static ${type2class(name)} from_file(std::string ${attrReaderName("file_name")});")

    outSrc.puts
    outSrc.puts(s"${type2class(name)} ${type2class(name)}::from_file(std::string ${attrReaderName("file_name")}) {")
    outSrc.inc
    outSrc.puts("std::ifstream* ifs = new std::ifstream(file_name, std::ifstream::binary);")
    outSrc.puts("kaitai::kstream *ks = new kaitai::kstream(ifs);")
    outSrc.puts(s"return new ${type2class(name)}(ks);")
    outSrc.dec
    outSrc.puts("}")
    */
  }

  override def classFooter(name: List[String]): Unit = {
    outHdr.dec
    outHdr.puts("};")
  }

  override def classForwardDeclaration(name: List[String]): Unit = {
    outHdr.puts(s"class ${type2class(name)};")
  }

  override def classConstructorHeader(name: List[String], parentClassName: List[String], rootClassName: List[String]): Unit = {
    outHdr.puts
    outHdr.puts(s"${type2class(List(name.last))}(" +
      s"$kstreamName* p_io, " +
      s"${type2class(parentClassName)}* p_parent = 0, " +
      s"${type2class(rootClassName)}* p_root = 0);"
    )

    outSrc.puts
    outSrc.puts(s"${type2class(name)}::${type2class(List(name.last))}(" +
      s"$kstreamName *p_io, " +
      s"${type2class(parentClassName)} *p_parent, " +
      s"${type2class(rootClassName)} *p_root) : $kstructName(p_io) {"
    )
    outSrc.inc
    handleAssignmentSimple(ParentIdentifier, "p_parent")
    handleAssignmentSimple(RootIdentifier, if (name == rootClassName) {
      "this"
    } else {
      "p_root"
    })
  }

  override def classConstructorFooter: Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def classDestructorHeader(name: List[String], parentTypeName: List[String], topClassName: List[String]): Unit = {
    outHdr.puts(s"~${type2class(List(name.last))}();")

    outSrc.puts
    outSrc.puts(s"${type2class(name)}::~${type2class(List(name.last))}() {")
    outSrc.inc
  }

  override def classDestructorFooter = classConstructorFooter

  override def attributeDeclaration(attrName: Identifier, attrType: BaseType, condSpec: ConditionalSpec): Unit = {
    ensureMode(PrivateAccess)
    outHdr.puts(s"${kaitaiType2NativeType(attrType)} ${privateMemberName(attrName)};")
    declareNullFlag(attrName, condSpec)
  }

  def ensureMode(newMode: AccessMode): Unit = {
    if (accessMode != newMode) {
      outHdr.dec
      outHdr.puts
      outHdr.puts(newMode match {
        case PrivateAccess => "private:"
        case PublicAccess => "public:"
      })
      outHdr.inc
      accessMode = newMode
    }
  }

  override def attributeReader(attrName: Identifier, attrType: BaseType, condSpec: ConditionalSpec): Unit = {
    ensureMode(PublicAccess)
    outHdr.puts(s"${kaitaiType2NativeType(attrType)} ${publicMemberName(attrName)}() const { return ${privateMemberName(attrName)}; }")
  }

  override def universalDoc(doc: String): Unit = {
    // All docstrings would be for public stuff, so it's safe to start it here
    ensureMode(PublicAccess)

    outHdr.puts
    outHdr.puts( "/**")
    outHdr.puts(s" * $doc")
    outHdr.puts( " */")
  }

  override def attrDestructor(attr: AttrLikeSpec, id: Identifier): Unit = {
    val t = attr.dataTypeComposite

    val checkFlags = attr match {
      case is: InstanceSpec =>
        val dataType = TypeProcessor.getInstanceDataType(is)
        dataType match {
          case ut: UserType =>
            val checkLazy = calculatedFlagForName(id.asInstanceOf[InstanceIdentifier])
            val checkNull = if (is.cond.ifExpr.isDefined) {
              s" && !${nullFlagForName(id)}"
            } else {
              ""
            }
            outSrc.puts(s"if ($checkLazy$checkNull) {")
            outSrc.inc
            true
          case _ =>
            false
        }
      case as: AttrSpec =>
        as.dataType match {
          case ut: UserType =>
            if (as.cond.ifExpr.isDefined) {
              outSrc.puts(s"if (!${nullFlagForName(id)}) {")
              outSrc.inc
              true
            } else {
              false
            }
          case _ =>
            false
        }
    }

    t match {
      case ArrayType(_: UserTypeKnownSize) =>
        outSrc.puts(s"delete ${privateMemberName(RawIdentifier(id))};")
      case _ =>
        // no cleanup needed
    }

    t match {
      case ArrayType(el: UserType) =>
        val arrVar = privateMemberName(id)
        outSrc.puts(s"for (std::vector<${kaitaiType2NativeType(el)}>::iterator it = $arrVar->begin(); it != $arrVar->end(); ++it) {")
        outSrc.inc
        outSrc.puts("delete *it;")
        outSrc.dec
        outSrc.puts("}")
      case _ =>
        // no cleanup needed
    }

    t match {
      case _: UserTypeKnownSize =>
        outSrc.puts(s"delete ${privateMemberName(IoStorageIdentifier(RawIdentifier(id)))};")
      case _ =>
        // no cleanup needed
    }

    t match {
      case _: UserType | _: ArrayType =>
        outSrc.puts(s"delete ${privateMemberName(id)};")
      case _ =>
        // no cleanup needed
    }

    if (checkFlags) {
      outSrc.dec
      outSrc.puts("}")
    }
  }

  override def attrFixedContentsParse(attrName: Identifier, contents: String): Unit =
    outSrc.puts(s"${privateMemberName(attrName)} = $normalIO->ensure_fixed_contents($contents);")

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit = {
    val srcName = privateMemberName(varSrc)
    val destName = privateMemberName(varDest)

    proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "process_xor_one"
          case _: BytesType => "process_xor_many"
        }
        outSrc.puts(s"$destName = $kstreamName::$procName($srcName, ${expression(xorValue)});")
      case ProcessZlib =>
        outSrc.puts(s"$destName = $kstreamName::process_zlib($srcName);")
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        outSrc.puts(s"$destName = $kstreamName::process_rotate_left($srcName, $expr);")
    }
  }

  override def allocateIO(varName: Identifier, rep: RepeatSpec): IoStorageIdentifier = {
    val memberName = privateMemberName(varName)

    val ioName = IoStorageIdentifier(varName)

    val args = rep match {
      case RepeatEos | RepeatExpr(_) | RepeatUntil(_) => s"$memberName->at($memberName->size() - 1)"
      case NoRepeat => memberName
    }

    outSrc.puts(s"${privateMemberName(ioName)} = new $kstreamName($args);")
    ioName
  }

  override def useIO(ioEx: Ast.expr): String = {
    outSrc.puts(s"$kstreamName *io = ${expression(ioEx)};")
    "io"
  }

  override def pushPos(io: String): Unit =
    outSrc.puts(s"std::streampos _pos = $io->pos();")

  override def seek(io: String, pos: Ast.expr): Unit =
    outSrc.puts(s"$io->seek(${expression(pos)});")

  override def popPos(io: String): Unit =
    outSrc.puts(s"$io->seek(_pos);")

  override def instanceClear(instName: InstanceIdentifier): Unit =
    outSrc.puts(s"${calculatedFlagForName(instName)} = false;")

  override def instanceSetCalculated(instName: InstanceIdentifier): Unit =
    outSrc.puts(s"${calculatedFlagForName(instName)} = true;")

  override def condIfSetNull(instName: Identifier): Unit =
    outSrc.puts(s"${nullFlagForName(instName)} = true;")

  override def condIfSetNonNull(instName: Identifier): Unit =
    outSrc.puts(s"${nullFlagForName(instName)} = false;")

  override def condIfHeader(expr: Ast.expr): Unit = {
    outSrc.puts(s"if (${expression(expr)}) {")
    outSrc.inc
  }

  override def condIfFooter(expr: Ast.expr): Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean): Unit = {
    if (needRaw)
      outSrc.puts(s"${privateMemberName(RawIdentifier(id))} = new std::vector<std::string>();")
    outSrc.puts(s"${privateMemberName(id)} = new std::vector<${kaitaiType2NativeType(dataType)}>();")
    outSrc.puts(s"while (!$io->is_eof()) {")
    outSrc.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    outSrc.puts(s"${privateMemberName(id)}->push_back($expr);")
  }

  override def condRepeatEosFooter: Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean, repeatExpr: Ast.expr): Unit = {
    val lenVar = s"l_${idToStr(id)}"
    outSrc.puts(s"int $lenVar = ${expression(repeatExpr)};")
    if (needRaw) {
      outSrc.puts(s"${privateMemberName(RawIdentifier(id))} = new std::vector<std::string>();")
      outSrc.puts(s"${privateMemberName(RawIdentifier(id))}->reserve($lenVar);")
    }
    outSrc.puts(s"${privateMemberName(id)} = new std::vector<${kaitaiType2NativeType(dataType)}>();")
    outSrc.puts(s"${privateMemberName(id)}->reserve($lenVar);")
    outSrc.puts(s"for (int i = 0; i < $lenVar; i++) {")
    outSrc.inc
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit = {
    outSrc.puts(s"${privateMemberName(id)}->push_back($expr);")
  }

  override def condRepeatExprFooter: Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean, untilExpr: expr): Unit = {
    if (needRaw)
      outSrc.puts(s"${privateMemberName(RawIdentifier(id))} = new ArrayList<byte[]>();")
    outSrc.puts(s"${privateMemberName(id)} = new std::vector<${kaitaiType2NativeType(dataType)}>();")
    outSrc.puts("{")
    outSrc.inc
    outSrc.puts(s"${kaitaiType2NativeType(dataType)} ${translator.doName("_")};")
    outSrc.puts("do {")
    outSrc.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String): Unit = {
    outSrc.puts(s"${translator.doName("_")} = $expr;")
    outSrc.puts(s"${privateMemberName(id)}->push_back(${translator.doName("_")});")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean, untilExpr: expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    outSrc.dec
    outSrc.puts(s"} while (!(${expression(untilExpr)}));")
    outSrc.dec
    outSrc.puts("}")
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    outSrc.puts(s"${privateMemberName(id)} = $expr;")
  }

  override def parseExpr(dataType: BaseType, io: String): String = {
    dataType match {
      case t: ReadableType =>
        s"$io->read_${t.apiCall}()"
      // Aw, crap, can't use interpolated strings here: https://issues.scala-lang.org/browse/SI-6476
      case StrByteLimitType(bs, encoding) =>
        s"$io->read_str_byte_limit(${expression(bs)}, ${encodingToStr(encoding)})"
      case StrEosType(encoding) =>
        s"$io->read_str_eos(${encodingToStr(encoding)})"
      case StrZType(encoding, terminator, include, consume, eosError) =>
        s"$io->read_strz(${encodingToStr(encoding)}, $terminator, $include, $consume, $eosError)"
      case BytesLimitType(size, _) =>
        s"$io->read_bytes(${expression(size)})"
      case BytesEosType(_) =>
        s"$io->read_bytes_full()"
      case BitsType(width: Int) =>
        s"$io->read_bits_int($width)"
      case t: UserType =>
        val addArgs = if (!t.isOpaque) s", this, ${privateMemberName(RootIdentifier)}" else ""
        s"new ${type2class(t.name)}($io$addArgs)"
    }
  }

  def encodingToStr(encoding: String): String = "\"" + encoding + "\""

  /**
    * Designates switch mode. If false, we're doing real switch-case for this
    * attribute. If true, we're doing if-based emulation.
    */
  var switchIfs = false

  override def switchStart(id: Identifier, on: Ast.expr): Unit = {
    val onType = translator.detectType(on)

    // Determine switching mode for this construct based on type
    switchIfs = onType match {
      case _: IntType | _: EnumType => false
      case _ => true
    }

    if (switchIfs) {
      outSrc.puts("{")
      outSrc.inc
      outSrc.puts(s"${kaitaiType2NativeType(onType)} on = ${expression(on)};")
    } else {
      outSrc.puts(s"switch (${expression(on)}) {")
    }
  }

  override def switchCaseFirstStart(condition: Ast.expr): Unit = {
    if (switchIfs) {
      outSrc.puts(s"if (on == ${expression(condition)}) {")
      outSrc.inc
    } else {
      outSrc.puts(s"case ${expression(condition)}:")
      outSrc.inc
    }
  }

  override def switchCaseStart(condition: Ast.expr): Unit = {
    if (switchIfs) {
      outSrc.puts(s"else if (on == ${expression(condition)}) {")
      outSrc.inc
    } else {
      outSrc.puts(s"case ${expression(condition)}:")
      outSrc.inc
    }
  }

  override def switchCaseEnd(): Unit = {
    if (switchIfs) {
      outSrc.dec
      outSrc.puts("}")
    } else {
      outSrc.puts("break;")
      outSrc.dec
    }
  }

  override def switchElseStart(): Unit = {
    if (switchIfs) {
      outSrc.puts("else {")
      outSrc.inc
    } else {
      outSrc.puts("default:")
      outSrc.inc
    }
  }

  override def switchEnd(): Unit =
    if (switchIfs) {
      outSrc.dec
      outSrc.puts("}")
    } else {
      outSrc.puts("}")
    }

  override def switchBytesOnlyAsRaw = true

  override def instanceDeclaration(attrName: InstanceIdentifier, attrType: BaseType, condSpec: ConditionalSpec): Unit = {
    ensureMode(PrivateAccess)
    outHdr.puts(s"bool ${calculatedFlagForName(attrName)};")
    outHdr.puts(s"${kaitaiType2NativeType(attrType)} ${privateMemberName(attrName)};")
    declareNullFlag(attrName, condSpec)
  }

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: BaseType): Unit = {
    ensureMode(PublicAccess)
    outHdr.puts(s"${kaitaiType2NativeType(dataType)} ${publicMemberName(instName)}();")

    outSrc.puts
    outSrc.puts(s"${kaitaiType2NativeType(dataType, true)} ${type2class(className)}::${publicMemberName(instName)}() {")
    outSrc.inc
  }

  override def instanceFooter: Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier): Unit = {
    outSrc.puts(s"if (${calculatedFlagForName(instName)})")
    outSrc.inc
    instanceReturn(instName)
    outSrc.dec
  }

  override def instanceReturn(instName: InstanceIdentifier): Unit = {
    outSrc.puts(s"return ${privateMemberName(instName)};")
  }

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, String)]): Unit = {
    val enumClass = type2class(List(enumName))

    outHdr.puts
    outHdr.puts(s"enum $enumClass {")
    outHdr.inc

    if (enumColl.size > 1) {
      enumColl.dropRight(1).foreach { case (id, label) =>
        outHdr.puts(s"${value2Const(enumName, label)} = $id,")
      }
    }
    enumColl.last match {
      case (id, label) =>
        outHdr.puts(s"${value2Const(enumName, label)} = $id")
    }

    outHdr.dec
    outHdr.puts("};")
  }

  def value2Const(enumName: String, label: String) = (enumName + "_" + label).toUpperCase

  def kaitaiType2NativeType(attrType: BaseType, absolute: Boolean = false): String = {
    attrType match {
      case Int1Type(false) => "uint8_t"
      case IntMultiType(false, Width2, _) => "uint16_t"
      case IntMultiType(false, Width4, _) => "uint32_t"
      case IntMultiType(false, Width8, _) => "uint64_t"

      case Int1Type(true) => "int8_t"
      case IntMultiType(true, Width2, _) => "int16_t"
      case IntMultiType(true, Width4, _) => "int32_t"
      case IntMultiType(true, Width8, _) => "int64_t"

      case FloatMultiType(Width4, _) => "float"
      case FloatMultiType(Width8, _) => "double"

      case BitsType(1) => "bool"
      case BitsType(_) => "uint64_t"

      case BooleanType => "bool"
      case CalcIntType => "int32_t"
      case CalcFloatType => "double"

      case _: StrType => "std::string"
      case _: BytesType => "std::string"

      case t: UserType =>
        val typeStr = type2class(if (absolute) {
          t.classSpec.get.name
        } else {
          t.name
        })
        s"$typeStr*"

      case t: EnumType =>
        type2class(if (absolute) {
          t.enumSpec.get.name
        } else {
          t.name
        })

      case ArrayType(inType) => s"std::vector<${kaitaiType2NativeType(inType, absolute)}>*"

      case KaitaiStreamType => s"$kstreamName*"
      case KaitaiStructType => s"$kstructName*"

      case SwitchType(on, cases) =>
        kaitaiType2NativeType(BaseTranslator.combineTypes(
          // C++ does not have a concept of AnyType, and common use case "lots of incompatible UserTypes
          // for cases + 1 BytesType for else" combined would result in exactly AnyType - so we try extra
          // hard to avoid that here with this pre-filtering. In C++, "else" case with raw byte array would
          // be available through _raw_* attribute anyway.
          cases.filterNot { case (caseExpr, caseValue) => caseExpr == SwitchType.ELSE_CONST }.values
        ), absolute)
    }
  }

  def defineName(className: String) = className.toUpperCase + "_H_"

  def calculatedFlagForName(ksName: InstanceIdentifier) =
    s"f_${ksName.name}"

  def nullFlagForName(ksName: Identifier) = {
    ksName match {
      case NamedIdentifier(name) => s"n_$name"
      case InstanceIdentifier(name) => s"n_$name"
    }
  }

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

  override def privateMemberName(id: Identifier): String = s"m_${idToStr(id)}"

  override def publicMemberName(id: Identifier): String = idToStr(id)

  def declareNullFlag(attrName: Identifier, condSpec: ConditionalSpec) = {
    if (condSpec.ifExpr.nonEmpty) {
      outHdr.puts(s"bool ${nullFlagForName(attrName)};")
      ensureMode(PublicAccess)
      outHdr.puts(s"bool _is_null_${idToStr(attrName)}() { ${publicMemberName(attrName)}(); return ${nullFlagForName(attrName)}; };")
      ensureMode(PrivateAccess)
    }
  }
}

object CppCompiler extends LanguageCompilerStatic with StreamStructNames {
  override def getTranslator(tp: TypeProvider): BaseTranslator = new CppTranslator(tp)
  override def indent: String = "    "
  override def outFileName(topClassName: String): String = topClassName

  override def kstructName = "kaitai::kstruct"
  override def kstreamName = "kaitai::kstream"

  def type2class(components: List[String]) = {
    components.map {
      case "kaitai_struct" => "kaitai::kstruct"
      case s => s + "_t"
    }.mkString("::")
  }
}
