package io.kaitai.struct.languages

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format._
import io.kaitai.struct.translators.{BaseTranslator, CppTranslator, TypeProvider}
import io.kaitai.struct.{LanguageOutputWriter, Utils}

class CppCompiler(verbose: Boolean, outSrc: LanguageOutputWriter, outHdr: LanguageOutputWriter)
  extends LanguageCompiler(verbose, outSrc)
    with EveryReadIsExpression {
  import CppCompiler._

  sealed trait AccessMode
  case object PrivateAccess extends AccessMode
  case object PublicAccess extends AccessMode

  var accessMode: AccessMode = PrivateAccess

  override def getTranslator(tp: TypeProvider): BaseTranslator = new CppTranslator(tp)

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
      s"$kstreamName* _io, " +
      s"${type2class(parentClassName)}* _parent = 0, " +
      s"${type2class(rootClassName)}* _root = 0);"
    )

    outSrc.puts
    outSrc.puts(s"${type2class(name)}::${type2class(List(name.last))}(" +
      s"$kstreamName *_io, " +
      s"${type2class(parentClassName)} *_parent, " +
      s"${type2class(rootClassName)} *_root) : $kstructName(_io) {"
    )
    outSrc.inc
    handleAssignmentSimple("_parent", "_parent")
    handleAssignmentSimple("_root", if (name == rootClassName) {
      "this"
    } else {
      "_root"
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

  override def attributeDeclaration(attrName: String, attrType: BaseType): Unit = {
    ensureMode(PrivateAccess)
    outHdr.puts(s"${kaitaiType2NativeType(attrType)} ${privateMemberName(attrName)};")
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

  override def attributeReader(attrName: String, attrType: BaseType): Unit = {
    ensureMode(PublicAccess)
    outHdr.puts(s"${kaitaiType2NativeType(attrType)} ${attrReaderName(attrName)}() const { return ${privateMemberName(attrName)}; }")
  }

  override def attrDestructor(attr: AttrLikeSpec, id: String): Unit = {
    var t = attr.dataTypeComposite

    if (attr.isArray) {
      outSrc.puts(s"${privateMemberName(id)}->clear();")
      outSrc.puts(s"delete ${privateMemberName(id)};")
      return
    }

    t match {
      case ut: UserType =>
        if (attr.cond.ifExpr.isDefined) {
          outSrc.puts(s"if (${flagForInstName(id)})")
          outSrc.inc
          outSrc.puts(s"delete ${privateMemberName(id)};")
          outSrc.dec
        } else {
          outSrc.puts(s"delete ${privateMemberName(id)};")
        }
      case _ =>
        // no cleanup needed
    }
  }

  override def attrFixedContentsParse(attrName: String, contents: Array[Byte]): Unit = {
    val strLiteral = contents.map { x => "\\x%02x".format(x) }.mkString
    outSrc.puts(s"${privateMemberName(attrName)} = $normalIO->ensure_fixed_contents(${contents.length}, " + "\"" + strLiteral + "\");")
  }

  override def attrProcess(proc: ProcessExpr, varSrc: String, varDest: String): Unit = {
    proc match {
      case ProcessXor(xorValue) =>
        outHdr.puts(s"this->$varDest = new byte[this.$varSrc.length];")
        outHdr.puts(s"for (int i = 0; i < this.$varSrc.length; i++) {")
        outHdr.inc
        outHdr.puts(s"this->$varDest[i] = (byte) (this->$varSrc[i] ^ (${expression(xorValue)}));")
        outHdr.dec
        outHdr.puts("}")
    }
  }

  override def normalIO: String = privateMemberName("_io")

  override def allocateIO(varName: String, rep: RepeatSpec): String = {
    val memberName = privateMemberName(varName)

    val ioName = s"_io_$varName"

    val args = rep match {
      case RepeatEos | RepeatExpr(_) => s"$memberName.get($memberName.size() - 1)"
      case NoRepeat => memberName
    }

    outSrc.puts(s"${privateMemberName(ioName)} = new $kstreamName($args);")
    ioName
  }

  override def useIO(ioEx: expr): String = {
    outSrc.puts(s"KaitaiStream *io = ${expression(ioEx)};")
    "io"
  }

  override def pushPos(io: String): Unit =
    outSrc.puts(s"uint64_t _pos = $io->pos();")

  override def seek(io: String, pos: Ast.expr): Unit =
    outSrc.puts(s"$io->seek(${expression(pos)});")

  override def popPos(io: String): Unit =
    outSrc.puts(s"$io->seek(_pos);")

  override def instanceClear(instName: String): Unit = {
    outSrc.puts(s"${flagForInstName(instName)} = false;")
  }

  override def instanceSetCalculated(instName: String): Unit = {
    outSrc.puts(s"${flagForInstName(instName)} = true;")
  }

  override def condIfHeader(expr: expr): Unit = {
    outSrc.puts(s"if (${expression(expr)}) {")
    outSrc.inc
  }

  override def condIfFooter(expr: expr): Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def condRepeatEosHeader(id: String, io: String, dataType: BaseType, needRaw: Boolean): Unit = {
    if (needRaw)
      outSrc.puts(s"this._raw_${privateMemberName(id)} = new ArrayList<byte[]>();")
    outSrc.puts(s"${privateMemberName(id)} = new std::vector<${kaitaiType2NativeType(dataType)}>();")
    outSrc.puts(s"while (!$io->is_eof()) {")
    outSrc.inc
  }

  override def handleAssignmentRepeatEos(id: String, expr: String): Unit = {
    outSrc.puts(s"${privateMemberName(id)}->push_back($expr);")
  }

  override def condRepeatEosFooter: Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def condRepeatExprHeader(id: String, io: String, dataType: BaseType, needRaw: Boolean, repeatExpr: expr): Unit = {
    if (needRaw)
      outSrc.puts(s"this._raw_${privateMemberName(id)} = new ArrayList<byte[]>((int) (${expression(repeatExpr)}));")
    outSrc.puts(s"${privateMemberName(id)} = new std::vector<${kaitaiType2NativeType(dataType)}>();")
    outSrc.puts(s"${privateMemberName(id)}->reserve(${expression(repeatExpr)});")
    outSrc.puts(s"for (int i = 0; i < ${expression(repeatExpr)}; i++) {")
    outSrc.inc
  }

  override def handleAssignmentRepeatExpr(id: String, expr: String): Unit = {
    outSrc.puts(s"${privateMemberName(id)}->push_back($expr);")
  }

  override def condRepeatExprFooter: Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def handleAssignmentSimple(id: String, expr: String): Unit = {
    outSrc.puts(s"${privateMemberName(id)} = $expr;")
  }

  override def parseExpr(dataType: BaseType, io: String): String = {
    dataType match {
      case t: IntType =>
        s"$io->read_${t.apiCall}()"
      // Aw, crap, can't use interpolated strings here: https://issues.scala-lang.org/browse/SI-6476
      case StrByteLimitType(bs, encoding) =>
        s"$io->read_str_byte_limit(${expression(bs)} /*, $encoding */)"
      case StrEosType(encoding) =>
        s"$io->read_str_eos(/* $encoding */)"
      case StrZType(encoding, terminator, include, consume, eosError) =>
        s"$io->read_strz(/* $encoding, */ $terminator, $include, $consume, $eosError)"
//      case EnumType(enumName, t) =>
//        s"${type2class(enumName)}.byId($io->read${Utils.capitalize(t.apiCall)}())"
      case BytesLimitType(size, _) =>
        s"$io->read_bytes(${expression(size)})"
      case BytesEosType(_) =>
        s"$io->read_bytes_full()"
      case t: UserType =>
        s"new ${type2class(t.name)}($io, this, ${privateMemberName("_root")})"
    }
  }

  override def instanceDeclaration(attrName: String, attrType: BaseType): Unit = {
    ensureMode(PrivateAccess)
    outHdr.puts(s"bool ${flagForInstName(attrName)};")
    outHdr.puts(s"${kaitaiType2NativeType(attrType)} ${privateMemberName(attrName)};")
  }

  override def instanceHeader(className: List[String], instName: String, dataType: BaseType): Unit = {
    ensureMode(PublicAccess)
    outHdr.puts(s"${kaitaiType2NativeType(dataType)} ${attrReaderName(instName)}();")

    outSrc.puts
    outSrc.puts(s"${kaitaiType2NativeType(dataType, true, className)} ${type2class(className)}::${attrReaderName(instName)}() {")
    outSrc.inc
  }

  override def instanceAttrName(instName: String): String = instName

  override def instanceFooter: Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def instanceCheckCacheAndReturn(instName: String): Unit = {
    outSrc.puts(s"if (${flagForInstName(instName)})")
    outSrc.inc
    instanceReturn(instName)
    outSrc.dec
  }

  override def instanceReturn(instName: String): Unit = {
    outSrc.puts(s"return ${privateMemberName(instName)};")
  }

  override def instanceCalculate(instName: String, value: Ast.expr): Unit = {
    outSrc.puts(s"${privateMemberName(instName)} = ${expression(value)};")
  }

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Map[Long, String]): Unit = {
    val enumClass = type2class(List(enumName))

    outHdr.puts
    outHdr.puts(s"public enum $enumClass {")
    outHdr.inc

    if (enumColl.size > 1) {
      enumColl.dropRight(1).foreach { case (id, label) =>
        outHdr.puts(s"${value2Const(label)}($id),")
      }
    }
    enumColl.last match {
      case (id, label) =>
        outHdr.puts(s"${value2Const(label)}($id);")
    }

    outHdr.puts
    outHdr.puts("private final long id;")
    outHdr.puts(s"$enumClass(long id) { this.id = id; }")
    outHdr.puts("public long id() { return id; }")
    outHdr.puts(s"private static final Map<Long, $enumClass> byId = new HashMap<Long, $enumClass>(${enumColl.size});")
    outHdr.puts("static {")
    outHdr.inc
    outHdr.puts(s"for ($enumClass e : $enumClass.values())")
    outHdr.inc
    outHdr.puts(s"byId.put(e.id(), e);")
    outHdr.dec
    outHdr.dec
    outHdr.puts("}")
    outHdr.puts(s"public static $enumClass byId(long id) { return byId.get(id); }")
    outHdr.dec
    outHdr.puts("}")
  }

  def value2Const(s: String) = s.toUpperCase

  def kaitaiType2NativeType(attrType: BaseType, absolute: Boolean = false, curClass: List[String] = List()): String = {
    attrType match {
      case Int1Type(false) => "uint8_t"
      case IntMultiType(false, Width2, _) => "uint16_t"
      case IntMultiType(false, Width4, _) => "uint32_t"
      case IntMultiType(false, Width8, _) => "uint64_t"

      case Int1Type(true) => "int8_t"
      case IntMultiType(true, Width2, _) => "int16_t"
      case IntMultiType(true, Width4, _) => "int32_t"
      case IntMultiType(true, Width8, _) => "int64_t"

      case CalcIntType => "int32_t"

      case _: StrType => "std::string"
      case _: BytesType => "std::string"

      case t: UserType =>
        val classList: List[String] = if (absolute) {
          curClass ::: t.name
        } else {
          t.name
        }
        s"${type2class(classList)}*"

      case EnumType(name, _) => type2class(List(name))
      case ArrayType(inType) => s"std::vector<${kaitaiType2NativeType(inType)}>*"

      case KaitaiStreamType => s"$kstreamName*"
    }
  }

  def defineName(className: String) = className.toUpperCase + "_H_"

  override def privateMemberName(ksName: String): String = {
    /*
    if (ksName == "_root" || ksName == "_parent" || ksName == "_io") {
      "_m" + ksName
    } else if (ksName.startsWith("_raw_")) {
      "_raw_" + Utils.lowerCamelCase(ksName.substring("_raw_".length))
    } else {
      Utils.lowerCamelCase(ksName) + "_"
    }*/
    "m_" + ksName
  }

  def attrReaderName(ksName: String) = {
    /*
    if (ksName == "_root" || ksName == "_parent" || ksName == "_io") {
      "m" + ksName
    } else if (ksName.startsWith("_raw_")) {
      "m_raw_" + Utils.lowerCamelCase(ksName.substring("_raw_".length))
    } else {
      Utils.lowerCamelCase(ksName)
    }*/
    ksName
  }

  def flagForInstName(ksName: String) = "f_" + ksName

  def rawMemberName(ksName: String) = "raw_" + ksName

  def kstructName = "kaitai::kstruct"

  def kstreamName = "kaitai::kstream"

  def type2class(components: List[String]) = {
    components.map {
      case "kaitai_struct" => "kaitai::kstruct"
      case s => s + "_t"
    }.mkString("::")
  }

  /**
    * We don't have a luxury of garbage collection in C++ and want to keep things clean and simple, thus
    * we need to store allocated IOs to clean them up in destructor.
    * @return true
    */
  override def needToStoreIOs: Boolean = true
}

object CppCompiler extends LanguageCompilerStatic {
  override def indent: String = "    "
  override def outFileName(topClassName: String): String = topClassName
}
