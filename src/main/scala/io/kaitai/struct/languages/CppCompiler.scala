package io.kaitai.struct.languages

import io.kaitai.struct.{LanguageOutputWriter, Utils, Main}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format._
import io.kaitai.struct.translators.{JavaTranslator, BaseTranslator, TypeProvider}

class CppCompiler(verbose: Boolean, outSrc: LanguageOutputWriter, outHdr: LanguageOutputWriter) extends LanguageCompiler(verbose, outSrc) with EveryReadIsExpression {
  import CppCompiler._

  sealed trait AccessMode
  case object PrivateAccess extends AccessMode
  case object PublicAccess extends AccessMode

  var accessMode: AccessMode = PrivateAccess

  override def getTranslator(tp: TypeProvider): BaseTranslator = new JavaTranslator(tp)

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
  }

  override def fileFooter(topClassName: String): Unit = {
    outHdr.puts
    outHdr.puts(s"#endif  // ${defineName(topClassName)}")
  }

  override def classHeader(name: String): Unit = {
    outHdr.puts
    outHdr.puts(s"class ${type2class(name)} : public $kstructName {")
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

  override def classFooter(name: String): Unit = {
    outHdr.dec
    outHdr.puts("};")
  }

  override def classConstructorHeader(name: String, parentClassName: String, rootClassName: String): Unit = {
    outHdr.puts
    outHdr.puts(s"${type2class(name)}($kstreamName* _io, ${type2class(parentClassName)}* _parent = 0, ${type2class(rootClassName)}* _root = 0);")

    outSrc.puts
    outSrc.puts(s"${type2class(name)}::${type2class(name)}($kstreamName *_io, ${type2class(parentClassName)} *_parent, ${type2class(rootClassName)} *_root) : $kstructName(_io) {")
    outSrc.inc
    handleAssignmentSimple("_parent", "_parent");
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
    outHdr.puts(s"${kaitaiType2NativeType(attrType)} ${attrReaderName(attrName)}() { return ${privateMemberName(attrName)}; }")
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
    val javaName = privateMemberName(varName)

    val ioName = s"_io_$javaName"

    val args = rep match {
      case RepeatEos | RepeatExpr(_) => s"$javaName.get($javaName.size() - 1)"
      case NoRepeat => javaName
    }

    outSrc.puts(s"KaitaiStream *$ioName = new KaitaiStream($args);")
    ioName
  }

  override def useIO(ioEx: expr): String = {
    outSrc.puts(s"KaitaiStream *io = ${expression(ioEx)};")
    "io"
  }

  override def seek(io: String, pos: Ast.expr): Unit = {
    outSrc.puts(s"$io->seek(${expression(pos)});")
  }

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
    outSrc.puts(s"this.${privateMemberName(id)} = new ${kaitaiType2NativeType(ArrayType(dataType))}();")
    outSrc.puts(s"while (!$io->isEof()) {")
    outSrc.inc
  }

  override def handleAssignmentRepeatEos(id: String, expr: String): Unit = {
    outSrc.puts(s"${privateMemberName(id)}->push($expr);")
  }

  override def condRepeatEosFooter: Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def condRepeatExprHeader(id: String, io: String, dataType: BaseType, needRaw: Boolean, repeatExpr: expr): Unit = {
    if (needRaw)
      outSrc.puts(s"this._raw_${privateMemberName(id)} = new ArrayList<byte[]>((int) (${expression(repeatExpr)}));")
    outSrc.puts(s"${privateMemberName(id)} = new ${kaitaiType2NativeType(ArrayType(dataType))}((int) (${expression(repeatExpr)}));")
    outSrc.puts(s"for (int i = 0; i < ${expression(repeatExpr)}; i++) {")
    outSrc.inc
  }

  override def handleAssignmentRepeatExpr(id: String, expr: String): Unit = {
    outSrc.puts(s"${privateMemberName(id)}.add($expr);")
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
        s"$io->read_str_byte_limit(${expression(bs)}, " + '"' + encoding + "\")"
      case StrEosType(encoding) =>
        io + "->read_str_eos(\"" + encoding + "\")"
      case StrZType(encoding, terminator, include, consume, eosError) =>
        io + "->read_strz(\"" + encoding + '"' + s", $terminator, $include, $consume, $eosError)"
      case EnumType(enumName, t) =>
        s"${type2class(enumName)}.byId($io->read${Utils.capitalize(t.apiCall)}())"
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

  override def instanceHeader(className: String, instName: String, dataType: BaseType): Unit = {
    ensureMode(PublicAccess)
    outHdr.puts(s"${kaitaiType2NativeType(dataType)} ${attrReaderName(instName)}();")

    outSrc.puts
    outSrc.puts(s"${kaitaiType2NativeType(dataType)} ${type2class(className)}::${attrReaderName(instName)}() {")
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

  override def enumDeclaration(curClass: String, enumName: String, enumColl: Map[Long, String]): Unit = {
    val enumClass = type2class(enumName)

    outHdr.puts
    outHdr.puts(s"public enum $enumClass {")
    outHdr.inc

    val it = enumColl.toIterable
    if (enumColl.size > 1) {
      it.dropRight(1).foreach { case (id, label) =>
        outHdr.puts(s"${value2Const(label)}($id),")
      }
    }
    it.last match {
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

  def kaitaiType2NativeType(attrType: BaseType): String = {
    attrType match {
      case Int1Type(false) => "uint8_t"
      case IntMultiType(false, Width2, _) => "uint16_t"
      case IntMultiType(false, Width4, _) => "uint32_t"
      case IntMultiType(false, Width8, _) => "uint64_t"

      case Int1Type(true) => "int8_t"
      case IntMultiType(true, Width2, _) => "int16_t"
      case IntMultiType(true, Width4, _) => "int32_t"
      case IntMultiType(true, Width8, _) => "int64_t"

      case _: StrType => "std::string"
      case _: BytesType => "std::string"

      case t: UserType => s"${type2class(t.name)}*"
      case EnumType(name, _) => type2class(name)

      case ArrayType(inType) => s"std::vector<${kaitaiType2NativeType(inType)}>"
    }
  }

  def defineName(className: String) = className.toUpperCase + "_H_"

  def privateMemberName(ksName: String) = {
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

  def rawMemberName(ksName: String) = "_raw_" + Utils.lowerCamelCase(ksName)

  def kstructName = "kaitai::kstruct"

  def kstreamName = "kaitai::kstream"
}

object CppCompiler extends LanguageCompilerStatic {
  override def indent: String = "    "
  override def outFileName(topClassName: String): String = s"${type2class(topClassName)}"
  def type2class(s: String) = {
    s match {
      case "kaitai_struct" => "kaitai::kstruct"
      case _ => s
    }
  }
}
