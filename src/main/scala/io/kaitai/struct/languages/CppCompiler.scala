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

    outHdr.puts(s"#ifndef ${defineName(topClassName)}")
    outHdr.puts(s"#define ${defineName(topClassName)}")
    outHdr.puts
    outHdr.puts(s"// $headerComment")
    outHdr.puts
    outHdr.puts("#include <kaitai/kaitaistruct.h>")
    outHdr.puts("#include <kaitai/kaitaistream.h>")
    outHdr.puts
    outHdr.puts("#include <stdint.h>")
    outHdr.puts
  }

  override def classHeader(name: String): Unit = {
    outHdr.puts(s"class ${type2class(name)} : public KaitaiStruct {")
    outHdr.inc
    outHdr.puts("public:")
    accessMode = PublicAccess
/*
    outHdr.puts(s"static ${type2class(name)} fromFile(std::string fileName);")

    outSrc.puts(s"${type2class(name)}::fromFile(std::string fileName) {")
    outSrc.inc
    outSrc.puts(s"return new ${type2class(name)}(new KaitaiStream(fileName));")
    outSrc.dec
    outSrc.puts("}")
    */
  }

  override def classFooter(name: String): Unit = {
    outHdr.dec
    outHdr.puts("};")
    outHdr.puts
    outHdr.puts(s"#endif  // ${defineName(name)}")
  }

  override def classConstructorHeader(name: String, parentClassName: String, rootClassName: String): Unit = {
    outHdr.puts
    outHdr.puts(s"${type2class(name)}(KaitaiStream* _io, ${type2class(parentClassName)}* _parent = 0, ${type2class(rootClassName)}* _root = 0);")

    outSrc.puts(s"${type2class(name)}::${type2class(name)}(KaitaiStream *_io, ${type2class(parentClassName)} *_parent, ${type2class(rootClassName)} *_root) : KaitaiStruct(_io) {")
    outSrc.inc
    outSrc.puts(s"${privateMemberName("_parent")} = _parent;")
    outSrc.puts(s"${privateMemberName("_root")} = _root ? _root : this;")
  }

  override def classConstructorFooter: Unit = {
    outSrc.dec
    outSrc.puts("}")
  }

  override def attributeDeclaration(attrName: String, attrType: BaseType): Unit = {
    if (accessMode != PrivateAccess) {
      outHdr.puts
      outHdr.puts(s"private:")
      accessMode = PrivateAccess
    }
    outHdr.puts(s"${kaitaiType2NativeType(attrType)} ${privateMemberName(attrName)};")
  }

  override def attributeReader(attrName: String, attrType: BaseType): Unit = {
    if (accessMode != PublicAccess) {
      outHdr.puts
      outHdr.puts("public:")
      accessMode = PublicAccess
    }
    outHdr.puts(s"${kaitaiType2NativeType(attrType)} ${attrReaderName(attrName)}() { return ${privateMemberName(attrName)}; }")
  }

  override def attrFixedContentsParse(attrName: String, contents: Array[Byte]): Unit = {
    outSrc.puts(s"privateMemberName(attrName) = _io->ensureFixedContents(${contents.length}, new byte[] { ${contents.mkString(", ")} });")
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

  override def normalIO: String = "_io"

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
        s"${type2class(enumName)}.byId($io.read${Utils.capitalize(t.apiCall)}())"
      case BytesLimitType(size, _) =>
        s"$io->read_bytes(${expression(size)})"
      case BytesEosType(_) =>
        s"$io->read_bytes_full()"
      case t: UserType =>
        s"new ${type2class(t.name)}($io, this, _root)"
    }
  }

  override def instanceDeclaration(attrName: String, attrType: BaseType): Unit = {
    outHdr.puts(s"private ${kaitaiType2NativeType(attrType)} ${privateMemberName(attrName)};")
  }

  override def instanceHeader(className: String, instName: String, dataType: BaseType): Unit = {
    outHdr.puts(s"public ${kaitaiType2NativeType(dataType)} ${privateMemberName(instName)}() throws IOException {")
    outHdr.inc
  }

  override def instanceAttrName(instName: String): String = instName

  override def instanceFooter: Unit = classConstructorFooter

  override def instanceCheckCacheAndReturn(instName: String): Unit = {
    outHdr.puts(s"if (${privateMemberName(instName)} != null)")
    outHdr.inc
    instanceReturn(instName)
    outHdr.dec
  }

  override def instanceReturn(instName: String): Unit = {
    outHdr.puts(s"return ${privateMemberName(instName)};")
  }

  override def instanceCalculate(instName: String, value: Ast.expr): Unit = {
    outHdr.puts(s"${privateMemberName(instName)} = ${expression(value)};")
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
    if (ksName == "_root" || ksName == "_parent" || ksName == "_io") {
      "_m" + ksName
    } else if (ksName.startsWith("_raw_")) {
      "_raw_" + Utils.lowerCamelCase(ksName.substring("_raw_".length))
    } else {
      Utils.lowerCamelCase(ksName) + "_"
    }
  }

  def attrReaderName(ksName: String) = {
    if (ksName == "_root" || ksName == "_parent" || ksName == "_io") {
      "m" + ksName
    } else if (ksName.startsWith("_raw_")) {
      "m_raw_" + Utils.lowerCamelCase(ksName.substring("_raw_".length))
    } else {
      Utils.lowerCamelCase(ksName)
    }
  }

  def rawMemberName(ksName: String) = "_raw_" + Utils.lowerCamelCase(ksName)
}

object CppCompiler extends LanguageCompilerStatic with UpperCamelCaseClasses {
  override def indent: String = "    "
  override def outFileName(topClassName: String): String = s"${type2class(topClassName)}"
}
