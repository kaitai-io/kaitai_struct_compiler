package io.kaitai.struct.languages

import io.kaitai.struct.{LanguageOutputWriter, RuntimeConfig, Utils}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType.{BytesType, CalcIntType, StrType, _}
import io.kaitai.struct.format.{NoRepeat, RepeatEos, RepeatExpr, RepeatSpec, _}
import io.kaitai.struct.translators.{BaseTranslator, CSharpTranslator, TypeProvider}

class CSharpCompiler(verbose: Boolean, out: LanguageOutputWriter, namespace: String = "Kaitai")
  extends LanguageCompiler(verbose, out)
    with EveryReadIsExpression
    with NoNeedForFullClassPath {
  import CSharpCompiler._

  override def getTranslator(tp: TypeProvider): BaseTranslator = new CSharpTranslator(tp)

  override def fileHeader(topClassName: String): Unit = {
    out.puts(s"// $headerComment")

    var ns = "Kaitai";
    if (!namespace.isEmpty) ns = namespace;

    out.puts
    out.puts("using System;")
    out.puts("using System.Collections.Generic;")
    if (ns != "Kaitai") out.puts("using Kaitai;")
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

    out.puts(s"public static ${type2class(name)} FromFile(string fileName)")
    out.puts(s"{")
    out.inc
    out.puts(s"return new ${type2class(name)}(new $kstreamName(fileName));")
    out.dec
    out.puts("}")
  }

  override def classFooter(name: String): Unit = fileFooter(name)

  override def classConstructorHeader(name: String, parentClassName: String, rootClassName: String): Unit = {
    out.puts
    out.puts(s"public ${type2class(name)}($kstreamName io, ${type2class(parentClassName)} parent = null, ${type2class(rootClassName)} root = null) : base(io)")
    out.puts(s"{")
    out.inc
    out.puts(s"${privateMemberName("_parent")} = parent;")

    if (name == rootClassName)
      out.puts(s"${privateMemberName("_root")} = root ?? this;")
    else
      out.puts(s"${privateMemberName("_root")} = root;")

    out.puts("_parse();")
    out.dec
    out.puts("}")
    out.puts

    out.puts("private void _parse()")
    out.puts("{")
    out.inc
  }

  override def classConstructorFooter: Unit = fileFooter(null)

  override def attributeDeclaration(attrName: String, attrType: BaseType, condSpec: ConditionalSpec): Unit = {
    out.puts(s"private ${kaitaiType2NativeType(attrType)} ${privateMemberName(attrName)};")
  }

  override def attributeReader(attrName: String, attrType: BaseType): Unit = {
    out.puts(s"public ${kaitaiType2NativeType(attrType)} ${publicMemberName(attrName)}() { return ${privateMemberName(attrName)}; }")
  }

  override def attrFixedContentsParse(attrName: String, contents: Array[Byte]): Unit = {
    out.puts(s"${privateMemberName(attrName)} = ${normalIO}.ensureFixedContents(${contents.length}, new byte[] { ${contents.mkString(", ")} });")
  }

  override def attrProcess(proc: ProcessExpr, varSrc: String, varDest: String): Unit = {
    proc match {
      case ProcessXor(xorValue) =>
        out.puts(s"${privateMemberName(varDest)} = ${normalIO}.processXorInt(${privateMemberName(varSrc)}, ${expression(xorValue)});")
      case ProcessZlib =>
        out.puts(s"${privateMemberName(varDest)} = ${normalIO}.processZlib(${privateMemberName(varSrc)});")
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        out.puts(s"${privateMemberName(varDest)} = ${normalIO}.processRotateLeft(${privateMemberName(varSrc)}, $expr, 1);")
    }
  }

  override def normalIO: String = "_io"

  override def allocateIO(varName: String, rep: RepeatSpec): String = {
    val privateVarName = privateMemberName(varName)

    val ioName = privateMemberName(s"_io_$privateVarName")

    val args = rep match {
      case RepeatEos | RepeatExpr(_) => s"$privateVarName[$privateVarName.Count - 1]"
      case NoRepeat => privateVarName
    }

    out.puts(s"var $ioName = new $kstreamName($args);")
    ioName
  }

  override def useIO(ioEx: expr): String = {
    out.puts(s"$kstreamName io = ${expression(ioEx)};")
    "io"
  }

  override def pushPos(io: String): Unit =
  out.puts(s"long _pos = $io.pos();")

  override def seek(io: String, pos: Ast.expr): Unit =
  out.puts(s"$io.seek(${expression(pos)});")

  override def popPos(io: String): Unit =
  out.puts(s"$io.seek(_pos);")

  override def condIfHeader(expr: expr): Unit = {
    out.puts(s"if (${expression(expr)}) {")
    out.inc
  }

  override def condIfFooter(expr: expr): Unit = {
    out.dec
    out.puts("}")
  }

  override def condRepeatEosHeader(id: String, io: String, dataType: BaseType, needRaw: Boolean): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(s"_raw_$id")} = new ArrayList<byte[]>();")
    out.puts(s"${privateMemberName(id)} = new ${kaitaiType2NativeType(ArrayType(dataType))}();")
    out.puts(s"while (!$io.isEof()) {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: String, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}.add($expr);")
  }

  override def condRepeatEosFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def condRepeatExprHeader(id: String, io: String, dataType: BaseType, needRaw: Boolean, repeatExpr: expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(s"_raw_$id")} = new ArrayList<byte[]>((int) (${expression(repeatExpr)}));")
    out.puts(s"${privateMemberName(id)} = new ${kaitaiType2NativeType(ArrayType(dataType))}((int) (${expression(repeatExpr)}));")
    out.puts(s"for (int i = 0; i < ${expression(repeatExpr)}; i++) {")
    out.inc
  }

  override def handleAssignmentRepeatExpr(id: String, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}.add($expr);")
  }

  override def condRepeatExprFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def handleAssignmentSimple(id: String, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)} = $expr;")
  }

  override def parseExpr(dataType: BaseType, io: String): String = {
    dataType match {
      case t: IntType =>
        s"$io.read${Utils.capitalize(t.apiCall)}()"
      // Aw, crap, can't use interpolated strings here: https://issues.scala-lang.org/browse/SI-6476
      case StrByteLimitType(bs, encoding) =>
        s"$io.readStrByteLimit(${expression(bs)}, " + '"' + encoding + "\")"
      case StrEosType(encoding) =>
        io + ".readStrEos(\"" + encoding + "\")"
      case StrZType(encoding, terminator, include, consume, eosError) =>
        io + ".readStrz(\"" + encoding + '"' + s", $terminator, $include, $consume, $eosError)"
      case EnumType(enumName, t) =>
        s"${type2class(enumName)}.byId($io.read${Utils.capitalize(t.apiCall)}())"
      case BytesLimitType(size, _) =>
        s"$io.readBytes(${expression(size)})"
      case BytesEosType(_) =>
        s"$io.readBytesFull()"
      case t: UserType =>
        s"new ${types2class(t.name)}($io, this, ${privateMemberName("_root")})"
    }
  }

  override def instanceDeclaration(attrName: String, attrType: BaseType, condSpec: ConditionalSpec): Unit = {
    out.puts(s"private ${kaitaiType2NativeType(attrType)} ${privateMemberName(attrName)};")
  }

  override def instanceHeader(className: String, instName: String, dataType: BaseType): Unit = {
    out.puts(s"public ${kaitaiType2NativeType(dataType)} ${publicMemberName(instName)}() throws IOException {")
    out.inc
  }

  override def instanceAttrName(instName: String): String = instName

  override def instanceFooter: Unit = classConstructorFooter

  override def instanceCheckCacheAndReturn(instName: String): Unit = {
    out.puts(s"if (${privateMemberName(instName)} != null)")
    out.inc
    instanceReturn(instName)
    out.dec
  }

  override def instanceReturn(instName: String): Unit = {
    out.puts(s"return ${privateMemberName(instName)};")
  }

  override def instanceCalculate(instName: String, value: Ast.expr): Unit = {
    out.puts(s"${lowerCamelCase(instName)} = ${expression(value)};")
  }

  override def enumDeclaration(curClass: String, enumName: String, enumColl: Map[Long, String]): Unit = {
    val enumClass = type2class(enumName)

    out.puts
    out.puts(s"public enum $enumClass")
    out.puts(s"{")
    out.inc

    enumColl.foreach { case (id, label) =>
      out.puts(s"${publicMemberName(label)} = $id,")
    }

    out.dec
    out.puts("}")
  }

  /**
    * Determine .NET data type corresponding to a KS data type.
    *
    * @param attrType KS data type
    * @return .NET data type
    */
  def kaitaiType2NativeType(attrType: BaseType): String = {
    attrType match {
      case Int1Type(false) => "byte"
      case IntMultiType(false, Width2, _) => "ushort"
      case IntMultiType(false, Width4, _) => "uint"
      case IntMultiType(false, Width8, _) => "ulong"

      case Int1Type(true) => "sbyte"
      case IntMultiType(true, Width2, _) => "short"
      case IntMultiType(true, Width4, _) => "int"
      case IntMultiType(true, Width8, _) => "long"

      case CalcIntType => "int"

      case _: StrType => "string"
      case _: BytesType => "byte[]"

      case t: UserType => types2class(t.name)
      case EnumType(name, _) => type2class(name)

      case ArrayType(inType) => s"List<${kaitaiType2NativeType(inType)}>"
    }
  }

  def lowerCamelCase(s: String): String = {
    if (s == "_root" || s == "_parent" || s == "_io") {
      s
    } else if (s.startsWith("_raw_")) {
      "_raw_" + lowerCamelCase(s.substring("_raw_".length))
    } else {
      Utils.lowerCamelCase(s)
    }
  }

  def types2class(names: List[String]) = names.map(x => type2class(x)).mkString(".")

  def publicMemberName(ksName: String): String = Utils.upperCamelCase(ksName)

  override def privateMemberName(ksName: String): String = s"m${Utils.upperCamelCase(ksName)}"

  def kstructName = "Kaitai.KaitaiStruct"

  def kstreamName = "KaitaiStream"

  def type2class(name: String): String = name match {
    case "kaitai_struct" => kstructName
    case "kaitai_stream" => kstreamName
    case _ => Utils.upperCamelCase(name)
  }
}

object CSharpCompiler extends LanguageCompilerStatic with UpperCamelCaseClasses {
  override def indent: String = "    "
  override def outFileName(topClassName: String): String = s"${type2class(topClassName)}.cs"
}
