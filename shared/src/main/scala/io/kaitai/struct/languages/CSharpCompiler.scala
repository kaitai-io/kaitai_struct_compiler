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
        out.puts(s"this.$varDest = ${normalIO}.processXorInt(this.$varSrc, ${expression(xorValue)});")
      case ProcessZlib =>
        out.puts(s"this.$varDest = ${normalIO}.processZlib(this.$varSrc);")
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        out.puts(s"this.$varDest = ${normalIO}.processRotateLeft(this.$varSrc, $expr, 1);")
    }
  }

  override def normalIO: String = "_io"

  override def allocateIO(varName: String, rep: RepeatSpec): String = {
    val javaName = lowerCamelCase(varName)

    val ioName = s"_io_$javaName"

    val args = rep match {
      case RepeatEos | RepeatExpr(_) => s"$javaName.get($javaName.size() - 1)"
      case NoRepeat => javaName
    }

    out.puts(s"$kstreamName $ioName = new $kstreamName($args);")
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
      out.puts(s"this._raw_${lowerCamelCase(id)} = new ArrayList<byte[]>();")
    out.puts(s"this.${lowerCamelCase(id)} = new ${kaitaiType2NativeType(ArrayType(dataType))}();")
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
      out.puts(s"this._raw_${lowerCamelCase(id)} = new ArrayList<byte[]>((int) (${expression(repeatExpr)}));")
    out.puts(s"${lowerCamelCase(id)} = new ${kaitaiType2NativeType(ArrayType(dataType))}((int) (${expression(repeatExpr)}));")
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
        s"new ${types2class(t.name)}($io, this, _root)"
    }
  }

  override def instanceDeclaration(attrName: String, attrType: BaseType, condSpec: ConditionalSpec): Unit = {
    out.puts(s"private ${kaitaiType2JavaTypeBoxed(attrType)} ${lowerCamelCase(attrName)};")
  }

  override def instanceHeader(className: String, instName: String, dataType: BaseType): Unit = {
    out.puts(s"public ${kaitaiType2JavaTypeBoxed(dataType)} ${lowerCamelCase(instName)}() throws IOException {")
    out.inc
  }

  override def instanceAttrName(instName: String): String = instName

  override def instanceFooter: Unit = classConstructorFooter

  override def instanceCheckCacheAndReturn(instName: String): Unit = {
    out.puts(s"if (${lowerCamelCase(instName)} != null)")
    out.inc
    instanceReturn(instName)
    out.dec
  }

  override def instanceReturn(instName: String): Unit = {
    out.puts(s"return ${lowerCamelCase(instName)};")
  }

  override def instanceCalculate(instName: String, value: Ast.expr): Unit = {
    out.puts(s"${lowerCamelCase(instName)} = ${expression(value)};")
  }

  override def enumDeclaration(curClass: String, enumName: String, enumColl: Map[Long, String]): Unit = {
    val enumClass = type2class(enumName)

    out.puts
    out.puts(s"public enum $enumClass {")
    out.inc

    val it = enumColl.toIterable
    if (enumColl.size > 1) {
      it.dropRight(1).foreach { case (id, label) =>
        out.puts(s"${value2Const(label)}($id),")
      }
    }
    it.last match {
      case (id, label) =>
        out.puts(s"${value2Const(label)}($id);")
    }

    out.puts
    out.puts("private final long id;")
    out.puts(s"$enumClass(long id) { this.id = id; }")
    out.puts("public long id() { return id; }")
    out.puts(s"private static final Map<Long, $enumClass> byId = new HashMap<Long, $enumClass>(${enumColl.size});")
    out.puts("static {")
    out.inc
    out.puts(s"for ($enumClass e : $enumClass.values())")
    out.inc
    out.puts(s"byId.put(e.id(), e);")
    out.dec
    out.dec
    out.puts("}")
    out.puts(s"public static $enumClass byId(long id) { return byId.get(id); }")
    out.dec
    out.puts("}")
  }

  def value2Const(s: String) = s.toUpperCase

  def kaitaiType2NativeType(attrType: BaseType): String = kaitaiType2JavaTypePrim(attrType)

  /**
    * Determine Java data type corresponding to a KS data type. A "primitive" type (i.e. "int", "long", etc) will
    * be returned if possible.
    *
    * @param attrType KS data type
    * @return Java data type
    */
  def kaitaiType2JavaTypePrim(attrType: BaseType): String = {
    attrType match {
      case Int1Type(false) => "int"
      case IntMultiType(false, Width2, _) => "int"
      case IntMultiType(false, Width4, _) => "long"
      case IntMultiType(false, Width8, _) => "long"

      case Int1Type(true) => "byte"
      case IntMultiType(true, Width2, _) => "short"
      case IntMultiType(true, Width4, _) => "int"
      case IntMultiType(true, Width8, _) => "long"

      case _: StrType => "String"
      case _: BytesType => "byte[]"

      case t: UserType => types2class(t.name)
      case EnumType(name, _) => type2class(name)

      case ArrayType(inType) => kaitaiType2JavaTypeBoxed(attrType)
    }
  }

  /**
    * Determine Java data type corresponding to a KS data type. A non-primitive type (i.e. "Integer", "Long", etc) will
    * be returned, to be used when proper objects should be used.
    *
    * @param attrType KS data type
    * @return Java data type
    */
  def kaitaiType2JavaTypeBoxed(attrType: BaseType): String = {
    attrType match {
      case Int1Type(false) => "Integer"
      case IntMultiType(false, Width2, _) => "Integer"
      case IntMultiType(false, Width4, _) => "Long"
      case IntMultiType(false, Width8, _) => "Long"

      case Int1Type(true) => "Byte"
      case IntMultiType(true, Width2, _) => "Short"
      case IntMultiType(true, Width4, _) => "Integer"
      case IntMultiType(true, Width8, _) => "Long"

      case CalcIntType => "Integer"

      case _: StrType => "String"
      case _: BytesType => "byte[]"

      case t: UserType => type2class(t.name.last)
      case EnumType(name, _) => type2class(name)

      case ArrayType(inType) => s"ArrayList<${kaitaiType2JavaTypeBoxed(inType)}>"
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
