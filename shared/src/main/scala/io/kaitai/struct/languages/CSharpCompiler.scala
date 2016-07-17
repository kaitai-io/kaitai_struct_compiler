package io.kaitai.struct.languages

import io.kaitai.struct.{LanguageOutputWriter, RuntimeConfig, Utils}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format._
import io.kaitai.struct.translators.{BaseTranslator, CSharpTranslator, TypeProvider}

class CSharpCompiler(verbose: Boolean, out: LanguageOutputWriter, namespace: String = "")
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

    out.puts(s"public partial class ${type2class(name)} : KaitaiStruct")
    out.puts(s"{")
    out.inc

    out.puts(s"public static ${type2class(name)} FromFile(string fileName)")
    out.puts(s"{")
    out.inc
    out.puts(s"return new ${type2class(name)}(new KaitaiStream(fileName));")
    out.dec
    out.puts("}")
  }

  override def classFooter(name: String): Unit = {
    out.dec
    out.puts("}")
  }

  override def classConstructorHeader(name: String, parentClassName: String, rootClassName: String): Unit = {
    out.puts
    out.puts(s"public ${type2class(name)}(KaitaiStream stream) : this(stream, null)")
    out.puts(s"{")
    out.puts
    out.puts("}")
    out.puts

    out.puts(s"public ${type2class(name)}(KaitaiStream stream, ${type2class(parentClassName)} parent) : this(stream, parent, null)")
    out.puts(s"{")
    out.inc
    if (name == rootClassName)
      out.puts("_root = this;")
    out.dec
    out.puts("}")
    out.puts

    out.puts(s"public ${type2class(name)}(KaitaiStream stream, ${type2class(parentClassName)} parent, ${type2class(rootClassName)} root) : base(stream)")
    out.puts(s"{")
    out.inc
    out.puts("_parent = parent;")
    out.puts("_root = root;")
    out.puts("ParseInternal();")
    out.dec
    out.puts("}")
    out.puts

    out.puts("private void ParseInternal()")
    out.puts("{")
    out.inc
  }

  override def classConstructorFooter: Unit = {
    out.dec
    out.puts("}")
    out.puts
  }

  override def attributeDeclaration(attrName: String, attrType: BaseType, condSpec: ConditionalSpec): Unit = {
    out.puts(s"private ${kaitaiType2NativeType(attrType)} ${camelCase(attrName)};")
  }

  override def attributeReader(attrName: String, attrType: BaseType): Unit = {
    out.puts(s"public ${kaitaiType2NativeType(attrType)} Get${pascalCase(attrName)}() { return ${camelCase(attrName)}; }")
  }

  override def attrFixedContentsParse(attrName: String, contents: Array[Byte]): Unit = {
    out.puts(s"${camelCase(attrName)} = _stream.EnsureFixedContents(${contents.length}, new byte[] { ${contents.mkString(", ")} });")
  }

  override def attrProcess(proc: ProcessExpr, varSrc: String, varDest: String): Unit = {
    proc match {
      case ProcessXor(xorValue) =>
        out.puts(s"this.$varDest = _stream.ProcessXorInt(this.$varSrc, ${expression(xorValue)});")
      case ProcessZlib =>
        out.puts(s"$varDest = _stream.ProcessZlib($varSrc);")
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        out.puts(s"$varDest = _stream.ProcessRotateLeft($varSrc, $expr, 1);")
    }
  }

  override def normalIO: String = "_stream"

  override def allocateIO(varName: String, rep: RepeatSpec): String = {
    val camelVarName = camelCase(varName)

    val ioName = s"stream_$camelVarName"

    val args = rep match {
      case RepeatEos | RepeatExpr(_) => s"$camelVarName[$camelVarName.Count - 1]"
      case NoRepeat => camelVarName
    }

    out.puts(s"KaitaiStream $ioName = new KaitaiStream($args);")
    ioName
  }

  override def useIO(ioEx: expr): String = {
    out.puts(s"KaitaiStream io = ${expression(ioEx)};")
    "io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"long _pos = $io.Pos();")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io.Seek(${expression(pos)});")

  override def popPos(io: String): Unit =
    out.puts(s"$io.Seek(_pos);")

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
      out.puts(s"_raw_${id} = new List<byte[]>();")
    out.puts(s"${camelCase(id)} = new ${kaitaiType2NativeType(ArrayType(dataType))}();")
    out.puts(s"while (!$io.IsEof()) {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: String, expr: String): Unit = {
    out.puts(s"${camelCase(id)}.Add($expr);")
  }

  override def condRepeatEosFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def condRepeatExprHeader(id: String, io: String, dataType: BaseType, needRaw: Boolean, repeatExpr: expr): Unit = {
    if (needRaw)
      out.puts(s"_raw_${id} = new List<byte[]>((int) (${expression(repeatExpr)}));")
    out.puts(s"${camelCase(id)} = new ${kaitaiType2NativeType(ArrayType(dataType))}((int) (${expression(repeatExpr)}));")
    out.puts(s"for (int i = 0; i < ${expression(repeatExpr)}; i++) {")
    out.inc
  }

  override def handleAssignmentRepeatExpr(id: String, expr: String): Unit = {
    out.puts(s"${camelCase(id)}.Add($expr);")
  }

  override def condRepeatExprFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def handleAssignmentSimple(id: String, expr: String): Unit = {
    out.puts(s"${camelCase(id)} = $expr;")
  }

  override def parseExpr(dataType: BaseType, io: String): String = {
    dataType match {
      case t: IntType =>
        s"$io.Read${Utils.capitalize(t.apiCall)}()"
      // Aw, crap, can't use interpolated strings here: https://issues.scala-lang.org/browse/SI-6476
      case StrByteLimitType(bs, encoding) =>
        s"$io.ReadStringByteLimit(${expression(bs)}, " + '"' + encoding + "\")"
      case StrEosType(encoding) =>
        io + ".ReadStringEos(\"" + encoding + "\")"
      case StrZType(encoding, terminator, include, consume, eosError) =>
        io + ".ReadStringTerminated(\"" + encoding + '"' + s", $terminator, $include, $consume, $eosError)"
      case EnumType(enumName, t) =>
        s"((${type2class(enumName)}) $io.Read${Utils.capitalize(t.apiCall)}())"
      case BytesLimitType(size, _) =>
        s"$io.ReadBytes(${expression(size)})"
      case BytesEosType(_) =>
        s"$io.ReadBytesFull()"
      case t: UserType =>
        s"new ${types2class(t.name)}($io, this, _root)"
    }
  }

  override def instanceDeclaration(attrName: String, attrType: BaseType, condSpec: ConditionalSpec): Unit = {
    out.puts(s"private ${kaitaiType2NativeType(attrType)} ${camelCase(attrName)};")
  }

  override def instanceHeader(className: String, instName: String, dataType: BaseType): Unit = {
    out.puts(s"public ${kaitaiType2NativeType(dataType)} Get${pascalCase(instName)}()")
    out.puts(s"{")
    out.inc

    // Move this out of `instanceCheckCacheAndReturn` because we need to know the datatype
    out.puts(s"if (${camelCase(instName)} != default(${kaitaiType2NativeType(dataType)}))")
    out.inc
    instanceReturn(instName)
    out.dec
  }

  override def instanceAttrName(instName: String): String = instName

  override def instanceFooter: Unit = {
    out.dec
    out.puts(s"}")
  }

  override def instanceCheckCacheAndReturn(instName: String): Unit = {
    // out.puts(s"if (${camelCase(instName)} != null)")
    // out.inc
    // instanceReturn(instName)
    // out.dec
  }

  override def instanceReturn(instName: String): Unit = {
    out.puts(s"return ${camelCase(instName)};")
  }

  override def instanceCalculate(instName: String, value: Ast.expr): Unit = {
    out.puts(s"${camelCase(instName)} = ${expression(value)};")
  }

  override def enumDeclaration(curClass: String, enumName: String, enumColl: Map[Long, String]): Unit = {
    val enumClass = type2class(enumName)

    out.puts
    out.puts(s"public enum $enumClass")
    out.puts(s"{")
    out.inc

    enumColl.foreach { case (id, label) =>
      out.puts(s"${pascalCase(label)} = $id,")
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
      case EnumType(name, _) => pascalCase(name)

      case ArrayType(inType) => s"List<${kaitaiType2NativeType(inType)}>"
    }
  }

  def pascalCase(s: String): String = {
    if (s == "_root" || s == "_parent" || s == "_stream") {
      "Kaitai" + Utils.upperCamelCase(s.substring(1))
    } else if (s.startsWith("_raw_")) {
      Utils.upperCamelCase(s.substring(1))
    } else {
      Utils.upperCamelCase(s)
    }
  }

  def camelCase(s: String): String = {
    if (s == "_root" || s == "_parent" || s == "_stream") {
      s
    } else if (s.startsWith("_raw_")) {
      s
    } else {
      Utils.lowerCamelCase(s)
    }
  }

  def types2class(names: List[String]) = names.map(x => type2class(x)).mkString(".")

  override def privateMemberName(ksName: String): String = s"${Utils.lowerCamelCase(ksName)}"
}

object CSharpCompiler extends LanguageCompilerStatic with UpperCamelCaseClasses {
  override def indent: String = "    "
  override def outFileName(topClassName: String): String = s"${type2class(topClassName)}.cs"
  override def outFilePath(config: RuntimeConfig, outDir: String, topClassName: String): String =
    s"$outDir/src/${config.dotNetNamespace.replace('.', '/')}/${outFileName(topClassName)}"
}
