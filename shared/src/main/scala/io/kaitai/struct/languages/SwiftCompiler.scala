package io.kaitai.struct.languages

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format._
import io.kaitai.struct.translators.{BaseTranslator, JavaTranslator, TypeProvider}
import io.kaitai.struct.{LanguageOutputWriter, Utils}

class SwiftCompiler(verbose: Boolean, out: LanguageOutputWriter)
  extends LanguageCompiler(verbose, out)
    with EveryReadIsExpression
    with NoNeedForFullClassPath {
  import SwiftCompiler._

  override def getTranslator(tp: TypeProvider): BaseTranslator = new JavaTranslator(tp)

  override def fileHeader(topClassName: String): Unit = {
    out.puts(s"// $headerComment")
    out.puts
    // TODO: some imports here?

    out.puts
  }

  override def classHeader(name: String): Unit = {
    out.puts(s"class ${type2class(name)} {")
    out.inc
  }

  override def classFooter(name: String): Unit = {
    out.dec
    out.puts("}")
  }

  override def classConstructorHeader(name: String, parentClassName: String, rootClassName: String): Unit = {
    out.puts("let _io: KaitaiStream")

    if (name == rootClassName) {
      out.puts("var _root: HelloWorld { return self }")
    }

    out.puts
    val rootStr = if (name == rootClassName) {
      ""
    } else {
      s", parent: ${type2class(parentClassName)}, root: ${type2class(rootClassName)}"
    }

    out.puts(s"init(io: KaitaiStream$rootStr) {")
    out.inc
    out.puts("self._io = io")
    if (name != rootClassName) {
      out.puts("self._parent = parent")
      out.puts("self._root = root")
    }
  }

  override def classConstructorFooter: Unit = classFooter(null)

  override def attributeDeclaration(attrName: String, attrType: BaseType): Unit = {
    out.puts(s"let ${lowerCamelCase(attrName)}: ${kaitaiType2LangType(attrType)}")
  }

  override def attributeReader(attrName: String, attrType: BaseType): Unit = {
    // Swift's attributes are automatically public and readable
  }

  override def attrFixedContentsParse(attrName: String, contents: Array[Byte]): Unit = {
    out.puts(s"this.${lowerCamelCase(attrName)} = _io.ensureFixedContents(${contents.length}, new byte[] { ${contents.mkString(", ")} });")
  }

  override def attrProcess(proc: ProcessExpr, varSrc: String, varDest: String): Unit = {
    proc match {
      case ProcessXor(xorValue) =>
        out.puts(s"this.$varDest = new byte[this.$varSrc.length];")
        out.puts(s"for (int i = 0; i < this.$varSrc.length; i++) {")
        out.inc
        out.puts(s"this.$varDest[i] = (byte) (this.$varSrc[i] ^ (${expression(xorValue)}));")
        out.dec
        out.puts("}")
      case ProcessZlib =>
        out.puts(s"this.$varDest = _io.processZlib(this.$varSrc);")
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        out.puts(s"this.$varDest = _io.processRotateLeft(this.$varSrc, $expr, 1);")
    }
  }

  override def normalIO: String = "_io"

  override def allocateIO(varName: String, rep: RepeatSpec): String = {
    val tmpName = lowerCamelCase(varName)

    val ioName = s"_io_$tmpName"

    val args = rep match {
      case RepeatEos | RepeatExpr(_) => s"$tmpName.get($tmpName.size() - 1)"
      case NoRepeat => tmpName
    }

    out.puts(s"let $ioName = KaitaiStream($args);")
    ioName
  }

  override def useIO(ioEx: expr): String = {
    out.puts(s"let io = ${expression(ioEx)};")
    "io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"let _pos = $io.pos()")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io.seek(${expression(pos)})")

  override def popPos(io: String): Unit =
    out.puts(s"$io.seek(_pos)")

  override def condIfHeader(expr: expr): Unit = {
    out.puts(s"if ${expression(expr)} {")
    out.inc
  }

  override def condIfFooter(expr: expr): Unit = {
    out.dec
    out.puts("}")
  }

  override def condRepeatEosHeader(id: String, io: String, dataType: BaseType, needRaw: Boolean): Unit = {
    if (needRaw)
      out.puts(s"self._raw_${lowerCamelCase(id)} = new ArrayList<byte[]>();")
    out.puts(s"self.${lowerCamelCase(id)} = ${kaitaiType2LangType(ArrayType(dataType))}();")
    out.puts(s"while (!$io.isEof()) {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: String, expr: String): Unit = {
    out.puts(s"self.${lowerCamelCase(id)}.add($expr);")
  }

  override def condRepeatEosFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def condRepeatExprHeader(id: String, io: String, dataType: BaseType, needRaw: Boolean, repeatExpr: expr): Unit = {
    if (needRaw)
      out.puts(s"self._raw_${lowerCamelCase(id)} = new ArrayList<byte[]>((int) (${expression(repeatExpr)}));")
    out.puts(s"${lowerCamelCase(id)} = new ${kaitaiType2LangType(ArrayType(dataType))}((int) (${expression(repeatExpr)}));")
    out.puts(s"for (int i = 0; i < ${expression(repeatExpr)}; i++) {")
    out.inc
  }

  override def handleAssignmentRepeatExpr(id: String, expr: String): Unit = {
    out.puts(s"self.${lowerCamelCase(id)}.add($expr)")
  }

  override def condRepeatExprFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def handleAssignmentSimple(id: String, expr: String): Unit = {
    out.puts(s"self.${lowerCamelCase(id)} = $expr")
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
        s"${types2class(t.name)}($io, this, _root)"
    }
  }

  override def instanceDeclaration(attrName: String, attrType: BaseType): Unit = {
    out.puts(s"private ${kaitaiType2LangType(attrType)} ${lowerCamelCase(attrName)};")
  }

  override def instanceHeader(className: String, instName: String, dataType: BaseType): Unit = {
    out.puts(s"public ${kaitaiType2LangType(dataType)} ${lowerCamelCase(instName)}() throws IOException {")
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

  def kaitaiType2LangType(attrType: BaseType): String = {
    attrType match {
      case Int1Type(false) | IntMultiType(false, _, _) => "UInt"
      case Int1Type(true) | IntMultiType(true, _, _) => "Int"

      case _: StrType => "String"
      case _: BytesType => "NSData"

      case t: UserType => types2class(t.name)
      case EnumType(name, _) => type2class(name)

      case ArrayType(inType) => s"Array<${kaitaiType2LangType(inType)}>"
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
}

object SwiftCompiler extends LanguageCompilerStatic with UpperCamelCaseClasses {
  override def indent: String = "    "
  override def outFileName(topClassName: String): String = s"${type2class(topClassName)}.swift"
}
