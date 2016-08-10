package io.kaitai.struct.languages

import io.kaitai.struct.LanguageOutputWriter
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format._
import io.kaitai.struct.translators.{BaseTranslator, PythonTranslator, TypeProvider}

class PythonCompiler(verbose: Boolean, out: LanguageOutputWriter)
  extends LanguageCompiler(verbose, out)
    with UpperCamelCaseClasses
    with EveryReadIsExpression
    with NoNeedForFullClassPath {

  override def getStatic = PythonCompiler

  override def fileHeader(topClassName: String): Unit = {
    out.puts(s"# $headerComment")
    out.puts
    out.puts("import array")
    out.puts("import cStringIO")
    out.puts("import zlib")
    out.puts("from enum import Enum")
    out.puts
    out.puts("from kaitaistruct import KaitaiStruct, KaitaiStream")
    out.puts
    out.puts
  }

  override def classHeader(name: String): Unit = {
    out.puts(s"class ${type2class(name)}(KaitaiStruct):")
    out.inc

    // Helper method to read from local file
    out.puts("@staticmethod")
    out.puts("def from_file(filename):")
    out.inc
    out.puts(s"return ${type2class(name)}(KaitaiStream(open(filename, 'rb')))")
    out.dec
    out.puts
  }

  override def classFooter(name: String): Unit = {
    out.dec
    out.puts
  }

  override def classConstructorHeader(name: String, parentClassName: String, rootClassName: String): Unit = {
    out.puts("def __init__(self, _io, _parent=None, _root=None):")
    out.inc
    out.puts("self._io = _io")
    out.puts("self._parent = _parent")
    out.puts("self._root = _root if _root else self")
  }

  override def classConstructorFooter: Unit = classFooter(null)

  override def attributeDeclaration(attrName: String, attrType: BaseType, condSpec: ConditionalSpec): Unit = {}

  override def attributeReader(attrName: String, attrType: BaseType): Unit = {}

  override def attrFixedContentsParse(attrName: String, contents: Array[Byte]): Unit = {
    out.puts(s"self.$attrName = self._io.ensure_fixed_contents(${contents.length}, array.array('B', [${contents.mkString(", ")}]))")
  }

  override def attrProcess(proc: ProcessExpr, varSrc: String, varDest: String): Unit = {
    proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "process_xor_one"
          case _: BytesType => "process_xor_many"
        }
        out.puts(s"${privateMemberName(varDest)} = KaitaiStream.$procName(${privateMemberName(varSrc)}, ${expression(xorValue)})")
      case ProcessZlib =>
        out.puts(s"${privateMemberName(varDest)} = zlib.decompress(${privateMemberName(varSrc)})")
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        out.puts(s"${privateMemberName(varDest)} = KaitaiStream.process_rotate_left(${privateMemberName(varSrc)}, $expr, 1)")
    }
  }

  override def normalIO: String = "self._io"

  override def allocateIO(varName: String, rep: RepeatSpec): String = {
    val args = rep match {
      case RepeatEos => s"$varName[-1]"
      case RepeatExpr(_) => s"$varName[i]"
      case NoRepeat => varName
    }

    out.puts(s"io = KaitaiStream(cStringIO.StringIO(self.$args))")
    "io"
  }

  override def useIO(ioEx: expr): String = {
    out.puts(s"io = ${expression(ioEx)}")
    "io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"_pos = $io.pos()")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io.seek(${expression(pos)})")

  override def popPos(io: String): Unit =
    out.puts(s"$io.seek(_pos)")

  override def condIfHeader(expr: Ast.expr): Unit = {
    out.puts(s"if ${expression(expr)}:")
    out.inc
  }
  override def condIfFooter(expr: Ast.expr): Unit = {
    out.dec
    out.puts
  }

  override def condRepeatEosHeader(id: String, io: String, dataType: BaseType, needRaw: Boolean): Unit = {
    if (needRaw)
      out.puts(s"self._raw_$id = []")
    out.puts(s"self.$id = []")
    out.puts(s"while not $io.is_eof():")
    out.inc
  }
  override def handleAssignmentRepeatEos(id: String, expr: String): Unit =
    out.puts(s"self.$id.append($expr)")
  override def condRepeatEosFooter: Unit = {
    out.dec
    out.puts
  }

  override def condRepeatExprHeader(id: String, io: String, dataType: BaseType, needRaw: Boolean, repeatExpr: expr): Unit = {
    if (needRaw)
      out.puts(s"self._raw_$id = [None] * ${expression(repeatExpr)}")
    out.puts(s"self.$id = [None] * ${expression(repeatExpr)}")
    out.puts(s"for i in xrange(${expression(repeatExpr)}):")
    out.inc
  }
  override def handleAssignmentRepeatExpr(id: String, expr: String): Unit =
    out.puts(s"self.$id[i] = $expr")
  override def condRepeatExprFooter: Unit = {
    out.dec
    out.puts
  }

  override def handleAssignmentSimple(id: String, expr: String): Unit =
    out.puts(s"self.$id = $expr")

  override def parseExpr(dataType: BaseType, io: String): String = {
    dataType match {
      case t: ReadableType =>
        s"$io.read_${t.apiCall}()"
      // Aw, crap, can't use interpolated strings here: https://issues.scala-lang.org/browse/SI-6476
      case StrByteLimitType(bs, encoding) =>
        s"$io.read_str_byte_limit(${expression(bs)}, " + '"' + encoding + "\")"
      case StrEosType(encoding) =>
        io + ".read_str_eos(\"" + encoding + "\")"
      case StrZType(encoding, terminator, include, consume, eosError) =>
        io + ".read_strz(\"" + encoding + '"' + s", $terminator, ${bool2Py(include)}, ${bool2Py(consume)}, ${bool2Py(eosError)})"
      case EnumType(enumName, t) =>
        s"self._root.${type2class(enumName)}(${parseExpr(t, io)})"

      case BytesLimitType(size, _) =>
        s"$io.read_bytes(${expression(size)})"
      case BytesEosType(_) =>
        s"$io.read_bytes_full()"
      case t: UserType =>
        s"self._root.${types2class(t.name)}($io, self, self._root)"
    }
  }

  override def instanceHeader(className: String, instName: String, dataType: BaseType): Unit = {
    out.puts("@property")
    out.puts(s"def $instName(self):")
    out.inc
  }

  override def instanceAttrName(instName: String) = s"_m_$instName"

  override def instanceFooter: Unit = classConstructorFooter

  override def instanceCheckCacheAndReturn(instName: String): Unit = {
    out.puts(s"if hasattr(self, '${instanceAttrName(instName)}'):")
    out.inc
    instanceReturn(instName)
    out.dec
    out.puts
  }

  override def instanceReturn(instName: String): Unit = {
    out.puts(s"return self.${instanceAttrName(instName)}")
  }

  override def instanceCalculate(instName: String, dataType: BaseType, value: expr): Unit = {
    out.puts(s"self.${instanceAttrName(instName)} = ${expression(value)};")
  }

  override def enumDeclaration(curClass: String, enumName: String, enumColl: Map[Long, String]): Unit = {
    out.puts
    out.puts(s"class ${type2class(enumName)}(Enum):")
    out.inc
    enumColl.foreach { case (id: Long, label: String) => out.puts(s"$label = $id") }
    out.dec
  }

  def bool2Py(b: Boolean): String = if (b) { "True" } else { "False" }

  def types2class(names: List[String]) = names.map(x => type2class(x)).mkString(".")

  override def privateMemberName(ksName: String): String = s"self.$ksName"
}

object PythonCompiler extends LanguageCompilerStatic {
  override def getTranslator(tp: TypeProvider): BaseTranslator = new PythonTranslator(tp)
  override def indent: String = "    "
  override def outFileName(topClassName: String): String = s"$topClassName.py"
}
