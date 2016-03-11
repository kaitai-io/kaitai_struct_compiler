package io.kaitai.struct.languages

import io.kaitai.struct.LanguageOutputWriter
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format._
import io.kaitai.struct.translators.{BaseTranslator, PythonTranslator, TypeProvider}

class PythonCompiler(verbose: Boolean, out: LanguageOutputWriter) extends LanguageCompiler(verbose, out) with UpperCamelCaseClasses with EveryReadIsExpression {
  override def getTranslator(tp: TypeProvider): BaseTranslator = new PythonTranslator(tp)

  override def fileHeader(topClassName: String): Unit = {
    out.puts(s"# $headerComment")
    out.puts
    out.puts("from kaitaistruct import KaitaiStruct")
    out.puts("import array")
    out.puts("import cStringIO")
    out.puts("from enum import Enum")
    out.puts
  }

  override def classHeader(name: String): Unit = {
    out.puts(s"class ${type2class(name)}(KaitaiStruct):")
    out.inc

    // Helper method to read from local file
    out.puts("@staticmethod")
    out.puts("def from_file(filename):")
    out.inc
    out.puts(s"return ${type2class(name)}(open(filename, 'rb'))")
    out.dec
    out.puts
  }

  override def classFooter(name: String): Unit = {
    out.dec
    out.puts
  }

  override def classConstructorHeader(name: String, parentClassName: String, rootClassName: String): Unit = {
    out.puts("def __init__(self, _io, _parent = None, _root = None):")
    out.inc
    out.puts("self._io = _io")
    out.puts("self._parent = _parent")
    out.puts("self._root = _root if _root else self")
  }

  override def classConstructorFooter: Unit = classFooter(null)

  override def attributeDeclaration(attrName: String, attrType: BaseType): Unit = {}

  override def attributeReader(attrName: String, attrType: BaseType): Unit = {}

  override def attrFixedContentsParse(attrName: String, contents: Array[Byte]): Unit = {
    out.puts(s"self.${attrName} = self.ensure_fixed_contents(${contents.size}, array.array('B', [${contents.mkString(", ")}]))")
  }

  override def attrProcess(proc: ProcessExpr, varSrc: String, varDest: String): Unit = {
    proc match {
      case ProcessXor(xorValue) =>
        out.puts(s"self.$varDest = array.array('B', self.$varSrc);")
        out.puts(s"for i in xrange(len(self.$varDest)):")
        out.inc
        out.puts(s"self.$varDest[i] ^= ${expression(xorValue)}")
        out.dec
        out.puts
        out.puts(s"self.$varDest = self.$varDest.tostring()")
    }
  }

  override def normalIO: String = "self._io"

  override def allocateIO(varName: String, rep: RepeatSpec): String = {
    val args = rep match {
      case RepeatEos => s"$varName[-1]"
      case RepeatExpr(_) => s"$varName[i]"
      case NoRepeat => varName
    }

    out.puts(s"io = cStringIO.StringIO(self.$args)")
    "io"
  }

  override def useIO(ioEx: expr): String = {
    out.puts(s"io = ${expression(ioEx)}")
    "io"
  }

  override def seek(io: String, pos: Ast.expr): Unit = {
    out.puts(s"${io}.seek(${expression(pos)})")
  }

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
      out.puts(s"self._raw_${id} = []")
    out.puts(s"self.${id} = []")
    out.puts(s"while not self.is_io_eof(${io}):")
    out.inc
  }
  override def handleAssignmentRepeatEos(id: String, expr: String): Unit =
    out.puts(s"self.${id}.append(${expr})")
  override def condRepeatEosFooter: Unit = {
    out.dec
    out.puts
  }

  override def condRepeatExprHeader(id: String, io: String, dataType: BaseType, needRaw: Boolean, repeatExpr: expr): Unit = {
    if (needRaw)
      out.puts(s"self._raw_${id} = [None] * ${expression(repeatExpr)}")
    out.puts(s"self.${id} = [None] * ${expression(repeatExpr)}")
    out.puts(s"for i in xrange(${expression(repeatExpr)}):")
    out.inc
  }
  override def handleAssignmentRepeatExpr(id: String, expr: String): Unit =
    out.puts(s"self.${id}[i] = ${expr}")
  override def condRepeatExprFooter: Unit = {
    out.dec
    out.puts
  }

  override def handleAssignmentSimple(id: String, expr: String): Unit =
    out.puts(s"self.${id} = ${expr}")

  override def parseExpr(dataType: BaseType, io: String): String = {
    dataType match {
      case t: IntType =>
        s"self.read_${t.apiCall}()"
      // Aw, crap, can't use interpolated strings here: https://issues.scala-lang.org/browse/SI-6476
      case StrByteLimitType(bs, encoding) =>
        s"self.read_str_byte_limit(${expression(bs)}, " + '"' + encoding + "\")"
      case StrEosType(encoding) =>
        "self.read_str_eos(\"" + encoding + "\")"
      case StrZType(encoding, terminator, include, consume, eosError) =>
        "self.read_strz(\"" + encoding + '"' + s", ${terminator}, ${bool2Py(include)}, ${bool2Py(consume)}, ${bool2Py(eosError)})"
      case EnumType(enumName, t) =>
        s"self._root.${type2class(enumName)}(self.read_${t.apiCall}())"

      case BytesLimitType(size, _) =>
        s"self._io.read(${expression(size)})"
      case BytesEosType(_) =>
        s"self._io.read()"
      case t: UserType =>
        s"self._root.${type2class(t.name)}($io, self, self._root)"
    }
  }

  override def instanceHeader(className: String, instName: String, dataType: BaseType): Unit = {
    out.puts("@property")
    out.puts(s"def ${instName}(self):")
    out.inc
  }

  override def instanceAttrName(instName: String) = s"_m_${instName}"

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

  override def instanceCalculate(instName: String, value: Ast.expr): Unit = {
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
}

object PythonCompiler extends LanguageCompilerStatic {
  override def indent: String = "    "
  override def outFileName(topClassName: String): String = s"${topClassName}.py"
}
