package io.kaitai.struct.languages

import io.kaitai.struct.LanguageOutputWriter
import io.kaitai.struct.format.{ProcessExpr, AttrSpec}

class PythonCompiler(outFileName: String) extends LanguageCompiler with UpperCamelCaseClasses with EveryReadIsExpression {
  val out = new LanguageOutputWriter(outFileName, "    ")

  override def fileHeader(sourceFileName: String, topClassName: String): Unit = {
    out.puts(s"# This file was generated from '${sourceFileName}' with kaitai_struct compiler")
    out.puts
    out.puts("from kaitaistruct import KaitaiStruct")
    out.puts("import array")
    out.puts("import cStringIO")
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

  override def classFooter: Unit = {
    out.dec
    out.puts
  }

  override def classConstructorHeader(name: String): Unit = {
    out.puts("def __init__(self, _io, _parent = None):")
    out.inc
    out.puts("self._io = _io")
    out.puts("self._parent = _parent")
  }

  override def classConstructorFooter: Unit = classFooter

  override def attributeDeclaration(attrName: String, attrType: String, isArray: Boolean): Unit = {}

  override def attributeReader(attrName: String, attrType: String, isArray: Boolean): Unit = {}

  override def attrFixedContentsParse(attrName: String, contents: Array[Byte]): Unit = {
    out.puts(s"self.${attrName} = self.ensure_fixed_contents(${contents.size}, array.array('B', [${contents.mkString(", ")}]))")
  }

  override def attrNoTypeWithSize(varName: String, size: String) {
    out.puts(s"self.${varName} = self._io.read(${expression2Python(size)})")
  }

  override def attrNoTypeWithSizeEos(varName: String) {
    out.puts(s"self.${varName} = self._io.read()")
  }

  override def attrUserTypeParse(id: String, attr: AttrSpec, io: String): Unit = {
    handleAssignment(id, attr, s"self.${type2class(attr.dataType)}(${io}, self)", io)
  }

  override def attrProcess(proc: ProcessExpr, varSrc: String, varDest: String): Unit = ???

  override def normalIO: String = "self._io"

  override def allocateIO(varName: String): String = {
    out.puts(s"io = cStringIO.StringIO(self.${varName})")
    "io"
  }

  override def seek(io: String, pos: String): Unit = {
    out.puts(s"${io}.seek(${expression2Python(pos)})")
  }

  override def handleAssignment(id: String, attr: AttrSpec, expr: String, io: String): Unit = {
    if (attr.ifExpr.isDefined) {
      out.puts(s"if ${attr.ifExpr.get}:")
      out.inc
    }

    attr.repeat match {
      case Some("eos") =>
        out.puts(s"self.${id} = []")
        out.puts(s"while not self.is_io_eof(${io}):")
        out.inc
        out.puts(s"self.${id}.append(${expr})")
        out.dec
        out.puts
      case Some("expr") =>
        attr.repeatExpr match {
          case Some(repeatExpr) =>
            out.puts(s"self.${id} = [None] * ${expression2Python(repeatExpr)}")
            out.puts(s"for i in xrange(${expression2Python(repeatExpr)}):")
            out.inc
            out.puts(s"self.${id}[i] = ${expr}")
            out.dec
            out.puts

          case None =>
            throw new RuntimeException("repeat: expr, but no repeat-expr value given")
        }
      case None => out.puts(s"self.${id} = ${expr}")
    }

    if (attr.ifExpr.isDefined) {
      out.dec
      out.puts
    }
  }

  override def stdTypeParseExpr(attr: AttrSpec, endian: Option[String]): String = {
    attr.dataType match {
      case "u1" | "s1" | "u2le" | "u2be" | "u4le" | "u4be" | "u8le" | "u8be" | "s2le" | "s2be" | "s4le" | "s4be" | "s8le" | "s8be"  =>
        s"self.read_${attr.dataType}()"
      case "u2" | "u4" | "u8" | "s2" | "s4" | "s8" =>
        endian match {
          case Some(e) => s"self.read_${attr.dataType}${e}()"
          case None => throw new RuntimeException(s"Unable to parse ${attr.dataType} with no default endianess defined")
        }
      case null => throw new RuntimeException("should never happen")
      // Aw, crap, can't use interpolated strings here: https://issues.scala-lang.org/browse/SI-6476

      case "str" =>
        ((attr.byteSize, attr.sizeEos)) match {
          case (Some(bs: String), false) =>
            s"self.read_str_byte_limit(${expression2Python(bs)}, " + '"' + attr.encoding.get + "\")"
          case (None, true) =>
            "self.read_str_eos(\"" + attr.encoding.get + "\")"
          case (None, false) =>
            throw new RuntimeException("type str: either \"byte_size\" or \"size_eos\" must be specified")
          case (Some(_), true) =>
            throw new RuntimeException("type str: only one of \"byte_size\" or \"size_eos\" must be specified")
        }
      case "strz" =>
        "self.read_strz(\"" + attr.encoding.get + '"' + s", ${attr.terminator}, ${bool2Py(attr.include)}, ${bool2Py(attr.consume)}, ${bool2Py(attr.eosError)})"
    }
  }

  override def instanceHeader(instName: String, dataType: String, isArray: Boolean): Unit = {
    out.puts("@property")
    out.puts(s"def ${instName}(self):")
    out.inc
  }

  override def instanceAttrName(instName: String) = s"_m_${instName}"

  override def instanceFooter: Unit = classFooter

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

  val ReInt = "^\\d+$".r
  val ReLiteral = "^[A-Za-z][A-Za-z0-9_]*$".r

  def expression2Python(s: String): String = {
    s match {
      case ReInt() => s
      case ReLiteral() => s"self.${s}"
    }
  }

  def bool2Py(b: Boolean): String = if (b) { "True" } else { "False" }
}
