package io.kaitai.struct.languages

import io.kaitai.struct.{LanguageOutputWriter, Utils}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format._
import io.kaitai.struct.translators.{BaseTranslator, PythonTranslator, TypeProvider}

class PythonCompiler(verbose: Boolean, out: LanguageOutputWriter)
  extends LanguageCompiler(verbose, out)
    with UniversalFooter
    with UpperCamelCaseClasses
    with EveryReadIsExpression
    with AllocateIOLocalVar
    with NoNeedForFullClassPath {

  import PythonCompiler._

  override def getStatic = PythonCompiler

  override def universalFooter: Unit = {
    out.dec
    out.puts
  }

  override def fileHeader(topClassName: String): Unit = {
    out.puts(s"# $headerComment")
    out.puts
    out.puts("import array")
    out.puts("import cStringIO")
    out.puts("import zlib")
    out.puts("from enum import Enum")
    out.puts
    out.puts(s"from kaitaistruct import $kstructName, $kstreamName")
    out.puts
    out.puts
  }

  override def classHeader(name: String): Unit = {
    out.puts(s"class ${type2class(name)}($kstructName):")
    out.inc
  }

  override def classConstructorHeader(name: String, parentClassName: String, rootClassName: String): Unit = {
    out.puts("def __init__(self, _io, _parent=None, _root=None):")
    out.inc
    out.puts("self._io = _io")
    out.puts("self._parent = _parent")
    out.puts("self._root = _root if _root else self")
  }

  override def attributeDeclaration(attrName: Identifier, attrType: BaseType, condSpec: ConditionalSpec): Unit = {}

  override def attributeReader(attrName: Identifier, attrType: BaseType): Unit = {}

  override def attrFixedContentsParse(attrName: Identifier, contents: Array[Byte]): Unit = {
    out.puts(s"${privateMemberName(attrName)} = self._io.ensure_fixed_contents(${contents.length}, array.array('B', [${contents.mkString(", ")}]))")
  }

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit = {
    proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "process_xor_one"
          case _: BytesType => "process_xor_many"
        }
        out.puts(s"${privateMemberName(varDest)} = $kstreamName.$procName(${privateMemberName(varSrc)}, ${expression(xorValue)})")
      case ProcessZlib =>
        out.puts(s"${privateMemberName(varDest)} = zlib.decompress(${privateMemberName(varSrc)})")
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        out.puts(s"${privateMemberName(varDest)} = $kstreamName.process_rotate_left(${privateMemberName(varSrc)}, $expr, 1)")
    }
  }

  override def normalIO: String = "self._io"

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = {
    val varStr = privateMemberName(varName)

    val args = rep match {
      case RepeatEos => s"$varStr[-1]"
      case RepeatExpr(_) => s"$varStr[i]"
      case NoRepeat => varStr
    }

    out.puts(s"io = $kstreamName(cStringIO.StringIO($args))")
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

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = []")
    out.puts(s"${privateMemberName(id)} = []")
    out.puts(s"while not $io.is_eof():")
    out.inc
  }
  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)}.append($expr)")

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean, repeatExpr: expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = [None] * ${expression(repeatExpr)}")
    out.puts(s"${privateMemberName(id)} = [None] * ${expression(repeatExpr)}")
    out.puts(s"for i in xrange(${expression(repeatExpr)}):")
    out.inc
  }
  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)}[i] = $expr")

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)} = $expr")

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

  override def instanceHeader(className: String, instName: InstanceIdentifier, dataType: BaseType): Unit = {
    out.puts("@property")
    out.puts(s"def ${publicMemberName(instName)}(self):")
    out.inc
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier): Unit = {
    out.puts(s"if hasattr(self, '${idToStr(instName)}'):")
    out.inc
    instanceReturn(instName)
    out.dec
    out.puts
  }

  override def instanceReturn(instName: InstanceIdentifier): Unit = {
    out.puts(s"return ${privateMemberName(instName)}")
  }

  override def instanceCalculate(instName: InstanceIdentifier, dataType: BaseType, value: expr): Unit = {
    out.puts(s"${privateMemberName(instName)} = ${expression(value)};")
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

  def idToStr(id: Identifier): String = {
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => name
      case InstanceIdentifier(name) => s"_m_${name}"
      case RawIdentifier(innerId) => s"_raw_${idToStr(innerId)}"
    }
  }

  override def privateMemberName(id: Identifier): String = s"self.${idToStr(id)}"

  override def publicMemberName(id: Identifier): String = {
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => name
      case InstanceIdentifier(name) => name
      case RawIdentifier(innerId) => s"_raw_${publicMemberName(innerId)}"
    }
  }
}

object PythonCompiler extends LanguageCompilerStatic
  with StreamStructNames {
  override def getTranslator(tp: TypeProvider): BaseTranslator = new PythonTranslator(tp)
  override def indent: String = "    "
  override def outFileName(topClassName: String): String = s"$topClassName.py"

  override def kstreamName: String = "KaitaiStream"
  override def kstructName: String = "KaitaiStruct"
}
