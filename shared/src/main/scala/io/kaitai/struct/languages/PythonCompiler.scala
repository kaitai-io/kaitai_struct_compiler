package io.kaitai.struct.languages

import io.kaitai.struct.{LanguageOutputWriter, RuntimeConfig}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.{BaseTranslator, PythonTranslator, TypeProvider}

class PythonCompiler(config: RuntimeConfig, out: LanguageOutputWriter)
  extends LanguageCompiler(config, out)
    with ObjectOrientedLanguage
    with UniversalFooter
    with EveryReadIsExpression
    with AllocateIOLocalVar
    with FixedContentsUsingArrayByteLiteral
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
    out.puts("import struct")
    out.puts("import zlib")
    out.puts("from enum import Enum")
    out.puts
    out.puts(s"from kaitaistruct import $kstructName, $kstreamName, BytesIO")
    out.puts
    out.puts
  }

  override def opaqueClassDeclaration(classSpec: ClassSpec): Unit = {
    val name = classSpec.name.head
    out.puts(s"from $name import ${type2class(name)}")
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

  override def attributeReader(attrName: Identifier, attrType: BaseType, condSpec: ConditionalSpec): Unit = {}

  override def attrFixedContentsParse(attrName: Identifier, contents: String): Unit =
    out.puts(s"${privateMemberName(attrName)} = self._io.ensure_fixed_contents($contents)")

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
      case RepeatEos | RepeatUntil(_) => s"$varStr[-1]"
      case RepeatExpr(_) => s"$varStr[i]"
      case NoRepeat => varStr
    }

    out.puts(s"io = $kstreamName(BytesIO($args))")
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
      out.puts(s"${privateMemberName(RawIdentifier(id))} = [None] * (${expression(repeatExpr)})")
    out.puts(s"${privateMemberName(id)} = [None] * (${expression(repeatExpr)})")
    out.puts(s"for i in range(${expression(repeatExpr)}):")
    out.inc
  }
  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)}[i] = $expr")

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean, untilExpr: expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = []")
    out.puts(s"${privateMemberName(id)} = []")
    out.puts("while True:")
    out.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String): Unit = {
    out.puts(s"${translator.doName("_")} = $expr")
    out.puts(s"${privateMemberName(id)}.append(${translator.doName("_")})")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: BaseType, needRaw: Boolean, untilExpr: expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.puts(s"if ${expression(untilExpr)}:")
    out.inc
    out.puts("break")
    out.dec
    out.dec
  }

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
      case BytesLimitType(size, _) =>
        s"$io.read_bytes(${expression(size)})"
      case BytesEosType(_) =>
        s"$io.read_bytes_full()"
      case BitsType(1) =>
        s"$io.read_bits_int(1) != 0"
      case BitsType(width: Int) =>
        s"$io.read_bits_int($width)"
      case t: UserType =>
        val addArgs = if (t.isOpaque) "" else ", self, self._root"
        s"${types2class(t.classSpec.get.name)}($io$addArgs)"
    }
  }

  override def switchStart(id: Identifier, on: Ast.expr): Unit = {
    out.puts(s"_on = ${expression(on)}")
  }

  override def switchCaseFirstStart(condition: Ast.expr): Unit = {
    out.puts(s"if _on == ${expression(condition)}:")
    out.inc
  }

  override def switchCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"elif _on == ${expression(condition)}:")
    out.inc
  }

  override def switchCaseEnd(): Unit =
    out.dec

  override def switchElseStart(): Unit = {
    out.puts(s"else:")
    out.inc
  }

  override def switchEnd(): Unit = {}

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
    // not very efficient, probably should be some other way to do that, but for now it will do:
    // workaround to avoid Python generating an "AttributeError: instance has no attribute"
    out.puts(s"return ${privateMemberName(instName)} if hasattr(self, '${idToStr(instName)}') else None")
  }

  override def enumDeclaration(curClass: String, enumName: String, enumColl: Seq[(Long, String)]): Unit = {
    out.puts
    out.puts(s"class ${type2class(enumName)}(Enum):")
    out.inc
    enumColl.foreach { case (id: Long, label: String) => out.puts(s"$label = $id") }
    out.dec
  }

  def bool2Py(b: Boolean): String = if (b) { "True" } else { "False" }

  def idToStr(id: Identifier): String = {
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => name
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => s"_m_$name"
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
  with UpperCamelCaseClasses
  with StreamStructNames {
  override def getTranslator(tp: TypeProvider): BaseTranslator = new PythonTranslator(tp)
  override def indent: String = "    "
  override def outFileName(topClassName: String): String = s"$topClassName.py"

  override def kstreamName: String = "KaitaiStream"
  override def kstructName: String = "KaitaiStruct"

  def types2class(name: List[String]): String = {
    if (name.size > 1) {
      val path = name.drop(1).map(x => type2class(x)).mkString(".")
      s"self._root.$path"
    } else {
      type2class(name.head)
    }
  }
}
