package io.kaitai.struct.languages

import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig, Utils}
import io.kaitai.struct.datatype.{DataType, FixedEndian, InheritedEndian}
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.LuaTranslator

class LuaCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with AllocateIOLocalVar
    with EveryReadIsExpression
    with FixedContentsUsingArrayByteLiteral
    with ObjectOrientedLanguage
    with SingleOutputFile
    with UniversalDoc
    with UniversalFooter
    with UpperCamelCaseClasses {

  import LuaCompiler._

  override val translator = new LuaTranslator(typeProvider, importList)

  override def innerClasses = false
  override def innerEnums = true

  override def indent: String = "  "
  override def outFileName(topClassName: String): String = s"$topClassName.lua"
  override def outImports(topClass: ClassSpec) =
    importList.toList.mkString("", "\n", "\n")

  override def opaqueClassDeclaration(classSpec: ClassSpec): Unit =
    out.puts("require(\"" + classSpec.name.head + "\")")

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"-- $headerComment")
    outHeader.puts("--")
    outHeader.puts("-- This file is compatible with Lua 5.3")
    outHeader.puts

    importList.add("local class = require(\"class\")")
    importList.add("require(\"kaitaistruct\")")

    out.puts
  }

  override def universalFooter: Unit =
    out.puts

  override def universalDoc(doc: DocSpec): Unit = {
    val docStr = doc.summary match {
      case Some(summary) =>
        val lastChar = summary.last
        if (lastChar == '.' || lastChar == '\n') {
          summary
        } else {
          summary + "."
        }
      case None =>
        ""
    }
    val extraNewLine = if (docStr.isEmpty || docStr.last == '\n') "" else "\n"
    val refStr = doc.ref.map {
      case TextRef(text) =>
        s"See also: $text"
      case UrlRef(url, text) =>
        s"See also: $text ($url)"
    }.mkString("\n")

    out.putsLines("-- ", "\n" + docStr + extraNewLine + refStr)
  }

  override def classHeader(name: List[String]): Unit = {
    out.puts(s"${types2class(name)} = class.class($kstructName)")
    out.puts
  }
  override def classFooter(name: List[String]): Unit =
    universalFooter
  override def classConstructorHeader(name: List[String], parentType: DataType, rootClassName: List[String], isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    val endianAdd = if (isHybrid) ", is_le" else ""
    val paramsList = Utils.join(params.map((p) => paramName(p.id)), "", ", ", ", ")

    out.puts(s"function ${types2class(name)}:_init($paramsList" + s"io, parent, root$endianAdd)")
    out.inc
    out.puts(s"$kstructName._init(self, io)")
    out.puts("self._parent = parent")
    out.puts("self._root = root or self")
    if (isHybrid)
      out.puts("self._is_le = is_le")

    // Store parameters passed to us
    params.foreach((p) => handleAssignmentSimple(p.id, paramName(p.id)))
  }
  override def classConstructorFooter: Unit = {
    out.dec
    out.puts("end")
    out.puts
  }

  override def runRead(): Unit =
    out.puts("self:_read()")
  override def runReadCalc(): Unit = {
    out.puts
    out.puts(s"if self._is_le then")
    out.inc
    out.puts("self:_read_le()")
    out.dec
    out.puts(s"elseif not self._is_le then")
    out.inc
    out.puts("self:_read_be()")
    out.dec
    out.puts("else")
    out.inc
    out.puts("error(\"unable to decide endianness\")")
    out.dec
    out.puts("end")
  }
  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = {
    val suffix = endian match {
      case Some(e) => s"_${e.toSuffix}"
      case None => ""
    }

    out.puts(s"function ${types2class(typeProvider.nowClass.name)}:_read$suffix()")
    out.inc
  }
  override def readFooter(): Unit = {
    out.dec
    out.puts("end")
    out.puts
  }

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit =
    {}
  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit =
    {}

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {
    out.puts("if self._is_le then")
    out.inc
    leProc()
    out.dec
    out.puts("else")
    out.inc
    beProc()
    out.dec
    out.puts("end")
  }

  override def attrFixedContentsParse(attrName: Identifier, contents: String): Unit =
    out.puts(s"${privateMemberName(attrName)} = self._io:ensure_fixed_contents($contents)")

  override def condIfHeader(expr: Ast.expr): Unit = {
    out.puts(s"if ${expression(expr)} then")
    out.inc
  }
  override def condIfFooter(expr: Ast.expr): Unit = {
    out.dec
    out.puts("end")
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = {}")
    out.puts(s"${privateMemberName(id)} = {}")
    out.puts("local i = 1")
    out.puts(s"while not $io:is_eof() do")
    out.inc
  }
  override def condRepeatEosFooter: Unit = {
    out.puts("i = i + 1")
    out.dec
    out.puts("end")
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: Ast.expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = {}")
    out.puts(s"${privateMemberName(id)} = {}")
    out.puts(s"for i = 1, ${expression(repeatExpr)} do")
    out.inc
  }
  override def condRepeatExprFooter: Unit = {
    out.dec
    out.puts("end")
  }

  override def condRepeatUntilHeader(id: Identifier, io: String, datatype: DataType, needRaw: Boolean, repeatExpr: Ast.expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = {}")
    out.puts(s"${privateMemberName(id)} = {}")
    out.puts("local i = 1")
    out.puts("while true do")
    out.inc
  }
  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, untilExpr: Ast.expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.puts(s"if ${expression(untilExpr)} then")
    out.inc
    out.puts("break")
    out.dec
    out.puts("end")
    out.puts("i = i + 1")
    out.dec
    out.puts("end")
    out.dec
  }

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit = {
    val srcName = privateMemberName(varSrc)
    val destName = privateMemberName(varDest)

    proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "process_xor_one"
          case _: BytesType => "process_xor_many"
        }
        out.puts(s"$destName = $kstreamName.$procName($srcName, ${expression(xorValue)})")
      case ProcessZlib =>
        throw new RuntimeException("Lua zlib not supported")
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        out.puts(s"$destName = $kstreamName.process_rotate_left($srcName, $expr, 1)")
      case ProcessCustom(name, args) =>
        val procName = s"_process_${idToStr(varSrc)}"

        importList.add("require(\"" + s"${name.last}" + "\")")

        out.puts(s"local $procName = ${types2class(name)}(${args.map(expression).mkString(", ")})")
        out.puts(s"$destName = $procName:decode($srcName)")
    }
  }

  override def useIO(ioEx: Ast.expr): String = {
    out.puts(s"local _io = ${expression(ioEx)}")
    "_io"
  }
  override def pushPos(io:String): Unit =
    out.puts(s"local _pos = $io:pos()")
  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io:seek(${expression(pos)})")
  override def popPos(io: String): Unit =
    out.puts(s"$io:seek(_pos)")
  override def alignToByte(io: String): Unit =
    out.puts(s"$io:align_to_byte()")

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"${types2class(className)}.property.${publicMemberName(instName)} = {}")
    out.puts(s"function ${types2class(className)}.property.${publicMemberName(instName)}:get()")
    out.inc
  }
  override def instanceFooter: Unit = {
    out.dec
    out.puts("end")
    out.puts
  }
  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    out.puts(s"if self.${idToStr(instName)} ~= nil then")
    out.inc
    instanceReturn(instName, dataType)
    out.dec
    out.puts("end")
    out.puts
  }
  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit =
    out.puts(s"return ${privateMemberName(instName)}")

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, EnumValueSpec)]): Unit = {
    importList.add("local enum = require(\"enum\")")

    out.puts(s"${types2class(curClass)}.${type2class(enumName)} = enum.Enum {")
    out.inc
    enumColl.foreach { case (id, label) => out.puts(s"${label.name} = $id,") }
    out.dec
    out.puts("}")
    out.puts
  }

  override def idToStr(id: Identifier): String = id match {
    case SpecialIdentifier(name) => name
    case NamedIdentifier(name) => name
    case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
    case InstanceIdentifier(name) => s"_m_$name"
    case RawIdentifier(innerId) => s"_raw_${idToStr(innerId)}"
  }
  override def privateMemberName(id: Identifier): String =
    s"self.${idToStr(id)}"
  override def publicMemberName(id: Identifier): String = id match {
    case SpecialIdentifier(name) => name
    case NamedIdentifier(name) => name
    case InstanceIdentifier(name) => name
    case RawIdentifier(innerId) => s"_raw_${publicMemberName(innerId)}"
  }
  override def localTemporaryName(id: Identifier): String =
    s"_t_${idToStr(id)}"

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)}[i] = $expr")
  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)}[i] = $expr")
  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val tmpName = translator.doName(if (isRaw) Identifier.ITERATOR2 else Identifier.ITERATOR)
    out.puts(s"$tmpName = $expr")
    out.puts(s"${privateMemberName(id)}[i] = $tmpName")
  }
  override def handleAssignmentSimple(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)} = $expr")

  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = dataType match {
    case t: ReadableType =>
      s"$io:read_${t.apiCall(defEndian)}()"
    case blt: BytesLimitType =>
      s"$io:read_bytes(${expression(blt.size)})"
    case _: BytesEosType =>
      s"$io:read_bytes_full()"
    case BytesTerminatedType(terminator, include, consume, eosError, _) =>
      s"$io:read_bytes_term($terminator, $include, $consume, $eosError)"
    case BitsType1 =>
      s"$io:read_bits_int(1)"
    case BitsType(width: Int) =>
      s"$io:read_bits_int($width)"
    case t: UserType =>
      val addParams = Utils.join(t.args.map((a) => translator.translate(a)), "", ", ", ", ")
      val addArgs = if (t.isOpaque) {
        ""
      } else {
        val parent = t.forcedParent match {
          case Some(fp) => translator.translate(fp)
          case None => "self"
        }
        val addEndian = t.classSpec.get.meta.endian match {
          case Some(InheritedEndian) => ", self._is_le"
          case _ => ""
        }
        s", $parent, self._root$addEndian"
      }
      s"${types2class(t.classSpec.get.name)}($addParams$io$addArgs)"
  }
  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean): String = {
    val expr1 = padRight match {
      case Some(padByte) => s"$kstreamName.bytes_strip_right($expr0, $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) => s"$kstreamName.bytes_terminate($expr1, $term, $include)"
      case None => expr1
    }
    expr2
  }

  override def userTypeDebugRead(id: String): Unit =
    out.puts(s"$id:_read()")

  override def switchStart(id: Identifier, on: Ast.expr): Unit =
    out.puts(s"local _on = ${expression(on)}")
  override def switchCaseFirstStart(condition: Ast.expr): Unit = {
    out.puts(s"if _on == ${expression(condition)} then")
    out.inc
  }
  override def switchCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"elseif _on == ${expression(condition)} then")
    out.inc
  }
  override def switchCaseEnd(): Unit =
    out.dec
  override def switchElseStart(): Unit = {
    out.puts("else")
    out.inc
  }
  override def switchEnd(): Unit =
    out.puts("end")

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = {
    val varStr = privateMemberName(varName)

    val args = rep match {
      case RepeatEos | RepeatUntil(_) => s"$varStr[#$varStr]"
      case RepeatExpr(_) => s"$varStr[i]"
      case NoRepeat => varStr
    }

    importList.add("local stringstream = require(\"string_stream\")")
    out.puts(s"local _io = $kstreamName(stringstream($args))")
    "_io"
  }
}

object LuaCompiler extends LanguageCompilerStatic
    with UpperCamelCaseClasses
    with StreamStructNames {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new LuaCompiler(tp, config)

  override def kstructName: String = "KaitaiStruct"
  override def kstreamName: String = "KaitaiStream"

  def types2class(name: List[String]): String =
    name.map(x => type2class(x)).mkString(".")
}
