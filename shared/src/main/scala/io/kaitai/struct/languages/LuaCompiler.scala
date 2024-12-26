package io.kaitai.struct.languages

import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig, Utils, ExternalType}
import io.kaitai.struct.datatype.{DataType, FixedEndian, InheritedEndian, KSError, ValidationNotEqualError, ValidationNotInEnumError}
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
    with SwitchIfOps
    with UpperCamelCaseClasses {

  import LuaCompiler._

  override val translator = new LuaTranslator(typeProvider, importList)

  override def innerClasses = false
  override def innerEnums = true

  override def indent: String = "  "
  override def outFileName(topClassName: String): String = s"$topClassName.lua"
  override def outImports(topClass: ClassSpec) =
    importList.toList.mkString("", "\n", "\n")

  override def externalTypeDeclaration(extType: ExternalType): Unit =
    importList.add("require(\"" + extType.name.head + "\")")

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
    if (name == rootClassName) {
      out.puts("self._root = root or self")
    } else {
      out.puts("self._root = root")
    }
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

  override def runRead(name: List[String]): Unit =
    out.puts("self:_read()")
  override def runReadCalc(): Unit = {
    out.puts
    out.puts("if self._is_le == true then")
    out.inc
    out.puts("self:_read_le()")
    out.dec
    out.puts("elseif self._is_le == false then")
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

  override def condRepeatInitAttr(id: Identifier, dataType: DataType): Unit = {
    out.puts(s"${privateMemberName(id)} = {}")
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType): Unit = {
    out.puts("local i = 0")
    out.puts(s"while not $io:is_eof() do")
    out.inc
  }
  override def condRepeatEosFooter: Unit = {
    out.puts("i = i + 1")
    out.dec
    out.puts("end")
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, repeatExpr: Ast.expr): Unit = {
    out.puts(s"for i = 0, ${expression(repeatExpr)} - 1 do")
    out.inc
  }
  override def condRepeatExprFooter: Unit = {
    out.dec
    out.puts("end")
  }

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, untilExpr: Ast.expr): Unit = {
    out.puts("local i = 0")
    out.puts("while true do")
    out.inc
  }
  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, untilExpr: Ast.expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.puts(s"if ${expression(untilExpr)} then")
    out.inc
    out.puts("break")
    out.dec
    out.puts("end")
    out.puts("i = i + 1")
    out.dec
    out.puts("end")
  }

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec): Unit = {
    val srcExpr = getRawIdExpr(varSrc, rep)

    val expr = proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "process_xor_one"
          case _: BytesType => "process_xor_many"
        }
        s"$kstreamName.$procName($srcExpr, ${expression(xorValue)})"
      case ProcessZlib =>
        s"$kstreamName.process_zlib($srcExpr)"
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        s"$kstreamName.process_rotate_left($srcExpr, $expr, 1)"
      case ProcessCustom(name, args) =>
        val procName = s"_process_${idToStr(varSrc)}"

        importList.add("require(\"" + s"${name.last}" + "\")")

        out.puts(s"local $procName = ${types2class(name)}(${args.map(expression).mkString(", ")})")
        s"$procName:decode($srcExpr)"
    }
    handleAssignment(varDest, expr, rep, false)
  }

  def getRawIdExpr(varName: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(varName)
    rep match {
      case NoRepeat => memberName
      case RepeatExpr(_) => s"$memberName[i + 1]"
      case _ => s"$memberName[#$memberName]"
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
    enumColl.foreach { case (id, label) => out.puts(s"${label.name} = ${translator.doIntLiteral(id)},") }
    out.dec
    out.puts("}")
    out.puts
  }

  override def idToStr(id: Identifier): String = LuaCompiler.idToStr(id)

  override def publicMemberName(id: Identifier): String =
    id match {
      case InstanceIdentifier(name) => name
      case _ => idToStr(id)
    }

  override def privateMemberName(id: Identifier): String = LuaCompiler.privateMemberName(id)

  override def localTemporaryName(id: Identifier): String =
    s"_t_${idToStr(id)}"

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)}[i + 1] = $expr")
  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)}[i + 1] = $expr")
  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val tmpName = translator.doName(if (isRaw) Identifier.ITERATOR2 else Identifier.ITERATOR)
    out.puts(s"local $tmpName = $expr")
    out.puts(s"${privateMemberName(id)}[i + 1] = $tmpName")
  }
  override def handleAssignmentSimple(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)} = $expr")

  override def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit =
    out.puts(s"local $id = $expr")

  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = dataType match {
    case t: ReadableType =>
      s"$io:read_${t.apiCall(defEndian)}()"
    case blt: BytesLimitType =>
      s"$io:read_bytes(${expression(blt.size)})"
    case _: BytesEosType =>
      s"$io:read_bytes_full()"
    case BytesTerminatedType(terminator, include, consume, eosError, _) =>
      if (terminator.length == 1) {
        val term = terminator.head & 0xff
        s"$io:read_bytes_term($term, $include, $consume, $eosError)"
      } else {
        s"$io:read_bytes_term_multi(${translator.doByteArrayLiteral(terminator)}, $include, $consume, $eosError)"
      }
    case BitsType1(bitEndian) =>
      s"$io:read_bits_int_${bitEndian.toSuffix}(1) ~= 0"
    case BitsType(width: Int, bitEndian) =>
      s"$io:read_bits_int_${bitEndian.toSuffix}($width)"
    case t: UserType =>
      val addParams = Utils.join(t.args.map((a) => translator.translate(a)), "", ", ", ", ")
      val addArgs = if (t.isExternal(typeProvider.nowClass)) {
        ""
      } else {
        val parent = t.forcedParent match {
          case Some(USER_TYPE_NO_PARENT) => "nil"
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
  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Seq[Byte]], include: Boolean): String = {
    val expr1 = padRight match {
      case Some(padByte) => s"$kstreamName.bytes_strip_right($expr0, $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) =>
        if (term.length == 1) {
          val t = term.head & 0xff
          s"$kstreamName.bytes_terminate($expr1, $t, $include)"
        } else {
          s"$kstreamName.bytes_terminate_multi($expr1, ${translator.doByteArrayLiteral(term)}, $include)"
        }
      case None => expr1
    }
    expr2
  }

  override def userTypeDebugRead(id: String, dataType: DataType, assignType: DataType): Unit =
    out.puts(s"$id:_read()")

  override def tryFinally(tryBlock: () => Unit, finallyBlock: () => Unit): Unit = {
    out.puts("local success, err = pcall(function()")
    out.inc
    tryBlock()
    out.dec
    out.puts("end)")
    finallyBlock()
    out.puts("if not success then")
    out.inc
    out.puts("error(err)")
    out.dec
    out.puts("end")
  }

  override def switchStart(id: Identifier, on: Ast.expr): Unit = {}
  override def switchCaseStart(condition: Ast.expr): Unit = {}
  override def switchCaseEnd(): Unit = {}
  override def switchElseStart(): Unit = {}
  override def switchEnd(): Unit = {}

  override def switchRequiresIfs(onType: DataType): Boolean = true

  override def switchIfStart(id: Identifier, on: Ast.expr, onType: DataType): Unit =
    out.puts(s"local _on = ${expression(on)}")

  override def switchIfCaseFirstStart(condition: Ast.expr): Unit = {
    out.puts(s"if _on == ${expression(condition)} then")
    out.inc
  }

  override def switchIfCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"elseif _on == ${expression(condition)} then")
    out.inc
  }

  override def switchIfCaseEnd(): Unit =
    out.dec

  override def switchIfElseStart(): Unit = {
    out.puts("else")
    out.inc
  }

  override def switchIfEnd(): Unit =
    out.puts("end")

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = {
    val varStr = privateMemberName(varName)

    val args = getRawIdExpr(varName, rep)

    importList.add("local stringstream = require(\"string_stream\")")
    out.puts(s"local _io = $kstreamName(stringstream($args))")
    "_io"
  }

  override def ksErrorName(err: KSError): String = LuaCompiler.ksErrorName(err)

  override def attrValidateExpr(
    attr: AttrLikeSpec,
    checkExpr: Ast.expr,
    err: KSError,
    errArgs: List[Ast.expr]
  ): Unit =
    attrValidate(s"not(${translator.translate(checkExpr)})", err, errArgs)

  override def attrValidateInEnum(
    attr: AttrLikeSpec,
    et: EnumType,
    valueExpr: Ast.expr,
    err: ValidationNotInEnumError,
    errArgs: List[Ast.expr]
  ): Unit = {
    // NOTE: this condition works for now because we haven't implemented
    // https://github.com/kaitai-io/kaitai_struct/issues/778 for Lua yet, but
    // it will need to be changed when we do.
    attrValidate(s"${translator.translate(valueExpr)} == nil", err, errArgs)
  }

  private def attrValidate(failCondExpr: String, err: KSError, errArgs: List[Ast.expr]): Unit = {
    val errArgsCode = errArgs.map(translator.translate)
    out.puts(s"if $failCondExpr then")
    out.inc
    val msg = err match {
      case _: ValidationNotEqualError => {
        val (expected, actual) = (
          errArgsCode.lift(0).getOrElse("[expected]"),
          errArgsCode.lift(1).getOrElse("[actual]")
        )
        s""""not equal, expected " ..  $expected .. ", but got " .. $actual"""
      }
      case _ => "\"" + ksErrorName(err) + "\""
    }
    out.puts(s"error($msg)")
    out.dec
    out.puts("end")
  }

  override def classToString(toStringExpr: Ast.expr): Unit = {
    out.puts(s"function ${types2class(typeProvider.nowClass.name)}:__tostring()")
    out.inc
    out.puts(s"return ${translator.translate(toStringExpr)}")
    out.dec
    out.puts("end")
  }
}

object LuaCompiler extends LanguageCompilerStatic
    with UpperCamelCaseClasses
    with StreamStructNames
    with ExceptionNames {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new LuaCompiler(tp, config)

  def idToStr(id: Identifier): String =
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => name
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => s"_m_$name"
      case RawIdentifier(innerId) => s"_raw_${idToStr(innerId)}"
    }

  def privateMemberName(id: Identifier): String =
    s"self.${idToStr(id)}"

  override def kstructName: String = "KaitaiStruct"
  override def kstreamName: String = "KaitaiStream"
  override def ksErrorName(err: KSError): String = err.name

  def types2class(name: List[String]): String =
    name.map(x => type2class(x)).mkString(".")
}
