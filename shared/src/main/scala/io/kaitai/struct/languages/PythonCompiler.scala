package io.kaitai.struct.languages

import io.kaitai.struct.datatype.{DataType, FixedEndian, InheritedEndian}
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.PythonTranslator
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig, StringLanguageOutputWriter, Utils}

class PythonCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with ObjectOrientedLanguage
    with UpperCamelCaseClasses
    with SingleOutputFile
    with UniversalFooter
    with EveryReadIsExpression
    with AllocateIOLocalVar
    with FixedContentsUsingArrayByteLiteral
    with UniversalDoc
    with NoNeedForFullClassPath {

  import PythonCompiler._

  override val translator = new PythonTranslator(typeProvider, importList)

  override def innerDocstrings = true

  override def universalFooter: Unit = {
    out.dec
    out.puts
  }

  override def indent: String = "    "
  override def outFileName(topClassName: String): String = s"$topClassName.py"

  override def outImports(topClass: ClassSpec) =
    importList.toList.mkString("", "\n", "\n")

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"# $headerComment")
    outHeader.puts

    importList.add("from pkg_resources import parse_version")
    importList.add(s"from kaitaistruct import __version__ as ks_version, $kstructName, $kstreamName, BytesIO")

    out.puts
    out.puts

    // API compatibility check
    out.puts(
      "if parse_version(ks_version) < parse_version('" +
        KSVersion.minimalRuntime +
        "'):"
    )
    out.inc
    out.puts(
      "raise Exception(\"Incompatible Kaitai Struct Python API: " +
        KSVersion.minimalRuntime +
        " or later is required, but you have %s\" % (ks_version))"
    )
    out.dec
    out.puts
  }

  override def opaqueClassDeclaration(classSpec: ClassSpec): Unit = {
    val name = classSpec.name.head
    out.puts(
      if (config.pythonPackage.nonEmpty) {
        s"from ${config.pythonPackage} import $name"
      } else {
        s"import $name"
      }
    )
  }

  override def classHeader(name: String): Unit = {
    out.puts(s"class ${type2class(name)}($kstructName):")
    out.inc
  }

  override def classConstructorHeader(name: String, parentType: DataType, rootClassName: String, isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    val endianAdd = if (isHybrid) ", _is_le=None" else ""
    val paramsList = Utils.join(params.map((p) => paramName(p.id)), ", ", ", ", "")

    out.puts(s"def __init__(self$paramsList, _io, _parent=None, _root=None$endianAdd):")
    out.inc
    out.puts("self._io = _io")
    out.puts("self._parent = _parent")
    out.puts("self._root = _root if _root else self")

    if (isHybrid)
      out.puts("self._is_le = _is_le")

    // Store parameters passed to us
    params.foreach((p) => handleAssignmentSimple(p.id, paramName(p.id)))

    if (config.readStoresPos) {
      importList.add("import collections")
      out.puts("self._debug = collections.defaultdict(dict)")
    }
  }

  override def runRead(): Unit = {
    out.puts("self._read()")
  }

  override def runReadCalc(): Unit = {
    out.puts
    out.puts(s"if self._is_le == True:")
    out.inc
    out.puts("self._read_le()")
    out.dec
    out.puts("elif self._is_le == False:")
    out.inc
    out.puts("self._read_be()")
    out.dec
    out.puts("else:")
    out.inc
    //out.puts(s"raise $kstreamName.UndecidedEndiannessError")
    out.puts("raise Exception(\"Unable to decide endianness\")")
    out.dec
  }

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = {
    val suffix = endian match {
      case Some(e) => s"_${e.toSuffix}"
      case None => ""
    }
    out.puts(s"def _read$suffix(self):")
    out.inc
    if (isEmpty)
      out.puts("pass")
  }

  override def readFooter() = universalFooter

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {}

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {}

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

    val extraNewline = if (docStr.isEmpty || docStr.last == '\n') "" else "\n"
    val refStr = doc.ref.map {
      case TextRef(text) =>
        val seeAlso = new StringLanguageOutputWriter("")
        seeAlso.putsLines("   ", text)
        s"$extraNewline\n.. seealso::\n${seeAlso.result}"
      case ref: UrlRef =>
        val seeAlso = new StringLanguageOutputWriter("")
        seeAlso.putsLines("   ", s"${ref.text} - ${ref.url}")
        s"$extraNewline\n.. seealso::\n${seeAlso.result}"
    }.mkString("\n")

    out.putsLines("", "\"\"\"" + docStr + refStr + "\"\"\"")
  }

  override def attrFixedContentsParse(attrName: Identifier, contents: String): Unit =
    out.puts(s"${privateMemberName(attrName)} = self._io.ensure_fixed_contents($contents)")

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {
    out.puts("if self._is_le:")
    out.inc
    leProc()
    out.dec
    out.puts("else:")
    out.inc
    beProc()
    out.dec
  }

  // This function is to generate code for scan-end
  override def attrScanCustom(scanEnd: ScanExpr, varSrc: Identifier, varDest: Identifier): Unit = {
    val srcName = privateMemberName(varSrc)
    val destName = privateMemberName(varDest)

    scanEnd match {
      case ScanCustom(name, args) =>
        val scanClass = if (name.length == 1) {
          val onlyName = name.head
          val className = type2class(onlyName)
          importList.add(s"from $onlyName import $className")
          className
        } else {
          val pkgName = name.init.mkString(".")
          importList.add(s"import $pkgName")
          s"$pkgName.${type2class(name.last)}"
        }

        out.puts(s"pos1 = self._io.pos()")
        out.puts(s"_scanner = $scanClass(self._io, ${args.map(expression).mkString(", ")})")
        out.puts(s"_scanner.scan()")
        out.puts(s"pos2 = self._io.pos()")
        out.puts(s"self._io.seek(pos1)")
        out.puts(s"$destName = self._io.read_bytes(pos2 - pos1)")
    }
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
        importList.add("import zlib")
        out.puts(s"$destName = zlib.decompress($srcName)")
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        out.puts(s"$destName = $kstreamName.process_rotate_left($srcName, $expr, 1)")
      case ProcessCustom(name, args) =>
        val procClass = if (name.length == 1) {
          val onlyName = name.head
          val className = type2class(onlyName)
          importList.add(s"from $onlyName import $className")
          className
        } else {
          val pkgName = name.init.mkString(".")
          importList.add(s"import $pkgName")
          s"$pkgName.${type2class(name.last)}"
        }

        out.puts(s"_process = $procClass(${args.map(expression).mkString(", ")})")
        out.puts(s"$destName = _process.decode($srcName)")
    }
  }

  override def normalIO: String = "self._io"

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = {
    val varStr = privateMemberName(varName)
    val ioName = s"_io_${idToStr(varName)}"

    val args = rep match {
      case RepeatEos | RepeatUntil(_) => s"$varStr[-1]"
      case RepeatExpr(_) => s"$varStr[i]"
      case NoRepeat => varStr
    }

    out.puts(s"$ioName = $kstreamName(BytesIO($args))")
    ioName
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

  override def alignToByte(io: String): Unit =
    out.puts(s"$io.align_to_byte()")

  override def attrDebugStart(attrId: Identifier, attrType: DataType, ios: Option[String], rep: RepeatSpec): Unit = {
    ios.foreach { (io) =>
      val name = attrId match {
        case _: RawIdentifier | _: SpecialIdentifier => return
        case _ => idToStr(attrId)
      }
      rep match {
        case NoRepeat =>
          out.puts(s"self._debug['$name']['start'] = $io.pos()")
        case _: RepeatExpr | RepeatEos | _: RepeatUntil =>
          out.puts(s"if not 'arr' in self._debug['$name']:")
          out.inc
          out.puts(s"self._debug['$name']['arr'] = []")
          out.dec
          out.puts(s"self._debug['$name']['arr'].append({'start': $io.pos()})")
      }
    }
  }

  override def attrDebugEnd(attrId: Identifier, attrType: DataType, io: String, rep: RepeatSpec): Unit = {
    val name = attrId match {
      case _: RawIdentifier | _: SpecialIdentifier => return
      case _ => idToStr(attrId)
    }
    rep match {
      case NoRepeat =>
        out.puts(s"self._debug['$name']['end'] = $io.pos()")
      case _: RepeatExpr =>
        out.puts(s"self._debug['$name']['arr'][i]['end'] = $io.pos()")
      case RepeatEos | _: RepeatUntil =>
        out.puts(s"self._debug['$name']['arr'][len(${privateMemberName(attrId)}) - 1]['end'] = $io.pos()")
    }
  }

  override def condIfHeader(expr: Ast.expr): Unit = {
    out.puts(s"if ${expression(expr)}:")
    out.inc
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = []")
    out.puts(s"${privateMemberName(id)} = []")
    out.puts("i = 0")
    out.puts(s"while not $io.is_eof():")
    out.inc
  }
  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)}.append($expr)")
  override def condRepeatEosFooter: Unit = {
    out.puts("i += 1")
    universalFooter
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = [None] * (${expression(repeatExpr)})")
    out.puts(s"${privateMemberName(id)} = [None] * (${expression(repeatExpr)})")
    out.puts(s"for i in range(${expression(repeatExpr)}):")
    out.inc
  }
  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)}[i] = $expr")

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, untilExpr: expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = []")
    out.puts(s"${privateMemberName(id)} = []")
    out.puts("i = 0")
    out.puts("while True:")
    out.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val tmpName = translator.doName(if (isRaw) Identifier.ITERATOR2 else Identifier.ITERATOR)
    out.puts(s"$tmpName = $expr")
    out.puts(s"${privateMemberName(id)}.append($tmpName)")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, untilExpr: expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.puts(s"if ${expression(untilExpr)}:")
    out.inc
    out.puts("break")
    out.dec
    out.puts("i += 1")
    out.dec
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)} = $expr")

  override def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit =
    out.puts(s"$id = $expr")

  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    dataType match {
      case t: ReadableType =>
        s"$io.read_${t.apiCall(defEndian)}()"
      case blt: BytesLimitType =>
        s"$io.read_bytes(${expression(blt.size)})"
      case _: BytesEosType =>
        s"$io.read_bytes_full()"
      case BytesTerminatedType(terminator, include, consume, eosError, _, _) =>
        s"$io.read_bytes_term($terminator, ${bool2Py(include)}, ${bool2Py(consume)}, ${bool2Py(eosError)})"
      case BytesScanEndType(_, scanEnd) =>
        s"''"
      case BitsType1 =>
        s"$io.read_bits_int(1) != 0"
      case BitsType(width: Int) =>
        s"$io.read_bits_int($width)"
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
        s"${userType2class(t)}($addParams$io$addArgs)"
    }
  }

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean) = {
    val expr1 = padRight match {
      case Some(padByte) => s"$kstreamName.bytes_strip_right($expr0, $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) => s"$kstreamName.bytes_terminate($expr1, $term, ${bool2Py(include)})"
      case None => expr1
    }
    expr2
  }

  override def userTypeDebugRead(id: String): Unit =
    out.puts(s"$id._read()")

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

  override def instanceHeader(className: String, instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    out.puts("@property")
    out.puts(s"def ${publicMemberName(instName)}(self):")
    out.inc
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    out.puts(s"if hasattr(self, '${idToStr(instName)}'):")
    out.inc
    instanceReturn(instName, dataType)
    out.dec
    out.puts
  }

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit = {
    // not very efficient, probably should be some other way to do that, but for now it will do:
    // workaround to avoid Python generating an "AttributeError: instance has no attribute"
    out.puts(s"return ${privateMemberName(instName)} if hasattr(self, '${idToStr(instName)}') else None")
  }

  override def enumDeclaration(curClass: String, enumName: String, enumColl: Seq[(Long, String)]): Unit = {
    importList.add("from enum import Enum")

    out.puts
    out.puts(s"class ${type2class(enumName)}(Enum):")
    out.inc
    enumColl.foreach { case (id: Long, label: String) => out.puts(s"$label = $id") }
    out.dec
  }

  override def debugClassSequence(seq: List[AttrSpec]) = {
    val seqStr = seq.map((attr) => "\"" + idToStr(attr.id) + "\"").mkString(", ")
    out.puts(s"SEQ_FIELDS = [$seqStr]")
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

  override def localTemporaryName(id: Identifier): String = s"_t_${idToStr(id)}"

  def userType2class(t: UserType): String = {
    val name = t.classSpec.get.name
    val firstName = name.head
    val prefix = if (t.isOpaque && firstName != translator.provider.nowClass.name.head) {
      s"$firstName."
    } else {
      ""
    }
    s"$prefix${types2class(name)}"
  }
}

object PythonCompiler extends LanguageCompilerStatic
  with UpperCamelCaseClasses
  with StreamStructNames {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new PythonCompiler(tp, config)

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
