package io.kaitai.struct.languages

import io.kaitai.struct.datatype.DataType.{BooleanType, _}
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.JuliaTranslator
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig, StringLanguageOutputWriter, Utils}

class JuliaCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with ObjectOrientedLanguage
    with UpperCamelCaseClasses
    with SingleOutputFile
    with UniversalFooter
    with EveryReadIsExpression
    with AllocateIOLocalVar
    with FixedContentsUsingArrayByteLiteral
    with UniversalDoc
    with SwitchIfOps {

  import JuliaCompiler._

  override val translator = new JuliaTranslator(typeProvider, importList)
  private val enums = new StringLanguageOutputWriter(indent)

  override def innerDocstrings = true

  override def innerClasses: Boolean = false

  override def universalFooter: Unit = {
    out.dec
    out.puts("end")
    out.puts
  }

  override def results(topClass: ClassSpec): Map[String, String] = {
    Map(outFileName(topClass.nameAsStr) ->
      (outHeader.result + outImports(topClass) + enums.result + out.result)
    )
  }

  // override def classForwardDeclaration(name: List[String]): Unit = {
  //   abstractTypesAndEnums.puts(s"abstract type ${types2class("Abstract" :: name)} end")
  // }

  override def indent: String = "    "
  override def outFileName(topClassName: String): String = s"${type2class(topClassName)}Module.jl"

  override def outImports(topClass: ClassSpec): String =
    importList.toList.mkString("", "\n", "\n")

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"module ${type2class(topClassName)}Module")
    outHeader.puts(s"# $headerComment")
    outHeader.puts
    importList.add("export from_file")
    importList.add("using KaitaiStruct")
    out.puts
  }

  override def fileFooter(topClassName: String): Unit = {
    fromFile(topClassName)
    out.puts("end")
  }

  override def externalClassDeclaration(classSpec: ClassSpec): Unit = {
    // val name = classSpec.name.head
    // importList.add(s"include(${'"'}../../compiled/julia/${classSpec.name.head}.jl${'"'})")
    // importList.add(s"using .${types2class(classSpec.name)}Module: ${types2class(classSpec.name)}")
    importList.add(s"import ${type2class(classSpec.name.head)}Module: ${types2class(classSpec.name)}")
  }

  override def classHeader(name: List[String]): Unit = {
    // val subtype = if (typeProvider.nowClass.isTopLevel) "" else s" <: ${types2class("Abstract" :: name)}"
    // out.puts(s"mutable struct ${types2class(name)}$subtype")
    out.puts(s"mutable struct ${types2class(name)} <: KaitaiStruct.UserType")
    out.inc
    typeProvider.nowClass.meta.endian match {
      case Some(_: CalcEndian) | Some(InheritedEndian) =>
        out.puts(s"_is_le::Union{Bool,Nothing}")
      case _ =>
      // no _is_le variable
    }
    if (config.readStoresPos) {
      out.puts("_attrStart::Dict")
      out.puts("_attrEnd::Dict")
      out.puts("_arrStart::Dict")
      out.puts("_arrEnd::Dict")
    }
  }

  override def classFooter(name: List[String]): Unit = {
    universalFooter
  }

  override def classConstructorHeader(name: List[String], parentType: DataType, rootClassName: List[String], isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    val endianAdd = if (isHybrid) ", _is_le=nothing" else ""
    val paramsList = Utils.join(params.map(p => paramName(p.id)), "", ", ", ",")

    out.puts(s"function ${types2class(name)}(${paramsList}_io, _parent=nothing, _root=nothing$endianAdd)")
    out.inc
    out.puts("this = new()")
    if (isHybrid)
      out.puts("this._is_le = _is_le")

    // Store parameters passed to us
    params.foreach(p => handleAssignmentSimple(p.id, paramName(p.id)))

    out.puts("this._io = _io")
    out.puts("this._parent = _parent")
    if (name == rootClassName) {
      out.puts("this._root = _root === nothing ? this : _root")
    }
    else {
      out.puts("this._root = _root")
    }
    if (config.readStoresPos) {
      out.puts("this._attrStart = Dict()")
      out.puts("this._attrEnd = Dict()")
      out.puts("this._arrStart = Dict()")
      out.puts("this._arrEnd = Dict()")
    }
  }

  override def attrInit(attr: AttrLikeSpec): Unit = {
    if (attr.isNullable)
      out.puts(s"this.${idToStr(attr.id)} = nothing")
  }

  override def classConstructorFooter: Unit = {
    out.puts("this")
    universalFooter
  }

  override def runRead(name: List[String]): Unit = {
    typeProvider.nowClass.instances.keys.foreach(instanceIdentifier => out.puts(s"this.${idToStr(instanceIdentifier)} = nothing"))
    out.puts("_read(this)")
  }

  override def runReadCalc(): Unit = {
    out.puts(s"if this._is_le == true")
    out.inc
    out.puts("_read_le(this)")
    out.dec
    out.puts("elseif this._is_le == false")
    out.inc
    out.puts("_read_be(this)")
    out.dec
    out.puts(s"else")
    out.inc
    out.puts(s"throw(${ksErrorName(UndecidedEndiannessError)}(" + "\"" + typeProvider.nowClass.path.mkString("/", "/", "") + "\"))")
    universalFooter
  }

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = {
    val suffix = endian match {
      case Some(e) => s"_${e.toSuffix}"
      case None => ""
    }
    out.puts(s"function _read$suffix(this::${types2class(typeProvider.nowClass.name)})")
    out.inc
  }

  override def readFooter(): Unit = {
    out.puts("nothing")
    universalFooter
  }

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    if (isNullable) {
      out.puts(s"${idToStr(attrName)}::Union{Nothing,${kaitaiType2NativeType(attrType)}}")
    } else {
      out.puts(s"${idToStr(attrName)}::${kaitaiType2NativeType(attrType)}")
    }
  }

  override def instanceDeclaration(attrName: InstanceIdentifier, attrType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"${idToStr(attrName)}::Union{Nothing,${kaitaiType2NativeType(attrType)}}")
  }



  def fromFile(name: String): Unit = {
    // if (typeProvider.nowClass.params.isEmpty) {
    out.puts(s"function from_file(filename::String)::${type2class(name)}")
    out.inc
    out.puts(s"${type2class(name)}($kstreamName(open(filename, ${'"'}r${'"'})))")
    universalFooter
    // }
  }

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
        s"$extraNewline\nSee also ${seeAlso.result}"
      case ref: UrlRef =>
        val seeAlso = new StringLanguageOutputWriter("")
        seeAlso.putsLines("   ", s"${ref.text} - ${ref.url}")
        s"$extraNewline\nSee also ${seeAlso.result}"
    }.mkString("\n")

    out.putsLines("", "\"\"\"" + docStr + refStr + "\"\"\"")
  }

  override def attrFixedContentsParse(attrName: Identifier, contents: String): Unit =
    out.puts(s"#ensure_fixed_content")

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {
    out.puts("if this._is_le")
    out.inc
    leProc()
    out.dec
    out.puts("else")
    out.inc
    beProc()
    blockScopeFooter
  }

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec): Unit = {
    val srcExpr = getRawIdExpr(varSrc, rep)

    val expr = proc match {
      case ProcessXor(xorValue) =>
        val xorValueStr = translator.detectType(xorValue) match {
          case _: IntType => s"UInt8(${translator.doCast(xorValue, Int1Type(true))})"
          case _ => expression(xorValue)
        }
        s"KaitaiStruct.process_xor($srcExpr, $xorValueStr)"
      case ProcessZlib =>
        importList.add("using CodecZlib")
        s"transcode(GzipDecompressor, $srcExpr)"
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        s"KaitaiStruct.process_rotate_left($srcExpr, $expr, 1)"
      case ProcessCustom(name, args) =>
        val procClass = type2class(name.last)
        importList.add(s"using ${name.map(x => type2class(x)).mkString(".")}")
        s"$procClass.decode(${(args.map(expression) :+ srcExpr).mkString(", ")})"
    }
    handleAssignment(varDest, expr, rep, isRaw = false)
  }

  override def normalIO: String = "this._io"

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = {
    val ioName = s"_io_${idToStr(varName)}"

    val args = getRawIdExpr(varName, rep)

    out.puts(s"$ioName = $kstreamName(IOBuffer($args))")
    ioName
  }

  def getRawIdExpr(varName: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(varName)
    rep match {
      case NoRepeat => memberName
      case RepeatExpr(_) => s"$memberName[i]"
      case _ => s"$memberName[end]"
    }
  }

  override def useIO(ioEx: expr): String = {
    out.puts(s"io = ${expression(ioEx)}")
    "io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"_pos = KaitaiStruct.pos($io)")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"KaitaiStruct.seek($io, ${expression(pos)})")

  override def popPos(io: String): Unit =
    out.puts(s"KaitaiStruct.seek($io, _pos)")

  override def alignToByte(io: String): Unit =
    out.puts(s"KaitaiStruct.align_to_byte($io)")

  override def attrDebugStart(attrId: Identifier, attrType: DataType, ios: Option[String], rep: RepeatSpec): Unit = {
    ios.foreach { io =>
      val name = attrId match {
        case _: RawIdentifier | _: SpecialIdentifier => return
        case _ => idToStr(attrId)
      }
      rep match {
        case NoRepeat =>
          out.puts("this._attrStart[\"" + name + "\"] = KaitaiStruct.pos(" + io + ")")
        case _: RepeatExpr | RepeatEos | _: RepeatUntil =>
          getOrCreatePosList("_arrStart", name, io)
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
        out.puts("this._attrEnd[\"" + name + "\"] = KaitaiStruct.pos(" + io + ")")
      case _: RepeatExpr | RepeatEos | _: RepeatUntil =>
        getOrCreatePosList("_arrEnd", name, io)
    }
  }

  override def blockScopeHeader: Unit = {
    out.puts("begin")
    out.inc
  }

  override def blockScopeFooter: Unit = {
    out.dec
    out.puts("end")
  }

  def getOrCreatePosList(listName: String, varName: String, io: String): Unit = {
    blockScopeHeader
    out.puts("_posList = get(" + listName + ", \"" + varName + "\", nothing)")
    out.puts("if _posList === nothing ")
    out.inc
    out.puts("_posList = Vector{Integer}()")
    out.puts(listName + "[\"" + varName + "\"] = _posList")
    blockScopeFooter
    out.puts(s"push!(_posList, KaitaiStruct.pos($io))")
    blockScopeFooter
  }

  override def condIfHeader(expr: Ast.expr): Unit = {
    out.puts(s"if ${expression(expr)}")
    out.inc
  }

  override def condRepeatInitAttr(id: Identifier, dataType: DataType): Unit = {
    out.puts(s"${privateMemberName(id)} = []")
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType): Unit = {
    out.puts("i = 0")
    out.puts(s"while !KaitaiStruct.iseof($io)")
    out.inc
  }
  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit =
    out.puts(s"push!(${privateMemberName(id)}, $expr)")

  override def condRepeatEosFooter: Unit = {
    out.puts("i += 1")
    blockScopeFooter
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, repeatExpr: expr): Unit = {
    out.puts(s"for i in 1:${expression(repeatExpr)}")
    out.inc
  }
  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit =
    handleAssignmentRepeatEos(id, expr)

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, untilExpr: expr): Unit = {
    out.puts("i = 0")
    out.puts("while true")
    out.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val tmpName = translator.doName(if (isRaw) Identifier.ITERATOR2 else Identifier.ITERATOR)
    out.puts(s"$tmpName = $expr")
    out.puts(s"push!(${privateMemberName(id)}, $tmpName)")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, untilExpr: expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.puts(s"if ${expression(untilExpr)}")
    out.inc
    out.puts("break")
    blockScopeFooter
    out.puts("i += 1")
    blockScopeFooter
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)} = $expr")

  override def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit =
    out.puts(s"$id = $expr")

  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    dataType match {
      case t: ReadableType =>
        s"KaitaiStruct.read${Utils.capitalize(t.apiCall(defEndian))}($io)"
      case blt: BytesLimitType =>
        s"KaitaiStruct.read_bytes($io, UInt(${expression(blt.size)}))"
      case _: BytesEosType =>
        s"KaitaiStruct.read_bytes_full($io)"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        s"KaitaiStruct.read_bytes_term($io, UInt8($terminator), $include, $consume, $eosError)"
      case BitsType1(bitEndian) =>
        s"KaitaiStruct.read_bits_int_${bitEndian.toSuffix}($io, 1) != 0"
      case BitsType(width: Int, bitEndian) =>
        s"KaitaiStruct.read_bits_int_${bitEndian.toSuffix}($io, $width)"
      case t: UserType =>
        val addParams = Utils.join(t.args.map(a => translator.translate(a)), "", ", ", ", ")
        val addArgs = if (t.isExternal(typeProvider.nowClass)) {
          ""
        } else {
          val parent = t.forcedParent match {
            case Some(USER_TYPE_NO_PARENT) => "nothing"
            case Some(fp) => translator.translate(fp)
            case None => "this"
          }
          val addEndian = t.classSpec.get.meta.endian match {
            case Some(InheritedEndian) => ", this._is_le"
            case _ => ""
          }
          s", $parent, this._root$addEndian"
        }
        s"${userType2class(t)}($addParams$io$addArgs)"
    }
  }

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean): String = {
    val expr1 = padRight match {
      case Some(padByte) => s"KaitaiStruct.bytes_strip_right($expr0, $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) => s"KaitaiStruct.bytes_terminate($expr1, $term, $include)"
      case None => expr1
    }
    expr2
  }

  override def userTypeDebugRead(id: String, dataType: DataType, assignType: DataType): Unit =
    out.puts(s"_read($id)")

  override def switchStart(id: Identifier, on: Ast.expr): Unit = {}
  override def switchCaseStart(condition: Ast.expr): Unit = {}
  override def switchCaseEnd(): Unit = {}
  override def switchElseStart(): Unit = {}
  override def switchEnd(): Unit = {}

  override def switchRequiresIfs(onType: DataType): Boolean = true
  override def switchIfStart(id: Identifier, on: Ast.expr, onType: DataType): Unit = {
    out.puts(s"_on = ${expression(on)}")
  }

  override def switchIfCaseFirstStart(condition: Ast.expr): Unit = {
    out.puts(s"if _on == ${expression(condition)}")
    out.inc
  }

  override def switchIfCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"elseif _on == ${expression(condition)}")
    out.inc
  }

  override def switchIfCaseEnd(): Unit = {
    out.dec
  }

  override def switchIfElseStart(): Unit = {
    out.puts(s"else")
    out.inc
  }

  override def switchIfEnd(): Unit = {
    out.puts("end")
  }

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"function _get_${publicMemberName(instName)}(this::${types2class(className)})")
    out.inc
  }

  def overrideGetProperty(className: List[String], instances: Map[InstanceIdentifier, InstanceSpec]): Unit = {
    if (instances.isEmpty)
      return
    out.puts(s"function Base.getproperty(obj::${types2class(className)}, sym::Symbol)")
    out.inc
    var c = "if"
    instances.keys.foreach(instName => {
      out.puts(s"$c sym === :${publicMemberName(instName)}")
      c = "elseif"
      out.inc
      out.puts(s"return _get_${publicMemberName(instName)}(obj)")
      out.dec
    })
    out.puts("else")
    out.inc
    out.puts("return getfield(obj, sym)")
    universalFooter
    universalFooter
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    out.puts(s"if ${privateMemberName(instName)} !== nothing")
    out.inc
    out.puts(s"return ${privateMemberName(instName)}")
    universalFooter
  }

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit = {
    out.puts(s"${privateMemberName(instName)}")
  }

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, EnumValueSpec)]): Unit = {
    val fullEnumName: List[String] = curClass :+ enumName
    enums.puts(s"@enum ${types2class(fullEnumName)}::Int64 begin")
    enums.inc
    enumColl.foreach { case (id: Long, label: EnumValueSpec) => enums.puts(s"${enumToStr(fullEnumName, label.name)} = ${translator.doIntLiteral(id)}") }
    enums.dec
    enums.puts("end")
    enums.puts
  }

  override def debugClassSequence(seq: List[AttrSpec]): Unit = {
    val seqStr = seq.map(attr => "\"" + idToStr(attr.id) + "\"").mkString(", ")
    out.puts(s"SEQ_FIELDS = [$seqStr]")
  }

  override def classToString(toStringExpr: Ast.expr): Unit = {
    out.puts(s"Base.show(io::IO, this::${types2class(typeProvider.nowClass.name)}) = print(io, ${translator.translate(toStringExpr)})")
    out.puts
  }

  override def idToStr(id: Identifier): String = JuliaCompiler.idToStr(id)

  override def publicMemberName(id: Identifier): String = JuliaCompiler.publicMemberName(id)

  override def privateMemberName(id: Identifier): String = s"this.${idToStr(id)}"

  override def localTemporaryName(id: Identifier): String = s"_t_${idToStr(id)}"

  override def ksErrorName(err: KSError): String = JuliaCompiler.ksErrorName(err)

  override def attrValidateExpr(
    attrId: Identifier,
    attrType: DataType,
    checkExpr: Ast.expr,
    err: KSError,
    errArgs: List[Ast.expr]
  ): Unit = {
    val errArgsStr = errArgs.map(translator.translate).mkString(", ")
    out.puts(s"if !(${translator.translate(checkExpr)})")
    out.inc
    out.puts(s"throw(${ksErrorName(err)}($errArgsStr))")
    universalFooter
  }

  def userType2class(t: UserType): String = {
    val name = t.classSpec.get.name
    // val firstName = name.head
    // val prefix = if (t.isOpaque && firstName != translator.provider.nowClass.name.head) {
    //   s"${type2class(firstName)}Module."
    // } else {
    //   ""
    // }
    // s"$prefix${types2class(name)}"
    s"${types2class(name)}"
  }
}

object JuliaCompiler extends LanguageCompilerStatic
  with UpperCamelCaseClasses
  with StreamStructNames
  with ExceptionNames {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new JuliaCompiler(tp, config)

  def idToStr(id: Identifier): String =
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => name
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => s"_m_$name"
      case RawIdentifier(innerId) => s"_raw_${idToStr(innerId)}"
    }

  def publicMemberName(id: Identifier): String =
    id match {
      case InstanceIdentifier(name) => name
      case _ => idToStr(id)
    }

  override def kstreamName: String = "KaitaiStruct.KaitaiStream"
  override def kstructName: String = "Any"
  override def ksErrorName(err: KSError): String = err match {
    case EndOfStreamError => "ErrorException"
    case ConversionError => "ArgumentError"
    case _ => s"KaitaiStruct.${err.name}"
  }

  def types2class(name: List[String]): String = name.map(x => type2class(x)).mkString("_")

  def kaitaiType2NativeType(attrType: DataType): String = {
    attrType match {
      case Int1Type(false) => "UInt8"
      case IntMultiType(false, Width2, _) => "UInt16"
      case IntMultiType(false, Width4, _) => "UInt32"
      case IntMultiType(false, Width8, _) => "UInt64"

      case Int1Type(true) => "Int8"
      case IntMultiType(true, Width2, _) => "Int16"
      case IntMultiType(true, Width4, _) => "Int32"
      case IntMultiType(true, Width8, _) => "Int64"

      case FloatMultiType(Width4, _) => "Float32"
      case FloatMultiType(Width8, _) => "Float64"

      case BitsType(_, _) => "UInt64"

      case _: BooleanType => "Bool"
      case CalcIntType => "Integer"
      case CalcFloatType => "Float64"

      case _: StrType => "String"
      case _: BytesType => "Vector{UInt8}"

      case AnyType => "Any"
      case KaitaiStreamType | OwnedKaitaiStreamType => kstreamName
      case KaitaiStructType | CalcKaitaiStructType(_) => kstructName
      // case t: UserType => types2class(t.classSpec match {
      //   case Some(cs) => if (cs.isTopLevel) cs.name else "Abstract" :: cs.name
      //   case None => t.name
      // })
      case _: UserType => "KaitaiStruct.UserType"

      // case t: EnumType => s"Union{${types2class(t.enumSpec.get.name)},Integer}"
      case t: EnumType => s"Union{Enum,Integer}"

      case at: ArrayType => s"Vector{${kaitaiType2NativeType(at.elType)}}"

      case st: SwitchType => kaitaiType2NativeType(st.combinedType)
    }
  }

  def enumToStr(typeName: List[String], enumName: String): String = {
    typeName.mkString("_") + "__" + enumName
  }
}
