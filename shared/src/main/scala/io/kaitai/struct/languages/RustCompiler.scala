package io.kaitai.struct.languages

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype.{DataType, FixedEndian, InheritedEndian, KSError}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{NoRepeat, RepeatEos, RepeatExpr, RepeatSpec, _}
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.RustTranslator
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig, Utils, ExternalType}

class RustCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with ObjectOrientedLanguage
    with UpperCamelCaseClasses
    with SingleOutputFile
    with AllocateIOLocalVar
    with UniversalFooter
    with UniversalDoc
    with FixedContentsUsingArrayByteLiteral
    with EveryReadIsExpression {

  import RustCompiler._

  override def innerClasses = false

  override def innerEnums = false

  override val translator: RustTranslator = new RustTranslator(typeProvider, config)

  override def universalFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def outImports(topClass: ClassSpec) =
    importList.toList.map((x) => s"use $x;").mkString("", "\n", "\n")

  override def indent: String = "    "
  override def outFileName(topClassName: String): String = s"$topClassName.rs"

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"// $headerComment")
    outHeader.puts

    importList.add("std::option::Option")
    importList.add("std::boxed::Box")
    importList.add("std::io::Result")
    importList.add("std::io::Cursor")
    importList.add("std::vec::Vec")
    importList.add("std::default::Default")
    importList.add("kaitai_struct::KaitaiStream")
    importList.add("kaitai_struct::KaitaiStruct")

    out.puts
  }

  override def externalTypeDeclaration(extType: ExternalType): Unit = {
    val className = type2class(extType.name.last)
    val pkg = type2classAbs(extType.name)

    importList.add(s"$pkg::$className")
  }

  override def classHeader(name: List[String]): Unit =
    classHeader(name, Some(kstructName))

  def classHeader(name: List[String], parentClass: Option[String]): Unit = {
    out.puts("#[derive(Default)]")
    out.puts(s"pub struct ${type2class(name)} {")
  }

  override def classFooter(name: List[String]): Unit = universalFooter

  override def classConstructorHeader(name: List[String], parentType: DataType, rootClassName: List[String], isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    out.puts("}")
    out.puts

    out.puts(s"impl KaitaiStruct for ${type2class(name)} {")
    out.inc

    // Parameter names
    val pIo = paramName(IoIdentifier)
    val pParent = paramName(ParentIdentifier)
    val pRoot = paramName(RootIdentifier)

    // Types
    val tIo = kstreamName
    val tParent = kaitaiType2NativeType(parentType)

    out.puts(s"fn new<S: KaitaiStream>(stream: &mut S,")
    out.puts(s"                        _parent: &Option<Box<KaitaiStruct>>,")
    out.puts(s"                        _root: &Option<Box<KaitaiStruct>>)")
    out.puts(s"                        -> Result<Self>")
    out.inc
    out.puts(s"where Self: Sized {")

    out.puts(s"let mut s: Self = Default::default();")
    out.puts

    out.puts(s"s.stream = stream;")

    out.puts(s"s.read(stream, _parent, _root)?;")
    out.puts

    out.puts("Ok(s)")
    out.dec
    out.puts("}")
    out.puts
  }

  override def runRead(name: List[String]): Unit = {

  }

  override def runReadCalc(): Unit = {

  }

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean) = {
    out.puts
    out.puts(s"fn read<S: KaitaiStream>(&mut self,")
    out.puts(s"                         stream: &mut S,")
    out.puts(s"                         _parent: &Option<Box<KaitaiStruct>>,")
    out.puts(s"                         _root: &Option<Box<KaitaiStruct>>)")
    out.puts(s"                         -> Result<()>")
    out.inc
    out.puts(s"where Self: Sized {")
  }

  override def readFooter(): Unit = {
    out.puts
    out.puts("Ok(())")
    out.dec
    out.puts("}")
  }

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    attrName match {
      case ParentIdentifier | RootIdentifier | IoIdentifier =>
        // just ignore it for now
      case IoIdentifier =>
        out.puts(s"     stream: ${kaitaiType2NativeType(attrType)},")
      case _ =>
        out.puts(s"    pub ${idToStr(attrName)}: ${kaitaiType2NativeType(attrType)},")
    }
  }

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {

  }

  override def universalDoc(doc: DocSpec): Unit = {
    if (doc.summary.isDefined) {
      out.puts
      out.puts("/*")
      doc.summary.foreach((summary) => out.putsLines(" * ", summary))
      out.puts(" */")
    }
  }

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {
    out.puts("if ($this->_m__is_le) {")
    out.inc
    leProc()
    out.dec
    out.puts("} else {")
    out.inc
    beProc()
    out.dec
    out.puts("}")
  }

  override def attrFixedContentsParse(attrName: Identifier, contents: String): Unit =
    out.puts(s"${privateMemberName(attrName)} = $normalIO.ensureFixedContents($contents);")

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec): Unit = {
    val srcExpr = getRawIdExpr(varSrc, rep)

    val expr = proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "processXorOne"
          case _: BytesType => "processXorMany"
        }
        s"$kstreamName::$procName($srcExpr, ${expression(xorValue)})"
      case ProcessZlib =>
        s"$kstreamName::processZlib($srcExpr);"
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        s"$kstreamName::processRotateLeft($srcExpr, $expr, 1)"
      case ProcessCustom(name, args) =>
        val procClass = if (name.length == 1) {
          val onlyName = name.head
          val className = type2class(onlyName)
          importList.add(s"$onlyName::$className")
          className
        } else {
          val pkgName = type2classAbs(name.init)
          val className = type2class(name.last)
          importList.add(s"$pkgName::$className")
          s"$pkgName::$className"
        }

        out.puts(s"let _process = $procClass::new(${args.map(expression).mkString(", ")});")
        s"_process.decode($srcExpr)"
    }
    handleAssignment(varDest, expr, rep, false)
  }

  override def allocateIO(id: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(id)

    val args = rep match {
      case RepeatUntil(_) => translator.doLocalName(Identifier.ITERATOR2)
      case _ => getRawIdExpr(id, rep)
    }

    out.puts(s"let mut io = Cursor::new($args);")
    "io"
  }

  def getRawIdExpr(varName: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(varName)
    rep match {
      case NoRepeat => memberName
      case _ => s"$memberName.last()"
    }
  }

  override def useIO(ioEx: Ast.expr): String = {
    out.puts(s"let mut io = ${expression(ioEx)};")
    "io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"let _pos = $io.pos();")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io.seek(${expression(pos)});")

  override def popPos(io: String): Unit =
    out.puts(s"$io.seek(_pos);")

  override def alignToByte(io: String): Unit =
    out.puts(s"$io.alignToByte();")

  override def condIfHeader(expr: Ast.expr): Unit = {
    out.puts(s"if ${expression(expr)} {")
    out.inc
  }

  override def condRepeatInitAttr(id: Identifier, dataType: DataType): Unit =
    out.puts(s"${privateMemberName(id)} = vec!();")

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType): Unit = {
    out.puts(s"while !$io.isEof() {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}.append($expr);")
  }

  override def condRepeatEosFooter: Unit = {
    super.condRepeatEosFooter
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, repeatExpr: Ast.expr): Unit = {
    out.puts(s"for i in 0..${expression(repeatExpr)} {")
    out.inc
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit =
    handleAssignmentRepeatEos(id, expr)

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, untilExpr: Ast.expr): Unit = {
    out.puts("while {")
    out.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val tempVar = if (isRaw) {
      translator.doLocalName(Identifier.ITERATOR2)
    } else {
      translator.doLocalName(Identifier.ITERATOR)
    }
    out.puts(s"let $tempVar = $expr;")
    out.puts(s"${privateMemberName(id)}.append($tempVar);")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, untilExpr: Ast.expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.puts(s"!(${expression(untilExpr)})")
    out.dec
    out.puts("} { }")
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)} = $expr;")
  }

  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    dataType match {
      case t: ReadableType =>
        s"$io.read_${t.apiCall(defEndian)}()?"
      case blt: BytesLimitType =>
        s"$io.read_bytes(${expression(blt.size)})?"
      case _: BytesEosType =>
        s"$io.read_bytes_full()?"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        val term = terminator.head & 0xff
        s"$io.read_bytes_term($term, $include, $consume, $eosError)?"
      case BitsType1(bitEndian) =>
        s"$io.read_bits_int(1)? != 0"
      case BitsType(width: Int, bitEndian) =>
        s"$io.read_bits_int($width)?"
      case t: UserType =>
        val addParams = Utils.join(t.args.map((a) => translator.translate(a)), "", ", ", ", ")
        val addArgs = if (t.isExternal(typeProvider.nowClass)) {
          ""
        } else {
          val parent = t.forcedParent match {
            case Some(USER_TYPE_NO_PARENT) => "null"
            case Some(fp) => translator.translate(fp)
            case None => "self"
          }
          val addEndian = t.classSpec.get.meta.endian match {
            case Some(InheritedEndian) => s", ${privateMemberName(EndianIdentifier)}"
            case _ => ""
          }
          s", $parent, ${privateMemberName(RootIdentifier)}$addEndian"
        }

        s"Box::new(${translator.types2classAbs(t.classSpec.get.name)}::new(self.stream, self, _root)?)"
    }
  }

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Seq[Byte]], include: Boolean): String = {
    val expr1 = padRight match {
      case Some(padByte) => s"$kstreamName::bytesStripRight($expr0, $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) =>
        val t = term.head & 0xff
        s"$kstreamName::bytesTerminate($expr1, $t, $include)"
      case None => expr1
    }
    expr2
  }

  var switchIfs = false
  val NAME_SWITCH_ON = Ast.expr.Name(Ast.identifier(Identifier.SWITCH_ON))

  override def switchStart(id: Identifier, on: Ast.expr): Unit = {
    val onType = translator.detectType(on)

    switchIfs = onType match {
      case _: ArrayTypeInStream | _: BytesType => true
      case _ => false
    }

    if (!switchIfs) {
      out.puts(s"match ${expression(on)} {")
      out.inc
    }
  }

  def switchCmpExpr(condition: Ast.expr): String =
    expression(
      Ast.expr.Compare(
        NAME_SWITCH_ON,
        Ast.cmpop.Eq,
        condition
      )
    )

  override def switchCaseFirstStart(condition: Ast.expr): Unit = {
    if (switchIfs) {
      out.puts(s"if ${switchCmpExpr(condition)} {")
      out.inc
    } else {
      switchCaseStart(condition)
    }
  }

  override def switchCaseStart(condition: Ast.expr): Unit = {
    if (switchIfs) {
      out.puts(s"else if ${switchCmpExpr(condition)} {")
      out.inc
    } else {
      out.puts(s"${expression(condition)} => {")
      out.inc
    }
  }

  override def switchCaseEnd(): Unit = {
    if (switchIfs) {
      out.dec
      out.puts("}")
    } else {
      out.dec
      out.puts("},")
    }
  }

  override def switchElseStart(): Unit = {
    if (switchIfs) {
      out.puts("else {")
      out.inc
    } else {
      out.puts("_ => {")
      out.inc
    }
  }

  override def switchElseEnd(): Unit = {
    out.dec
    out.puts("}")
  }

  override def switchEnd(): Unit = universalFooter

  override def instanceDeclaration(attrName: InstanceIdentifier, attrType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"    pub ${idToStr(attrName)}: Option<${kaitaiType2NativeType(attrType)}>,")
  }

  override def instanceDeclHeader(className: List[String]): Unit = {
    out.dec
    out.puts("}")
    out.puts

    out.puts(s"impl ${type2class(className)} {")
    out.inc
  }

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"fn ${idToStr(instName)}(&mut self) -> ${kaitaiType2NativeType(dataType)} {")
    out.inc
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    out.puts(s"if let Some(x) = ${privateMemberName(instName)} {")
    out.inc
    out.puts("return x;")
    out.dec
    out.puts("}")
    out.puts
  }

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit = {
    out.puts(s"return ${privateMemberName(instName)};")
  }

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, EnumValueSpec)]): Unit = {
    val enumClass = type2class(curClass ::: List(enumName))

    out.puts(s"enum $enumClass {")
    out.inc

    enumColl.foreach { case (id, label) =>
      universalDoc(label.doc)
      out.puts(s"${value2Const(label.name)},")
    }

    out.dec
    out.puts("}")
  }

  def value2Const(label: String) = Utils.upperUnderscoreCase(label)

  def idToStr(id: Identifier): String = {
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => Utils.lowerCamelCase(name)
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => Utils.lowerCamelCase(name)
      case RawIdentifier(innerId) => "_raw_" + idToStr(innerId)
    }
  }

  override def privateMemberName(id: Identifier): String = {
    id match {
      case IoIdentifier => s"self.stream"
      case RootIdentifier => s"_root"
      case ParentIdentifier => s"_parent"
      case _ => s"self.${idToStr(id)}"
    }
  }

  override def publicMemberName(id: Identifier) = idToStr(id)

  override def localTemporaryName(id: Identifier): String = s"$$_t_${idToStr(id)}"

  override def paramName(id: Identifier): String = s"${idToStr(id)}"

  def kaitaiType2NativeType(attrType: DataType): String = {
    attrType match {
      case Int1Type(false) => "u8"
      case IntMultiType(false, Width2, _) => "u16"
      case IntMultiType(false, Width4, _) => "u32"
      case IntMultiType(false, Width8, _) => "u64"

      case Int1Type(true) => "i8"
      case IntMultiType(true, Width2, _) => "i16"
      case IntMultiType(true, Width4, _) => "i32"
      case IntMultiType(true, Width8, _) => "i64"

      case FloatMultiType(Width4, _) => "f32"
      case FloatMultiType(Width8, _) => "f64"

      case BitsType(_, _) => "u64"

      case _: BooleanType => "bool"
      case CalcIntType => "i32"
      case CalcFloatType => "f64"

      case _: StrType => "String"
      case _: BytesType => "Vec<u8>"

      case t: UserType => t.classSpec match {
        case Some(cs) => s"Box<${type2class(cs.name)}>"
        case None => s"Box<${type2class(t.name)}>"
      }

      case t: EnumType => t.enumSpec match {
        case Some(cs) => s"Box<${type2class(cs.name)}>"
        case None => s"Box<${type2class(t.name)}>"
      }

      case at: ArrayType => s"Vec<${kaitaiType2NativeType(at.elType)}>"

      case KaitaiStreamType | OwnedKaitaiStreamType => s"Option<Box<KaitaiStream>>"
      case KaitaiStructType | CalcKaitaiStructType(_) => s"Option<Box<KaitaiStruct>>"

      case st: SwitchType => kaitaiType2NativeType(st.combinedType)
    }
  }

  def kaitaiType2Default(attrType: DataType): String = {
    attrType match {
      case Int1Type(false) => "0"
      case IntMultiType(false, Width2, _) => "0"
      case IntMultiType(false, Width4, _) => "0"
      case IntMultiType(false, Width8, _) => "0"

      case Int1Type(true) => "0"
      case IntMultiType(true, Width2, _) => "0"
      case IntMultiType(true, Width4, _) => "0"
      case IntMultiType(true, Width8, _) => "0"

      case FloatMultiType(Width4, _) => "0"
      case FloatMultiType(Width8, _) => "0"

      case BitsType(_, _) => "0"

      case _: BooleanType => "false"
      case CalcIntType => "0"
      case CalcFloatType => "0"

      case _: StrType => "\"\""
      case _: BytesType => "vec!()"

      case t: UserType => "Default::default()"
      case t: EnumType => "Default::default()"

      case ArrayTypeInStream(inType) => "vec!()"

      case KaitaiStreamType | OwnedKaitaiStreamType => "None"
      case KaitaiStructType => "None"

      case _: SwitchType => ""
      // TODO
    }
  }

  def type2class(names: List[String]) = types2classRel(names)

  def type2classAbs(names: List[String]) =
    names.mkString("::")

  override def ksErrorName(err: KSError): String = RustCompiler.ksErrorName(err)
}

object RustCompiler extends LanguageCompilerStatic
  with StreamStructNames
  with UpperCamelCaseClasses
  with ExceptionNames {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new RustCompiler(tp, config)

  override def kstructName = "&Option<Box<KaitaiStruct>>"
  override def kstreamName = "&mut S"
  override def ksErrorName(err: KSError): String = ???

  def types2class(typeName: Ast.typeId) = {
    typeName.names.map(type2class).mkString(
      if (typeName.absolute) "__" else "",
      "__",
      ""
    )
  }

  def types2classRel(names: List[String]) =
    names.map(type2class).mkString("__")
}
