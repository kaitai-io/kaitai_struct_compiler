package io.kaitai.struct.languages

import io.kaitai.struct._
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype.{CalcEndian, DataType, FixedEndian, InheritedEndian}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{NoRepeat, RepeatEos, RepeatExpr, RepeatSpec, _}
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.RustTranslator
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig, Utils}

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

  val outStructDefns = new StringLanguageOutputWriter(indent)
  val outPreInitialiser = new StringLanguageOutputWriter(indent)
  val outInitialiser = new StringLanguageOutputWriter(indent)

  override def results(topClass: ClassSpec): Map[String, String] = {
    val fn = topClass.nameAsStr
    Map(
      s"$fn.rs" -> (outHeader.result + outStructDefns.result + outPreInitialiser.result + outInitialiser.result + out.result)
    )
  }

  override def universalFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def indent: String = "    "
  override def outFileName(topClassName: String): String = s"$topClassName.rs"

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"// $headerComment")
    outHeader.puts
    
    outHeader.puts("use std::{")
    outHeader.puts("    option::Option,")
    outHeader.puts("    boxed::Box,")
    outHeader.puts("    io::Result")
    outHeader.puts("};")
    outHeader.puts

    outHeader.puts("use kaitai_struct::{")
    outHeader.puts("    KaitaiStream,")
    outHeader.puts("    KaitaiStruct")
    outHeader.puts("};")
    outHeader.puts
  }

  override def classHeader(name: List[String]): Unit =
    classHeader(name, Some(kstructName))

  def classHeader(name: List[String], parentClass: Option[String]): Unit = {
    outHeader.puts(s"pub struct ${type2class(name.last)} {")
  }

  override def classFooter(name: List[String]): Unit = universalFooter

  override def classConstructorHeader(name: List[String], parentType: DataType, rootClassName: List[String], isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    outPreInitialiser.puts("}")
    outPreInitialiser.puts

    outPreInitialiser.puts(s"impl KaitaiStruct for ${type2class(name.last)} {")
    outPreInitialiser.inc
    out.inc
    
    // Parameter names
    val pIo = paramName(IoIdentifier)
    val pParent = paramName(ParentIdentifier)
    val pRoot = paramName(RootIdentifier)

    // Types
    val tIo = kstreamName
    val tParent = kaitaiType2NativeType(parentType)

    outPreInitialiser.puts(s"fn new<S: KaitaiStream>(stream: &mut S,")
    outPreInitialiser.puts(s"                        _parent: &Option<Box<KaitaiStruct>>,")
    outPreInitialiser.puts(s"                        _root: &Option<Box<KaitaiStruct>>)")
    outPreInitialiser.puts(s"                        -> Result<Self>")
    outPreInitialiser.inc
    out.inc
    outPreInitialiser.puts(s"where Self: Sized {")

    outPreInitialiser.puts(s"let mut s = Self {")
    
    out.puts(s"};")
    out.puts

    out.puts(s"s.read(stream, _parent, _root)?;")
    out.puts

    out.puts("Ok(s)")
  }

  override def runRead(): Unit = {

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
      case _ =>
        outStructDefns.puts(s"    pub ${idToStr(attrName)}: ${kaitaiType2NativeType(attrType)},")
	outInitialiser.puts(s"            ${idToStr(attrName)}: ${kaitaiType2Default(attrType)},")
    }
  }

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
  
  }

  override def universalDoc(doc: DocSpec): Unit = {
    if (doc.summary.isDefined) {
      out.puts
      out.puts("/**")
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
    out.puts(s"${privateMemberName(attrName)} = $normalIO->ensureFixedContents($contents);")

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit = {
    val srcName = privateMemberName(varSrc)
    val destName = privateMemberName(varDest)

    proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "processXorOne"
          case _: BytesType => "processXorMany"
        }
        out.puts(s"$destName = $kstreamName::$procName($srcName, ${expression(xorValue)});")
      case ProcessZlib =>
        out.puts(s"$destName = $kstreamName::processZlib($srcName);")
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        out.puts(s"$destName = $kstreamName::processRotateLeft($srcName, $expr, 1);")
      case ProcessCustom(name, args) =>
        val isAbsolute = name.length > 1
        val procClass = name.map((x) => type2class(x)).mkString(
          if (isAbsolute) "\\" else "", "\\", ""
        )
        out.puts(s"$$_process = new $procClass(${args.map(expression).mkString(", ")});")
        out.puts(s"$destName = $$_process->decode($srcName);")
    }
  }

  override def allocateIO(id: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(id)

    val args = rep match {
      case RepeatEos | RepeatExpr(_) => s"end($memberName)"
      case RepeatUntil(_) => translator.doLocalName(Identifier.ITERATOR2)
      case NoRepeat => memberName
    }

    out.puts(s"$$io = new $kstreamName($args);")
    "$io"
  }

  override def useIO(ioEx: Ast.expr): String = {
    out.puts(s"$$io = ${expression(ioEx)};")
    "$io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"$$_pos = $io->pos();")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io->seek(${expression(pos)});")

  override def popPos(io: String): Unit =
    out.puts(s"$io->seek($$_pos);")

  override def alignToByte(io: String): Unit =
    out.puts(s"$io->alignToByte();")

  override def condIfHeader(expr: Ast.expr): Unit = {
    out.puts(s"if (${expression(expr)}) {")
    out.inc
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = [];")
    out.puts(s"${privateMemberName(id)} = [];")
    out.puts("$i = 0;")
    out.puts(s"while (!$io->isEof()) {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}[] = $expr;")
  }

  override def condRepeatEosFooter: Unit = {
    out.puts("$i++;")
    super.condRepeatEosFooter
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: Ast.expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = [];")
    out.puts(s"${privateMemberName(id)} = [];")
    out.puts(s"$$n = ${expression(repeatExpr)};")
    out.puts("for ($i = 0; $i < $n; $i++) {")
    out.inc
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}[] = $expr;")
  }

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, untilExpr: Ast.expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = [];")
    out.puts(s"${privateMemberName(id)} = [];")
    out.puts("$i = 0;")
    out.puts("do {")
    out.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val tmpName = translator.doLocalName(if (isRaw) Identifier.ITERATOR2 else Identifier.ITERATOR)
    out.puts(s"$tmpName = $expr;")
    out.puts(s"${privateMemberName(id)}[] = $tmpName;")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, untilExpr: Ast.expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.puts("$i++;")
    out.dec
    out.puts(s"} while (!(${expression(untilExpr)}));")
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)} = $expr;")
  }

  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    dataType match {
      case t: ReadableType =>
        s"$io.read_${t.apiCall(defEndian)}()?"
      case blt: BytesLimitType =>
        s"$io->readBytes(${expression(blt.size)})"
      case _: BytesEosType =>
        s"$io->readBytesFull()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        s"$io->readBytesTerm($terminator, $include, $consume, $eosError)"
      case BitsType1 =>
        s"$io->readBitsInt(1) != 0"
      case BitsType(width: Int) =>
        s"$io->readBitsInt($width)"
      case t: UserType =>
        val addParams = Utils.join(t.args.map((a) => translator.translate(a)), "", ", ", ", ")
        val addArgs = if (t.isOpaque) {
          ""
        } else {
          val parent = t.forcedParent match {
            case Some(USER_TYPE_NO_PARENT) => "null"
            case Some(fp) => translator.translate(fp)
            case None => "$this"
          }
          val addEndian = t.classSpec.get.meta.endian match {
            case Some(InheritedEndian) => s", ${privateMemberName(EndianIdentifier)}"
            case _ => ""
          }
          s", $parent, ${privateMemberName(RootIdentifier)}$addEndian"
        }
        s"new ${translator.types2classAbs(t.classSpec.get.name)}($addParams$io$addArgs)"
    }
  }

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean): String = {
    val expr1 = padRight match {
      case Some(padByte) => s"$kstreamName::bytesStripRight($expr0, $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) => s"$kstreamName::bytesTerminate($expr1, $term, $include)"
      case None => expr1
    }
    expr2
  }

  override def switchStart(id: Identifier, on: Ast.expr): Unit = {
    val onType = translator.detectType(on)

    out.puts(s"switch (${expression(on)}) {")
    out.inc
  }

  override def switchCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"case ${expression(condition)}:")
    out.inc
  }

  override def switchCaseEnd(): Unit = {
    out.puts("break;")
    out.dec
  }

  override def switchElseStart(): Unit = {
    out.puts("default:")
    out.inc
  }

  override def switchEnd(): Unit = universalFooter

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"public function ${idToStr(instName)}() {")
    out.inc
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier): Unit = {
    out.puts(s"if (${privateMemberName(instName)} !== null)")
    out.inc
    instanceReturn(instName)
    out.dec
  }

  override def instanceReturn(instName: InstanceIdentifier): Unit = {
    out.puts(s"return ${privateMemberName(instName)};")
  }

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, EnumValueSpec)]): Unit = {
    classHeader(curClass ::: List(enumName), None)
    enumColl.foreach { case (id, label) =>
      universalDoc(label.doc)
      out.puts(s"const ${value2Const(label.name)} = $id;")
    }
    universalFooter
  }

  def value2Const(label: String) = label.toUpperCase

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
      case IoIdentifier => s"stream"
      case RootIdentifier => s"_root"
      case ParentIdentifier => s"_parent"
      case _ => s"self.${idToStr(id)}"
    }
  }

  override def publicMemberName(id: Identifier) = idToStr(id)

  override def localTemporaryName(id: Identifier): String = s"$$_t_${idToStr(id)}"

  override def paramName(id: Identifier): String = s"${idToStr(id)}"

  /**
    * Determine PHP data type corresponding to a KS data type. Currently unused due to
    * problems with nullable types (which were introduced only in PHP 7.1).
    *
    * @param attrType KS data type
    * @return PHP data type
    */
    
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

      case BitsType(_) => "u64"

      case _: BooleanType => "bool"
      case CalcIntType => "i32"
      case CalcFloatType => "f64"

      case _: StrType => "String"
      case _: BytesType => "String"

      case t: UserType => ""
      case t: EnumType => ""
      // TODO: figure this out

      case ArrayType(inType) => s"Vec<${kaitaiType2NativeType(inType)}>*"

      case KaitaiStreamType => s"$kstreamName"
      case KaitaiStructType => s"$kstructName"
      
      case SwitchType(on, cases) => ""
      // TODO
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

      case FloatMultiType(Width4, _) => "f32"
      case FloatMultiType(Width8, _) => "f64"

      case BitsType(_) => "u64"

      case _: BooleanType => "bool"
      case CalcIntType => "i32"
      case CalcFloatType => "f64"

      case _: StrType => "String"
      case _: BytesType => "String"

      case t: UserType => ""
      case t: EnumType => ""
      // TODO: figure this out

      case ArrayType(inType) => s"Vec<${kaitaiType2NativeType(inType)}>*"

      case KaitaiStreamType => s"$kstreamName"
      case KaitaiStructType => s"$kstructName"
      
      case SwitchType(on, cases) => ""
      // TODO
    }
  }
}

object RustCompiler extends LanguageCompilerStatic
  with StreamStructNames
  with UpperCamelCaseClasses {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new RustCompiler(tp, config)

  override def kstructName = "&Option<Box<KaitaiStruct>>"
  override def kstreamName = "&mut S"

  def types2class(typeName: Ast.typeId) = {
    typeName.names.map(type2class).mkString(
      if (typeName.absolute) "::" else "",
      "::",
      ""
    )
  }

  def types2classRel(names: List[String]) =
    names.map(type2class).mkString("::")

  override def type2class(name: String) = name
}
