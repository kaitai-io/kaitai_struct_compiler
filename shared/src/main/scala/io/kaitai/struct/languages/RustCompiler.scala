package io.kaitai.struct.languages

import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig, Utils, _}
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype.{CalcEndian, DataType, FixedEndian, InheritedEndian}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{NoRepeat, RepeatEos, RepeatExpr, RepeatSpec, _}
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.{RustTranslator, TypeDetector}

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

  override def outImports(topClass: ClassSpec): String =
    importList.toList.map(x => s"use $x;").mkString("", "\n", "\n")

  override def indent: String = "    "
  override def outFileName(topClassName: String): String = s"$topClassName.rs"

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"// $headerComment")
    outHeader.puts

    importList.add("kaitai_runtime")
    importList.add("kaitai_runtime::KaitaiError")
    importList.add("kaitai_runtime::KaitaiStream")
    importList.add("kaitai_runtime::KaitaiStruct")
    importList.add("std::convert::TryFrom")
    importList.add("std::default::Default")
    importList.add("std::vec::Vec")

    out.puts
  }

  override def opaqueClassDeclaration(classSpec: ClassSpec): Unit = {
    val name = type2class(classSpec.name.last)
    val pkg = type2classAbs(classSpec.name)
    
    importList.add(s"$pkg::$name")
  }

  override def classHeader(name: List[String]): Unit =
    classHeader(name, Some(kstructName))

  def classHeader(name: List[String], parentClass: Option[String]): Unit = {
    out.puts("#[derive(Default)]")
    out.puts(s"pub struct ${type2class(name)}<'a> {")
  }

  override def classFooter(name: List[String]): Unit = universalFooter

  override def classConstructorHeader(name: List[String], parentType: DataType, rootClassName: List[String], isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    // Parameter names
    val pIo = paramName(IoIdentifier)
    val pParent = paramName(ParentIdentifier)
    val pRoot = paramName(RootIdentifier)

    // Types
    val tIo = kstreamName
    val tParent = kaitaiType2NativeType(parentType)
    val tRoot = kaitaiType2NativeType(CalcUserType(rootClassName, None))

    val isRoot = name == rootClassName

    if (isRoot) {
      out.puts(s"    _parent: Option<&'a Self>,")
    } else {
      out.puts(s"    _parent: Option<&'a $tParent<'a>>,")
    }
    out.puts(s"    _root: Option<&'a $tRoot<'a>>,")
    out.puts("}")
    out.puts

    out.puts(s"impl<'a> KaitaiStruct<'a> for ${type2class(name)}<'a> {")
    out.inc

    if (name == rootClassName) {
      out.puts(s"type Parent = Self;")
      out.puts(s"type Root = Self;")
    } else {
      out.puts(s"type Parent = $tParent<'a>;")
      out.puts(s"type Root = $tRoot<'a>;")
    }
    out.puts

    out.puts(s"fn new(")
    out.puts(s"    _parent: Option<&'a Self::Parent>,")
    out.puts(s"    _root: Option<&'a Self::Root>")
    out.puts(s") -> kaitai_runtime::Result<'a, Self>")
    out.puts(s"where")
    out.puts(s"    Self: Sized,")
    out.puts(s"{")
    out.inc

    out.puts(s"let mut s: Self = Default::default();")
    out.puts(s"s._parent = _parent;")
    if (isRoot) {
      out.puts(s"s._root = _root.or(Some(&s));")
    } else {
      out.puts(s"s._root = _root;")
    }
    out.puts(s"Ok(s)")

    out.dec
    out.puts(s"}")
  }

  override def runRead(): Unit = {

  }

  override def runReadCalc(): Unit = {
  
  }

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = {
    out.puts
    out.puts(s"fn read<S: KaitaiStream>(")
    out.puts(s"    &mut self,")
    out.puts(s"    stream: &mut S,")
    out.puts(s") -> kaitai_runtime::Result<'a, ()>")
    out.puts(s"where")
    out.puts(s"    Self: Sized")
    out.puts(s"{")
    out.inc
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
        out.puts(s"    stream: ${kaitaiType2NativeType(attrType)},")
      case _ =>
        out.puts(s"    pub ${idToStr(attrName)}: ${kaitaiType2NativeType(attrType)},")
    }
  }

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
  
  }

  override def universalDoc(doc: DocSpec): Unit = {
    if (doc.summary.isDefined) {
      out.puts
      doc.summary.foreach(summary => out.putsLines("/// ", summary))
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
    out.puts(s"${privateMemberName(attrName)} = $normalIO.ensure_fixed_cntents($contents);")

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit = {
    val srcName = privateMemberName(varSrc)
    val destName = privateMemberName(varDest)

    proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "process_xor_one"
          case _: BytesType => "process_xor_many"
        }
        out.puts(s"$destName = $kstreamName::$procName($srcName, ${expression(xorValue)});")
      case ProcessZlib =>
        out.puts(s"$destName = $kstreamName::process_zlib($srcName);")
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        out.puts(s"$destName = $kstreamName::process_rotate_left($srcName, $expr, 1);")
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
        out.puts(s"$destName = _process.decode($srcName);")
    }
  }

  override def allocateIO(id: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(id)

    val args = rep match {
      case RepeatEos | RepeatExpr(_) => s"$memberName.last()"
      case RepeatUntil(_) => translator.doLocalName(Identifier.ITERATOR2)
      case NoRepeat => memberName
    }

    out.puts(s"let mut io = Cursor::new($args);")
    "io"
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
    out.puts(s"$io.align_to_byte();")

  override def condIfHeader(expr: Ast.expr): Unit = {
    out.puts(s"if ${expression(expr)} {")
    out.inc
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = [];")
    out.puts(s"${privateMemberName(id)} = [];")
    out.puts(s"while !$io.is_eof() {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}.push($expr);")
  }

  override def condRepeatEosFooter: Unit = {
    super.condRepeatEosFooter
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: Ast.expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = vec!();")
    out.puts(s"${privateMemberName(id)} = vec!();")
    out.puts(s"for i in 0..${expression(repeatExpr)} {")
    out.inc
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}.push($expr);")
  }

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, untilExpr: Ast.expr): Unit = {
    // TODO: We've already initialized this as part of the class, is it really necessary to call `vec!()` again?
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = vec!();")
    out.puts(s"${privateMemberName(id)} = vec!();")
    out.puts("while {")
    out.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val tempVar = if (isRaw) {
      translator.doLocalName(Identifier.ITERATOR2)
    } else {
      translator.doLocalName(Identifier.ITERATOR)
    }
    out.puts(s"let mut $tempVar = $expr;")
    out.puts(s"${privateMemberName(id)}.push($tempVar);")
    out.puts(s"$tempVar.read(stream);")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, untilExpr: Ast.expr): Unit = {
    // TODO: I don't think this is OK? Looks like `while` loops will just spin indefinitely
    typeProvider._currentIteratorType = Some(dataType)
    out.puts(s"!(${expression(untilExpr)})")
    out.dec
    out.puts("} { }")
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)} = $expr;")
  }

  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    // TODO: assignType to indicate we're assigning to an Enum?
    // Right now we use `.into()` everywhere
    dataType match {
      case t: ReadableType =>
        s"$io.read_${t.apiCall(defEndian)}()?.into()?"
      case blt: BytesLimitType =>
        s"$io.read_bytes(${expression(blt.size)})?"
      case _: BytesEosType =>
        s"$io.read_bytes_full()?"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        s"$io.read_bytes_term($terminator, $include, $consume, $eosError)?"
      case BitsType1 =>
        s"$io.read_bits_int(1)? != 0"
      case BitsType(width: Int) =>
        s"$io.read_bits_int($width)?.into()?"
      case t: UserType =>
        val addParams = Utils.join(t.args.map(a => translator.translate(a)), "", ", ", ", ")
        val addArgs = if (t.isOpaque) {
          ""
        } else {
          val parent = t.forcedParent match {
            // TODO: Maybe `None` instead?
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
	
        s"${translator.types2classAbs(t.classSpec.get.name)}::new(Some(self), self.${privateMemberName(RootIdentifier)})?"
    }
  }

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean): String = {
    val expr1 = padRight match {
      case Some(padByte) => s"$kstreamName::bytes_strip_right($expr0, $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) => s"$kstreamName::bytes_terminate($expr1, $term, $include)"
      case None => expr1
    }
    expr2
  }

  var switchIfs = false
  val NAME_SWITCH_ON = Ast.expr.Name(Ast.identifier(Identifier.SWITCH_ON))

  override def switchStart(id: Identifier, on: Ast.expr): Unit = {
    val onType = translator.detectType(on)

    switchIfs = onType match {
      case _: ArrayType | _: BytesType => true
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

    out.puts(s"impl<'a> ${type2class(className)}<'a> {")
    out.inc
  }

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"fn ${idToStr(instName)}(&mut self) -> ${kaitaiType2NativeType(dataType)} {")
    out.inc
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    out.puts(s"if let Some(x) = ${privateMemberName(instName)} {")
    out.puts("    return x;")
    out.puts("}")
    out.puts
  }

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit = {
    out.puts(s"return ${privateMemberName(instName)}.unwrap();")
  }

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, EnumValueSpec)]): Unit = {
    val enumClass = type2class(curClass ::: List(enumName))

    out.puts(s"enum $enumClass {")
    out.inc
    
    enumColl.foreach { case (_, label) =>
      universalDoc(label.doc)
      out.puts(s"${Utils.upperCamelCase(label.name)},")
    }

    out.dec
    out.puts("}")

    out.puts
    out.puts(s"impl TryFrom<u64> for $enumClass {")

    out.inc
    out.puts(s"type Error = KaitaiError<'static>;")
    out.puts(s"fn try_from(flag: u64) -> kaitai_runtime::Result<'static, $enumClass> {")

    out.inc
    out.puts(s"match flag {")

    out.inc
    enumColl.foreach { case (value, label) =>
        out.puts(s"$value => Ok($enumClass::${Utils.upperCamelCase(label.name)}),")
    }
    out.puts("_ => Err(KaitaiError::UnknownEnum(flag)),")
    out.dec

    out.puts(s"}")
    out.dec
    out.puts(s"}")
    out.dec
    out.puts(s"}")
    out.puts
  }

  def idToStr(id: Identifier): String = {
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => name
      case InstanceIdentifier(name) => name
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
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

  override def publicMemberName(id: Identifier): String = idToStr(id)

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

      case BitsType(_) => "u64"

      case _: BooleanType => "bool"
      case CalcIntType => "i32"
      case CalcFloatType => "f64"

      case _: StrType => "String"
      case _: BytesType => "Vec<u8>"

      case t: UserType => t.classSpec match {
        case Some(cs) => s"${type2class(cs.name)}"
        case None => s"${type2class(t.name)}"
      }
      
      case t: EnumType => t.enumSpec match {
        case Some(cs) => s"Option<${type2class(cs.name)}>"
        case None => s"Option<${type2class(t.name)}>"
      }

      case ArrayType(inType) => s"Vec<${kaitaiType2NativeType(inType)}<'a>>"

      case KaitaiStreamType => s"KaitaiStream"
      case KaitaiStructType | CalcKaitaiStructType => s"KaitaiStruct"
      
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

      case BitsType(_) => "0"

      case _: BooleanType => "false"
      case CalcIntType => "0"
      case CalcFloatType => "0"

      case _: StrType => "\"\""
      case _: BytesType => "vec!()"

      case t: UserType => "Default::default()"
      case t: EnumType => "Default::default()"

      case ArrayType(inType) => "vec!()"

      case KaitaiStreamType => "None"
      case KaitaiStructType => "None"
      
      case _: SwitchType => ""
      // TODO
    }
  }
  
  def type2class(names: List[String]): String = types2classRel(names)

  def type2classAbs(names: List[String]): String =
    names.mkString("::")
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

  def types2class(typeName: Ast.typeId): String = {
    typeName.names.map(type2class).mkString(
      if (typeName.absolute) "__" else "",
      "__",
      ""
    )
  }

  def types2classRel(names: List[String]): String =
    names.map(type2class).mkString("")
}
