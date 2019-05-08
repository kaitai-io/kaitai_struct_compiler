package io.kaitai.struct.languages

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype.{DataType, Endianness, FixedEndian}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{RepeatSpec, _}
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.RustTranslator
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig}

class RustCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with EveryReadIsExpression
    with ObjectOrientedLanguage
    with SingleOutputFile
    with UpperCamelCaseClasses
    with UniversalFooter
    with UniversalDoc {

  import RustCompiler._

  override val translator = new RustTranslator(typeProvider, config)

  override def indent: String = "    "

  override def outFileName(topClassName: String): String = s"$topClassName.rs"

  override def outImports(topClass: ClassSpec): String =
    // TODO: #![allow(unused_imports)] instead?
    importList.toList.map(i => s"#[allow(unused_imports)]\nuse $i;").mkString("", "\n", "\n")

  override def opaqueClassDeclaration(classSpec: ClassSpec): Unit = {
    // TODO: Package name?
    importList.add(s"crate::${classSpec.name.mkString("", "::", "")}::${normalizeClassName(classSpec.name)}")
  }

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"// $headerComment")
    outHeader.puts

    // Runtime-required imports
    importList.add("kaitai::{self, KError, KResult, KStream, KStruct, KStructUnit}")
    importList.add("kaitai::{kf32_max, kf64_max, kf32_min, kf64_min}")
    // TODO: `no_std` compatibility?
    importList.add("std::convert::{TryFrom, TryInto}")
    importList.add("std::str")
    importList.add("std::vec::Vec")
  }

  override def classHeader(name: List[String]): Unit = {
    // Set up the struct definition

    val lifetime = if (classContainsReferences(typeProvider.nowClass)) "<'a>" else ""

    // TODO: Derive Clone/PartialEq?
    // Can't safely derive Copy because of byte slices
    out.puts
    out.puts(s"#[derive(Default, Debug)]")
    out.puts(s"pub struct ${normalizeClassName(name)}$lifetime {")

    out.inc
  }

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    val typeName = attrName match {
      // For keeping lifetimes simple, we don't store _io, _root, or _parent with the struct
      case IoIdentifier | RootIdentifier | ParentIdentifier => return
      case _ => kaitaiTypeToNativeType(attrName, typeProvider.nowClass, attrType)
    }

    out.puts(s"${idToAccessModifier(attrName)} ${idToStr(attrName)}: $typeName,".trim)
  }

  override def instanceDeclaration(attrName: InstanceIdentifier, attrType: DataType, isNullable: Boolean): Unit =
    out.puts(s"${idToAccessModifier(attrName)} ${idToStr(attrName)}: Option<${kaitaiTypeToNativeType(attrName, typeProvider.nowClass, attrType)}>,".trim)

  def idToAccessModifier(id: Identifier): String = id match {
    case RootIdentifier | ParentIdentifier | InstanceIdentifier(_) | RawIdentifier(_) => ""
    case _ => "pub"
  }

  // Intentional no-op; Called when all sub-types have finished, which is too late
  override def classFooter(name: List[String]): Unit = {}

  override def classConstructorHeader(name: List[String], parentType: DataType, rootClassName: List[String],
                                      isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    // Normally OO languages attempt to put classes within each other; Rust can't do that, so we end the struct
    // definition here
    out.dec
    out.puts("}")

    // If there are any switch types in the `seq` body, create the enums for them
    typeProvider.nowClass.seq.foreach(a => a.dataType match {
      case switchType: SwitchType => switchTypeEnum(a.id, switchType)
      case _ => ()
    })

    val lifetime = if (classContainsReferences(typeProvider.nowClass)) "<'a>" else ""
    val currentName = s"${normalizeClassName(name)}$lifetime"
    val parentType = parentClassType(typeProvider.nowClass)
    val rootType = rootClassType(typeProvider.nowClass, typeProvider.topClass)

    out.puts(s"impl<'a> KStruct<'a> for $currentName {")

    out.inc
    out.puts(s"type Parent = $parentType;")
    out.puts(s"type Root = $rootType;")
    out.puts
  }

  override def runRead(): Unit = out.puts("// runRead()")

  override def runReadCalc(): Unit = out.puts("// runReadCalc()")

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = {
    out.puts(s"fn read<'s: 'a, S: $kstreamName>(")
    out.inc
    out.puts(s"&mut self,")
    out.puts(s"${privateMemberName(IoIdentifier)}: &'s S,")
    out.puts(s"${privateMemberName(RootIdentifier)}: Option<&Self::Root>,")
    out.puts(s"${privateMemberName(ParentIdentifier)}: Option<&Self::Parent>,")
    out.dec
    out.puts(s") -> KResult<'s, ()> {")
    out.inc

    // If there are no attributes to parse, we need to end the read implementation ourselves
    if (typeProvider.nowClass.seq.isEmpty)
      endRead()
  }

  override def readFooter(): Unit = out.puts(s"// readFooter()")

  // Intentional no-op; Rust handles ownership, so we can use struct attributes directly without readers
  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {}

  override def attrParse(attr: AttrLikeSpec, id: Identifier, defEndian: Option[Endianness]): Unit = {
    super.attrParse(attr, id, defEndian)

    // TODO: readFooter isn't getting called? Goes to universalFooter instead?
    // Right now we detect when this is the last attribute parse, and finish the read method here
    if (typeProvider.nowClass.seq.last.id == id)
      endRead()
  }

  def endRead(): Unit = {
    out.puts("Ok(())")
    out.dec
    out.puts("}")
  }

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = ???

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit = pushToMember(id, expr)

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = pushToMember(id, expr)

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val tempVar = translator.doLocalName(if (isRaw) Identifier.ITERATOR2 else Identifier.ITERATOR)

    out.puts(s"let mut $tempVar = $expr;")
    out.puts(s"${doRead(tempVar)};")
    pushToMember(id, tempVar)

    // Because of Rust's move semantics, `tempVar` is no longer available. Instead,
    // we'll re-bind that variable name to the borrowed value, instead of owned.
    out.puts(s"let $tempVar = ${privateMemberName(id)}.last().unwrap();")
  }

  def doRead(id: String, isOpaque: Boolean = false, isTopLevel: Boolean = false): String =
    if (isOpaque)
      s"$id.read(${privateMemberName(IoIdentifier)}, None, None)?"
    else {
      val rootParam = if (isTopLevel)
        s"${privateMemberName(RootIdentifier)}.or(Some(self))"
      else
        s"Some(${privateMemberName(RootIdentifier)}.ok_or(KError::MissingRoot)?)"

      s"$id.read(${privateMemberName(IoIdentifier)}, $rootParam, Some(self))?"
    }

  def pushToMember(id: Identifier, expr: String): Unit = out.puts(s"${privateMemberName(id)}.push($expr);")

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    val seqId = typeProvider.nowClass.seq.filter(a => a.id == id)

    if (seqId.isEmpty) {
      // No matching ID's in the main parse body, so this is either an instance or raw
      id match {
        case _: InstanceIdentifier => out.puts(s"${privateMemberName(id)} = Some($expr);")
        case _: RawIdentifier => out.puts("panic!(\"No idea what to do with raw idents\");")
      }

      // Currently a ton of type-related issues in instance calculations, hold off for now
      // val dataType = typeProvider.nowClass.instances(id.asInstanceOf[InstanceIdentifier]).dataTypeComposite
      //s"Some($expr as ${kaitaiTypeToNativeType(dataType)})"
    } else seqId.head.dataType match {
      case _: EnumType =>
        // Assign to enum, so handle the conversion with `TryFrom`
        out.puts(s"${privateMemberName(id)} = Some(($expr as i64).try_into()?);")
      case _: UserType => out.puts(s"${privateMemberName(id)} = Some($expr);")
      case st: SwitchType =>
        // TODO: We have no idea which variant we're constructing for assignment
        val enumName = kaitaiTypeToNativeType(id, typeProvider.nowClass, st, excludeOptionWrapper = true, excludeLifetime = true)
        // out.puts(s"${privateMemberName(id)} = Some($enumName::$variantName($expr));
        out.puts(s"${privateMemberName(id)} = None; // handleAssignmentSimple($id, $expr)")
      case _ => out.puts(s"${privateMemberName(id)} = $expr;")
    }
  }

  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String =
    dataType match {
      case t: ReadableType => s"$io.read_${t.apiCall(defEndian)}()?"
      case b: BytesLimitType =>
        // Because `b.size` can be coming from an instance, we need to safely handle looking it up
        val expr = b.size match {
          case Ast.expr.Name(_) =>
            s"${expression(b.size)}(${privateMemberName(RootIdentifier)}, ${privateMemberName(ParentIdentifier)})"
          case _ => expression(b.size)
        }
        s"$io.read_bytes($expr as usize)?"
      case _: BytesEosType => s"$io.read_bytes_full()?"
      case b: BytesTerminatedType => s"$io.read_bytes_term(${b.terminator}, ${b.include}, ${b.consume}, ${b.eosError})?"
      case BitsType1 => s"$io.read_bits_int(1)? != 0"
      case BitsType(width) => s"$io.read_bits_int($width)?"
      case u: UserType =>
        s"{ let mut tmp = ${normalizeClassName(u.classSpec.get.name)}::default(); ${doRead("tmp", u.isOpaque, typeProvider.nowClass.isTopLevel)}; tmp }"
    }

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean): String = {
    val ioId = privateMemberName(IoIdentifier)
    val expr = padRight match {
      case Some(p) => s"$ioId.bytes_strip_right($expr0, $p)"
      case None => expr0
    }
    terminator match {
      case Some(term) => s"$ioId.bytes_terminate($expr, $term, $include)"
      case None => expr
    }
  }

  override def attrFixedContentsParse(attrName: Identifier, contents: Array[Byte]): Unit =
    out.puts(s"// attrFixedContentsParse($attrName, $contents)")

  override def condIfHeader(expr: Ast.expr): Unit = {
    out.puts(s"if ${expression(expr)} {")
    out.inc
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))}.clear();")

    out.puts(s"${privateMemberName(id)}.clear();")
    out.puts(s"while !$io.is_eof()? {")
    out.inc
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: Ast.expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))}.clear();")

    out.puts(s"${privateMemberName(id)}.clear();")
    out.puts(s"let size = ${expression(repeatExpr)} as usize;")
    out.puts(s"${privateMemberName(id)}.reserve(size);")
    out.puts(s"for i in 0..size {")
    out.inc
  }

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: Ast.expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))}.clear();")

    out.puts(s"${privateMemberName(id)}.clear();")

    // Because Rust allows the `while` predicate to be an expression, we actually do all work
    // as part of the predicate, and have an empty loop body. Slightly preferable to `loop { if pred { break }}`
    out.puts("while {")
    out.inc
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: Ast.expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.puts(s"!(${expression(repeatExpr)})")
    out.dec
    out.puts("} {}")
  }

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit =
    out.puts(s"// attrProcess($proc, $varSrc, $varDest)")

  override def useIO(ioEx: Ast.expr): String = s"// useIO($ioEx)"

  override def pushPos(io: String): Unit = out.puts(s"// pushPos($io)")

  override def seek(io: String, pos: Ast.expr): Unit = out.puts(s"// seek($io, $pos)")

  override def popPos(io: String): Unit = out.puts(s"// popPos($io)")

  override def alignToByte(io: String): Unit = out.puts(s"$io.align_to_byte()?;")

  override def instanceDeclHeader(className: List[String]): Unit = {
    val lifetime = if (classContainsReferences(typeProvider.nowClass)) "<'a>" else ""
    out.puts(s"impl$lifetime ${normalizeClassName(className)}$lifetime {")
    out.inc
  }

  override def instanceDeclFooter(className: List[String]): Unit = universalFooter

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    // If the current class has no lifetime associated with it, but either the root or parent does,
    // we need to declare the lifetime in the function definition so that it can be used as a lifetime param
    val rootHasLifetime = classContainsReferences(typeProvider.topClass)
    val parentHasLifetime = typeProvider.nowClass.parentClass match {
      case t: ClassSpec => classContainsReferences(t)
      case _ => false
    }
    val fnLifetime = if (!classContainsReferences(typeProvider.nowClass) && (rootHasLifetime || parentHasLifetime)) {
      s"<'a, 's: 'a, S: $kstreamName>"
    } else {
      s"<'s, S: $kstreamName>"
    }

    out.puts(s"fn ${idToStr(instName)}$fnLifetime(")
    out.inc
    out.puts(s"&mut self,")
    out.puts(s"${privateMemberName(IoIdentifier)}: &'s S,")
    out.puts(s"${privateMemberName(RootIdentifier)}: Option<&${rootClassType(typeProvider.nowClass, typeProvider.topClass)}>,")
    out.puts(s"${privateMemberName(ParentIdentifier)}: Option<&${parentClassType(typeProvider.nowClass)}>")
    out.dec
    out.puts(s") -> KResult<'s, ${kaitaiTypeToNativeType(instName, typeProvider.nowClass, dataType)}> {")
    out.inc
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    out.puts(s"if let Some(x) = ${privateMemberName(instName)} {")
    out.puts("    return Ok(x);")
    out.puts("}")
    out.puts
  }

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit = {
    out.puts(s"${privateMemberName(instName)}.ok_or(KError::MissingInstanceValue)")
  }

  override def instanceCalculate(instName: Identifier, dataType: DataType, value: Ast.expr): Unit = {
    // Because Rust doesn't auto-widen types, we need to inform the translator that the expressions it's using
    // have to obey a specific type
    val expr = translator.translateAsType(value, Some(dataType))
    handleAssignmentSimple(instName, expr)
  }

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, EnumValueSpec)]): Unit = {
    val enumClass = normalizeClassName(List(enumName))

    // Set up the actual enum definition
    out.puts(s"#[derive(Debug, PartialEq)]")
    out.puts(s"pub enum $enumClass {")
    out.inc

    enumColl.foreach { case (_, label) =>
      if (label.doc.summary.isDefined)
        universalDoc(label.doc)

      out.puts(s"${type2class(label.name)},")
    }

    out.dec
    out.puts("}")

    // Set up parsing enums from the underlying value
    out.puts(s"impl TryFrom<i64> for $enumClass {")

    out.inc
    // We typically need the lifetime in KError for returning byte slices from stream;
    // because we can only return `UnknownVariant` which contains a Copy type, it's safe
    // to declare that the error type is `'static`
    out.puts(s"type Error = KError<'static>;")
    out.puts(s"fn try_from(flag: i64) -> KResult<'static, $enumClass> {")

    out.inc
    out.puts(s"match flag {")

    out.inc
    enumColl.foreach { case (value, label) =>
      out.puts(s"$value => Ok($enumClass::${type2class(label.name)}),")
    }
    out.puts("_ => Err(KError::UnknownVariant(flag)),")
    out.dec

    out.puts(s"}")
    out.dec
    out.puts(s"}")
    out.dec
    out.puts(s"}")
    out.puts
  }


  /**
    * Renders identifier to a string, specifically for a given
    * language and settings. This usually includes things like
    * case and separator conversion and does *not* include things
    * like prepending "@" or "this." or "self." that might be
    * used to access private member.
    *
    * @param id identifier to render
    * @return identifier as string
    */
  override def idToStr(id: Identifier): String = RustCompiler.idToStr(id)

  override def privateMemberName(id: Identifier): String = RustCompiler.privateMemberName(id)

  /**
    * Renders identifier as a proper reference to a public member
    * that represents this field.
    *
    * @param id identifier to render
    * @return identifier as string
    */
  override def publicMemberName(id: Identifier): String = s"// publicMemberName($id)"

  /**
    * Renders identifier as a proper reference to a local temporary
    * variable appropriately named to hold a temporary reference to
    * this field.
    *
    * @param id identifier to render
    * @return identifier as string
    */
  override def localTemporaryName(id: Identifier): String = s"tmp_$id"

  /**
    * Single method that outputs all kind of footers in the language.
    */
  override def universalFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def universalDoc(doc: DocSpec): Unit = {
    out.puts
    doc.summary.foreach(out.putsLines("/// ", _))
  }


  override def switchStart(id: Identifier, on: Ast.expr): Unit = {
    out.puts(s"match ${expression(on)} {")
    out.inc
  }

  var hasElseCase = false

  override def switchCaseStart(condition: Ast.expr): Unit = {
    hasElseCase = false
    out.puts(s"${expression(condition)} => {")
    out.inc
  }

  override def switchCaseEnd(): Unit = {
    out.dec
    out.puts("},")
  }

  override def switchElseStart(): Unit = {
    hasElseCase = true
    out.puts("_ => {")
    out.inc
  }

  override def switchElseEnd(): Unit = {
    out.dec
    out.puts("},")
  }

  override def switchEnd(): Unit = {
    if (!hasElseCase) {
      out.puts("_ => ()")
      hasElseCase = true
    }

    out.dec
    out.puts("}")
  }

  override def extraAttrForIO(id: Identifier, rep: RepeatSpec): List[AttrSpec] = {
    out.puts(s"// extraAttrForIO($id, $rep)")
    Nil
  }

  def switchTypeEnum(id: Identifier, st: SwitchType): Unit = {
    // Because Rust can't handle `AnyType` in the type hierarchy,
    // we generate an enum with all possible variations
    val typeName = kaitaiTypeToNativeType(id, typeProvider.nowClass, st, excludeOptionWrapper = true)
    out.puts("#[derive(Debug)]")
    out.puts(s"pub enum $typeName {")
    out.inc

    val types = st.cases.values.toSet
    types.foreach(t => {
      // Because this switch type will itself be in an option, we can exclude it from user types
      val variantName = switchVariantName(id, t)
      val typeName = kaitaiTypeToNativeType(id, typeProvider.nowClass, t, excludeOptionWrapper = true)
      out.puts(s"$variantName($typeName),")
    })

    out.dec
    out.puts("}")
  }

  def switchVariantName(id: Identifier, attrType: DataType): String = attrType match {
    case Int1Type(false) => "U1"
    case IntMultiType(false, Width2, _) => "U2"
    case IntMultiType(false, Width4, _) => "U4"
    case IntMultiType(false, Width8, _) => "U8"

    case Int1Type(true) => "S1"
    case IntMultiType(true, Width2, _) => "S2"
    case IntMultiType(true, Width4, _) => "S4"
    case IntMultiType(true, Width8, _) => "S8"

    case FloatMultiType(Width4, _) => "F4"
    case FloatMultiType(Width8, _) => "F8"

    case BitsType(_) => "Bits"
    case _: BooleanType => "Boolean"
    case CalcIntType => "Int"
    case CalcFloatType => "Float"
    case _: StrType => "String"
    case _: BytesType => "Bytes"

    case t: UserType => kaitaiTypeToNativeType(id, typeProvider.nowClass, t, excludeOptionWrapper = true, excludeLifetime = true)
    case t: EnumType => kaitaiTypeToNativeType(id, typeProvider.nowClass, t, excludeOptionWrapper = true)
    case t: ArrayType => s"Arr${switchVariantName(id, t.elType)}"
  }
}

object RustCompiler extends LanguageCompilerStatic
  with UpperCamelCaseClasses
  with StreamStructNames {

  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new RustCompiler(tp, config)

  def normalizeClassName(names: List[String]): String = type2class(names.last)

  def kaitaiTypeToNativeType(id: Identifier, cs: ClassSpec, attrType: DataType,
                             excludeOptionWrapper: Boolean = false, excludeLifetime: Boolean = false): String = attrType match {
    case _: NumericType => kaitaiPrimitiveToNativeType(attrType)
    case _: BooleanType => kaitaiPrimitiveToNativeType(attrType)
    case _: StrType => kaitaiPrimitiveToNativeType(attrType)
    case _: BytesType => kaitaiPrimitiveToNativeType(attrType)

    case t: UserType =>
      val typeName = t.classSpec match {
        case Some(spec) =>
          val lifetime = if (!excludeLifetime && classContainsReferences(spec)) "<'a>" else ""
          s"${normalizeClassName(spec.name)}$lifetime"
        case None =>
          val lifetime = if (!excludeLifetime && datatypeContainsReferences(t)) "<'a>" else ""
          s"${normalizeClassName(t.name)}$lifetime"
      }
      if (excludeOptionWrapper) typeName else s"Option<$typeName>"

    case t: EnumType =>
      val typeName = t.enumSpec match {
        case Some(spec) => s"${normalizeClassName(spec.name)}"
        case None => s"${normalizeClassName(t.name)}"
      }
      if (excludeOptionWrapper) typeName else s"Option<$typeName>"

    case t: ArrayType => s"Vec<${kaitaiTypeToNativeType(id, cs, t.elType, excludeOptionWrapper = true)}>"

    case KaitaiStreamType => kstreamName
    // TODO: Handle KStruct types
    // Right now we fail on KStruct types because the associated type parameters aren't bound
    case KaitaiStructType | CalcKaitaiStructType => kstructUnitName

    case st: SwitchType =>
      val types = st.cases.values.toSet
      val lifetime = if (!excludeLifetime && types.exists(t => datatypeContainsReferences(t))) "<'a>" else ""
      val typeName = id match {
        case name: NamedIdentifier => s"${type2class(List(cs.name.last, name.name).mkString("_"))}$lifetime"
        case name: InstanceIdentifier => s"${type2class(List(cs.name.last, name.name).mkString("_"))}$lifetime"
        case _ => kstructUnitName
      }

      if (excludeOptionWrapper) typeName else s"Option<$typeName>"
  }

  def kaitaiPrimitiveToNativeType(attrType: DataType): String = attrType match {
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

    // TODO: More precise bit reading?
    case BitsType(_) => "u64"

    case _: BooleanType => "bool"
    case CalcIntType => "i32"
    case CalcFloatType => "f64"

    case _: StrType => "&'a str"
    case _: BytesType => "&'a [u8]"
  }

  override def kstreamName: String = "KStream"
  override def kstructName: String = "KStruct<'a>"

  def kstructUnitName: String = "KStructUnit"

  def rootClassType(nowClass: ClassSpec, topClass: ClassSpec): String = {
    if (nowClass.isTopLevel) "Self" else {
      val lifetime = if (classContainsReferences(topClass)) "<'a>" else ""
      s"${normalizeClassName(topClass.name)}$lifetime"
    }
  }

  def parentClassType(nowClass: ClassSpec): String =
    if (nowClass.isTopLevel)
      kstructUnitName
    else
      nowClass.parentClass match {
        case t: ClassSpec =>
          val lifetime = if (classContainsReferences(t)) "<'a>" else ""
          s"${normalizeClassName(t.name)}$lifetime"
        case GenericStructClassSpec => kstructUnitName
      }

  def privateMemberName(id: Identifier): String = id match {
    case IoIdentifier => "_io"
    case RootIdentifier => "_root"
    case ParentIdentifier => "_parent"
    case _ => s"self.${idToStr(id)}"
  }

  def idToStr(id: Identifier): String = id match {
    case SpecialIdentifier(n) => n
    case NamedIdentifier(n) => n
    case InstanceIdentifier(n) => n
    case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
    // Raw identifiers store bytes locally, we'll parse them later
    case RawIdentifier(inner) => s"raw_${idToStr(inner)}"
  }

  def classContainsReferences(c: ClassSpec): Boolean = {
    val seqReferences = c.seq.exists(t => datatypeContainsReferences(t.dataType))

    if (seqReferences)
      true
    else
      c.instances.exists(inst => datatypeContainsReferences(inst._2.dataTypeComposite))
  }

  def datatypeContainsReferences(d: DataType): Boolean = d match {
    case _: BytesType | _: StrType => true
    case t: UserType => t.classSpec match {
      case Some(inner) => classContainsReferences(inner)
      case None => false
    }
    case t: ArrayType => datatypeContainsReferences(t.elType)
    case st: SwitchType => st.cases.values.exists(datatypeContainsReferences)
    case _ => false
  }
}
