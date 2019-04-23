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
    with ObjectOrientedLanguage
    with UpperCamelCaseClasses
    with SingleOutputFile
    with UniversalFooter
    with UniversalDoc {

  import RustCompiler._

  override val translator = new RustTranslator(typeProvider, config)

  override def indent: String = "    "

  override def outFileName(topClassName: String): String = s"$topClassName.rs"

  override def outImports(topClass: ClassSpec): String =
    // TODO: #![allow(unused_imports)] instead?
    importList.toList.map(i => s"#[allow(unused_imports)]\nuse $i;").mkString("", "\n", "\n")

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"// $headerComment")
    outHeader.puts

    // Runtime-required imports
    importList.add("kaitai::{self, KError, KResult, KStream, KStruct}")
    importList.add("std::convert::TryFrom")
    importList.add("std::vec::Vec")
  }

  override def classHeader(name: List[String]): Unit = {
    // Set up the struct definition

    // TODO: Derive Clone/PartialEq/Debug?
    out.puts(s"#[derive(Default, Debug)]")
    out.puts(s"struct ${normalizeClassName(name)}<'a> {")

    out.inc
  }

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    val typeName = attrName match {
      case IoIdentifier => return // No-op, the stream doesn't get stored inside the struct
      case RootIdentifier =>
        val rootClass = if (typeProvider.nowClass.isTopLevel) "Self" else s"${normalizeClassName(typeProvider.topClass.name)}<'a>"
        s"Option<&'a $rootClass>"
      case ParentIdentifier =>
        val parentClass = typeProvider.nowClass.parentClass match {
          case t: ClassSpec => normalizeClassName(t.name)
          case GenericStructClassSpec => kstructName
        }
        s"Option<&'a $parentClass<'a>>"
      case _ => kaitaiTypeToNativeType(attrType)
    }

    out.puts(s"${idToAccessModifier(attrName)} ${idToStr(attrName)}: $typeName,".trim)
  }

  override def instanceDeclaration(attrName: InstanceIdentifier, attrType: DataType, isNullable: Boolean): Unit = {
    // Instances are different than attributes because we cache the values and re-use
    val typeName = kaitaiTypeToNativeType(attrType)
    val finalType = s"Option<$typeName>"
    out.puts(s"${idToAccessModifier(attrName)} ${idToStr(attrName)}: $finalType,".trim)
  }

  def idToAccessModifier(id: Identifier): String = id match {
    case RootIdentifier | ParentIdentifier | InstanceIdentifier(_) | RawIdentifier(_) => ""
    case _ => "pub"
  }

  // Intentional no-op; Called when all sub-types have finished, which is too late
  override def classFooter(name: List[String]): Unit = {}

  override def classConstructorHeader(name: List[String], parentType: DataType, rootClassName: List[String],
                                      isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    // Normally OO languages attempt to put classes within each other; Rust can't do that, so we end the struct
    // definition here and start the impl block
    out.dec
    out.puts("}")

    // Begin impl block
    out.puts(s"// classConstructorHeader($name, $parentType, $rootClassName, $isHybrid, $params)")
  }

  override def runRead(): Unit = out.puts("// runRead()")

  override def runReadCalc(): Unit = out.puts("// runReadCalc()")

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = out.puts(s"// readHeader($endian, $isEmpty)")

  override def readFooter(): Unit = out.puts(s"// readFooter()")

  // Intentional no-op; Rust handles ownership, so we can use struct attributes directly without readers
  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {}

  override def attrParse(attr: AttrLikeSpec, id: Identifier, defEndian: Option[Endianness]): Unit =
    out.puts(s"// attrParse($attr, $id, $defEndian)")

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit =
    out.puts(s"// attrParseHybrid(${leProc()}, ${beProc()})")

  override def attrFixedContentsParse(attrName: Identifier, contents: Array[Byte]): Unit =
    out.puts(s"// attrFixedContentsParse($attrName, $contents)")

  override def condIfHeader(expr: Ast.expr): Unit =
    out.puts(s"// condIfHeader($expr)")

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean): Unit =
    out.puts(s"// condRepeatEosHeader($id, $io, $dataType, $needRaw)")

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: Ast.expr): Unit =
    out.puts(s"// condRepeatExprHeader($id, $io, $dataType, $needRaw, $repeatExpr)")

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: Ast.expr): Unit =
    out.puts(s"// condRepeatUntilHeader($id, $io, $dataType, $needRaw, $repeatExpr)")

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: Ast.expr): Unit =
    out.puts(s"// condRepeatUntilFooter($id, $io, $dataType, $needRaw, $repeatExpr)")

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit =
    out.puts(s"// attrProcess($proc, $varSrc, $varDest)")

  override def useIO(ioEx: Ast.expr): String = s"// useIO($ioEx)"

  override def pushPos(io: String): Unit = out.puts(s"// pushPos($io)")

  override def seek(io: String, pos: Ast.expr): Unit = out.puts(s"// seek($io, $pos)")

  override def popPos(io: String): Unit = out.puts(s"// popPos($io)")

  override def alignToByte(io: String): Unit = out.puts(s"// alignToByte($io)")

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit =
    out.puts(s"// instanceHeader($className, $instName, $dataType, $isNullable)")

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit =
    out.puts(s"// instanceCheckCacheAndReturn($instName, $dataType)")

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit =
    out.puts(s"// instanceReturn($instName, $attrType)")

  override def instanceCalculate(instName: Identifier, dataType: DataType, value: Ast.expr): Unit =
    out.puts(s"// instanceCalculate($instName, $dataType, $value)")

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, EnumValueSpec)]): Unit = {
    val enumClass = normalizeClassName(curClass ::: List(enumName))

    // Set up the actual enum definition
    out.puts(s"#[derive(Debug)]")
    out.puts(s"enum $enumClass {")
    out.inc

    enumColl.foreach { case (_, label) =>
      if (label.doc.summary.isDefined)
        universalDoc(label.doc)

      out.puts(s"${type2class(label.name)},")
    }

    out.dec
    out.puts("}")

    // Set up parsing enums from the underlying value
    out.puts(s"impl TryFrom<u64> for $enumClass {")

    out.inc
    out.puts(s"type Error = KError<'static>;")
    out.puts(s"fn try_from(flag: u64) -> KResult<'static, $enumClass> {")

    out.inc
    out.puts(s"match flag {")

    out.inc
    enumColl.foreach { case (value, label) =>
      out.puts(s"$value => Ok($enumClass::${type2class(label.name)}),")
    }
    out.puts("_ => Err(KError::UnknownEnum(flag)),")
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
  override def idToStr(id: Identifier): String = id match {
    case SpecialIdentifier(n) => n
    case NamedIdentifier(n) => n
    case InstanceIdentifier(n) => n
    case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
    // Raw identifiers store bytes locally, we'll parse them later
    case RawIdentifier(inner) => s"raw_${idToStr(inner)}"
  }

  /**
    * Renders identifier as a proper reference to a private member
    * that represents this field. This might include some prefixes
    * like "@" or "this." or "self.".
    *
    * @param id identifier to render
    * @return identifier as string
    */
  override def privateMemberName(id: Identifier): String = s"// privateMemberName($id)"

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
  override def localTemporaryName(id: Identifier): String = s"// localTemporaryName($id)"

  /**
    * Single method that outputs all kind of footers in the language.
    */
  override def universalFooter: Unit = {
    out.puts("// universalFooter()")
  }

  override def universalDoc(doc: DocSpec): Unit = {
    out.puts
    doc.summary.foreach(out.putsLines("/// ", _))
  }

  override def switchStart(id: Identifier, on: Ast.expr): Unit = out.puts(s"// switchStart($id, $on)")

  override def switchCaseStart(condition: Ast.expr): Unit = out.puts(s"// switchCaseStart($condition)")

  override def switchCaseEnd(): Unit = out.puts(s"// switchCaseEnd()")

  override def switchElseStart(): Unit = out.puts(s"// switchElseStart()")

  override def switchEnd(): Unit = out.puts(s"// switchEnd()")

  override def extraAttrForIO(id: Identifier, rep: RepeatSpec): List[AttrSpec] = {
    out.puts(s"// extraAttrForIO($id, $rep)")
    Nil
  }

}

object RustCompiler extends LanguageCompilerStatic
  with UpperCamelCaseClasses
  with StreamStructNames {

  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new RustCompiler(tp, config)

  def normalizeClassName(names: List[String]): String = type2class(names.mkString("_"))

  def kaitaiTypeToNativeType(attrType: DataType, excludeOptionUser: Boolean = false): String = attrType match {
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

    case t: UserType =>
      val typeName = t.classSpec match {
        case Some(cs) => s"${normalizeClassName(cs.name)}<'a>"
        case None => s"${normalizeClassName(t.name)}<'a>"
      }
      if (excludeOptionUser)
        typeName
      else
        s"Option<&'a $typeName>"

    case t: EnumType => t.enumSpec match {
      case Some(cs) => s"Option<${normalizeClassName(cs.name)}>"
      case None => s"Option<${normalizeClassName(t.name)}>"
    }

    // TODO: Store references instead of owned? Potential issues with UserType ref vs. repeated EnumType ref
    case t: ArrayType => s"Vec<${kaitaiTypeToNativeType(t.elType, excludeOptionUser = true)}>"

    // TODO: Safer way of handling raw Kaitai types?
    case KaitaiStreamType => kstreamName
    case KaitaiStructType | CalcKaitaiStructType => kstructName

    case st: SwitchType => kaitaiTypeToNativeType(st.combinedType)
  }

  override def kstreamName: String = "KStream"
  override def kstructName: String = "KStruct"
}
