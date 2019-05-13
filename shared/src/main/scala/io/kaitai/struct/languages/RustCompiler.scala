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
    with AllocateIOLocalVar
    with EveryReadIsExpression
    with FixedContentsUsingArrayByteLiteral
    with ObjectOrientedLanguage
    with SingleOutputFile
    with UpperCamelCaseClasses
    with UniversalFooter
    with UniversalDoc {

  import RustCompiler._

  override val translator: RustTranslator =
    new RustTranslator(typeProvider, config)

  override def innerClasses = false

  override def innerEnums = false

  override def indent: String = "    "

  override def outFileName(topClassName: String): String = s"$topClassName.rs"

  override def outImports(topClass: ClassSpec): String =
    importList.toList
      .map(i => s"#[allow(unused_imports)]\nuse $i;")
      .mkString("", "\n", "\n")

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"// $headerComment")
    outHeader.puts

    importList.add(
      "kaitai::{KError, KResult, KStream, KStruct, KStructUnit, TypedStack}"
    )
    importList.add("kaitai::{kf32_max, kf64_max, kf32_min, kf64_min}")
  }

  override def opaqueClassDeclaration(classSpec: ClassSpec): Unit =
    importList.add(
      s"crate::${classSpec.name.last}::${type2class(classSpec.name.last)}"
    )

  override def classHeader(name: List[String]): Unit = {
    out.puts
    out.puts("#[derive(Default, Debug)]")
    out.puts(s"pub struct ${classTypeName(typeProvider.nowClass)} {")
    out.inc

    // TODO: Remove shim `PhantomData` implementation
    if (containsReferences(typeProvider.nowClass))
      out.puts(s"_phantom: std::marker::PhantomData<&$streamLife ()>")
  }

  // Intentional no-op; Rust has already ended the struct definition by the time we reach this
  override def classFooter(name: List[String]): Unit = {}

  override def classConstructorHeader(name: List[String],
                                      parentType: DataType,
                                      rootClassName: List[String],
                                      isHybrid: Boolean,
                                      params: List[ParamDefSpec]): Unit = {

    // Unlike other OOP languages, implementing an interface happens outside the "class" definition.
    // End the definition here so we can start the `impl` block
    universalFooter

    out.puts(
      s"impl<$readLife, $streamLife: $readLife> $kstructName<$readLife, $streamLife> for ${classTypeName(typeProvider.nowClass)} {"
    )
    out.inc
    out.puts(s"type Root = ${rootClassTypeName(typeProvider.nowClass)};")
    out.puts(
      s"type ParentStack = ${parentStackTypeName(typeProvider.nowClass)};"
    )
    out.puts
  }

  override def universalFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def runRead(): Unit = out.puts(s"// runRead()")

  override def runReadCalc(): Unit = out.puts(s"// runReadCalc()")

  override def readHeader(endian: Option[FixedEndian],
                          isEmpty: Boolean): Unit = {
    out.puts(s"fn read<S: $kstreamName>(")
    out.inc
    out.puts(s"&mut self,")
    out.puts(s"${privateMemberName(IoIdentifier)}: &$streamLife S,")
    out.puts(
      s"${privateMemberName(RootIdentifier)}: Option<&$readLife Self::Root>,"
    )
    out.puts(
      s"${privateMemberName(ParentIdentifier)}: TypedStack<Self::ParentStack>"
    )
    out.dec
    out.puts(s") -> KResult<$streamLife, ()> {")
    out.inc

    // If there aren't any attributes to parse, we need to end the read implementation here
    if (typeProvider.nowClass.seq.isEmpty)
      endRead()
  }

  override def readFooter(): Unit = out.puts(s"// readFooter()")

  override def attributeDeclaration(attrName: Identifier,
                                    attrType: DataType,
                                    isNullable: Boolean): Unit =
    out.puts(s"// attributeDeclaration($attrName, $attrType, $isNullable)")

  // Intentional no-op; Rust handles ownership, so don't worry about reader methods
  override def attributeReader(attrName: Identifier,
                               attrType: DataType,
                               isNullable: Boolean): Unit = {}

  override def attrParse(attr: AttrLikeSpec,
                         id: Identifier,
                         defEndian: Option[Endianness]): Unit = {
    super.attrParse(attr, id, defEndian)

    // Detect if this is the last attribute parse and finish the read method
    if (typeProvider.nowClass.seq.nonEmpty && typeProvider.nowClass.seq.last.id == id)
      endRead()
  }

  def endRead(): Unit = {
    out.puts("Ok(())")
    out.dec
    out.puts("}")
  }

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit =
    out.puts(s"// attrParseHybrid(${leProc()}, ${beProc()})")

  override def condIfHeader(expr: Ast.expr): Unit = {
    // TODO: Actual implementation, this is a shim to enable compiling
    out.puts("{")
    out.inc

    out.puts(s"// condIfHeader($expr)")
  }

  override def condRepeatEosHeader(id: Identifier,
                                   io: String,
                                   dataType: DataType,
                                   needRaw: Boolean): Unit = {
    // TODO: Actual implementation, this is a shim to enable compiling
    out.puts("{")
    out.inc

    out.puts(s"// condRepeatEosHeader($id, $io, $dataType, $needRaw)")
  }

  override def condRepeatExprHeader(id: Identifier,
                                    io: String,
                                    dataType: DataType,
                                    needRaw: Boolean,
                                    repeatExpr: Ast.expr): Unit = {
    // TODO: Actual implementation, this is a shim to enable compiling
    out.puts("{")
    out.inc

    out.puts(
      s"// condRepeatExprHeader($id, $io, $dataType, $needRaw, $repeatExpr)"
    )
  }

  override def condRepeatUntilHeader(id: Identifier,
                                     io: String,
                                     dataType: DataType,
                                     needRaw: Boolean,
                                     repeatExpr: Ast.expr): Unit = {
    // TODO: Actual implementation, this is a shim to enable compiling
    out.puts("{")
    out.inc

    out.puts(
      s"// condRepeatUntilHeader($id, $io, $dataType, $needRaw, $repeatExpr)"
    )
  }

  override def condRepeatUntilFooter(id: Identifier,
                                     io: String,
                                     dataType: DataType,
                                     needRaw: Boolean,
                                     repeatExpr: Ast.expr): Unit = {
    out.puts(
      s"// condRepeatUntilFooter($id, $io, $dataType, $needRaw, $repeatExpr)"
    )
    out.dec
    out.puts("} {}")
  }

  override def attrProcess(proc: ProcessExpr,
                           varSrc: Identifier,
                           varDest: Identifier): Unit =
    out.puts(s"// attrProcess($proc, $varSrc, $varDest)")

  override def useIO(ioEx: Ast.expr): String = s"// useIO($ioEx)"

  override def pushPos(io: String): Unit = out.puts(s"// pushPos($io)")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"// seek($io, $pos)")

  override def popPos(io: String): Unit = out.puts(s"// popPos($io)")

  override def alignToByte(io: String): Unit = out.puts(s"// alignToByte($io)")

  override def instanceDeclHeader(className: List[String]): Unit = {
    out.puts(
      s"impl<$readLife, $streamLife: $readLife> ${classTypeName(typeProvider.nowClass)} {"
    )
    out.inc
  }

  override def instanceDeclFooter(className: List[String]): Unit =
    universalFooter

  override def instanceHeader(className: List[String],
                              instName: InstanceIdentifier,
                              dataType: DataType,
                              isNullable: Boolean): Unit = {

    out.puts(s"fn ${idToStr(instName)}<S: $kstreamName>(")
    out.inc
    out.puts(s"${privateMemberName(IoIdentifier)}: &$streamLife S,")
    out.puts(
      s"${privateMemberName(RootIdentifier)}: Option<&$readLife ${rootClassTypeName(typeProvider.nowClass)}>,"
    )
    out.puts(
      s"${privateMemberName(ParentIdentifier)}: TypedStack<${parentStackTypeName(typeProvider.nowClass)}>"
    )
    out.dec
    // TODO: Return actual instance type
    out.puts(s") -> KResult<$streamLife, ()> {")
    out.inc
  }

  override def privateMemberName(id: Identifier): String =
    RustCompiler.privateMemberName(id)

  override def idToStr(id: Identifier): String = RustCompiler.idToStr(id)

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier,
                                           dataType: DataType): Unit =
    out.puts(s"// instanceCheckCacheAndReturn($instName, $dataType)")

  override def instanceReturn(instName: InstanceIdentifier,
                              attrType: DataType): Unit = {
    out.puts("panic!(\"Instance calculation not yet supported.\");")
    out.puts(s"// instanceReturn($instName, $attrType)")
  }

  override def enumDeclaration(curClass: List[String],
                               enumName: String,
                               enumColl: Seq[(Long, EnumValueSpec)]): Unit =
    out.puts(s"// enumDeclaration($curClass, $enumName, $enumColl)")

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit =
    out.puts(s"// handleAssignmentRepeatEos($id, $expr)")

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit =
    out.puts(s"// handleAssignmentRepeatExpr($id, $expr)")

  override def handleAssignmentRepeatUntil(id: Identifier,
                                           expr: String,
                                           isRaw: Boolean): Unit =
    out.puts(s"// handleAssignmentRepeatUntil($id, $expr, $isRaw)")

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit =
    out.puts(s"// handleAssignmentSimple($id, $expr)")

  override def parseExpr(dataType: DataType,
                         assignType: DataType,
                         io: String,
                         defEndian: Option[FixedEndian]): String =
    s"// parseExpr($dataType, $assignType, $io, $defEndian)"

  override def bytesPadTermExpr(expr0: String,
                                padRight: Option[Int],
                                terminator: Option[Int],
                                include: Boolean): String =
    s"// bytesPadTermExpr($expr0, $padRight, $terminator, $include)"

  override def attrFixedContentsParse(attrName: Identifier,
                                      contents: String): Unit =
    out.puts(s"// attrFixedContentsParse($attrName, $contents)")

  override def publicMemberName(id: Identifier): String =
    s"// publicMemberName($id)"

  override def localTemporaryName(id: Identifier): String =
    s"// localTemporaryName($id)"

  override def universalDoc(doc: DocSpec): Unit = {
    out.puts(s"// universalDoc()")
  }

  override def switchStart(id: Identifier, on: Ast.expr): Unit =
    out.puts(s"// switchStart($id, $on)")

  override def switchCaseStart(condition: Ast.expr): Unit =
    out.puts(s"// switchCaseStart($condition)")

  override def switchCaseEnd(): Unit = out.puts(s"// switchCaseEnd()")

  override def switchElseStart(): Unit = out.puts(s"// switchElseStart()")

  override def switchEnd(): Unit = out.puts(s"// switchEnd()")

  override def extraAttrForIO(id: Identifier,
                              rep: RepeatSpec): List[AttrSpec] = {
    out.puts(s"// extraAttrForIO($id, $rep)")
    Nil
  }

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String =
    s"// allocateIO($varName, $rep)"
}

object RustCompiler
  extends LanguageCompilerStatic
    with StreamStructNames
    with UpperCamelCaseClasses {
  override def getCompiler(tp: ClassTypeProvider,
                           config: RuntimeConfig): LanguageCompiler =
    new RustCompiler(tp, config)

  override def kstructName = s"KStruct"

  def streamLife = "'s"

  def readLife = "'r"

  override def kstreamName = "KStream"

  def kstructUnitName = "KStructUnit"

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
    case RawIdentifier(inner) => s"raw_${idToStr(inner)}"
  }

  def rootClassTypeName(c: ClassSpec, isRecurse: Boolean = false): String = {
    if (!isRecurse && c.isTopLevel)
      "Self"
    else if (c.isTopLevel)
      classTypeName(c)
    else
      rootClassTypeName(c.upClass.get, isRecurse = true)
  }

  def parentStackTypeName(c: ClassSpec): String = {
    if (c.isTopLevel)
      s"($kstructUnitName)"
    else
      s"(${classTypeName(c.upClass.get)}, <${classTypeName(c.upClass.get)} as $kstructName<$readLife, $streamLife>>::ParentStack)"
  }

  def types2class(names: List[String]): String =
    names.map(x => type2class(x)).mkString("_")

  def classTypeName(c: ClassSpec): String =
    s"${types2class(c.name)}${lifetimeParam(c)}"

  def lifetimeParam(c: ClassSpec): String =
    if (containsReferences(c)) s"<$streamLife>" else ""

  def lifetimeParam(d: DataType): String =
    if (containsReferences(d)) s"<$streamLife>" else ""

  def containsReferences(c: ClassSpec): Boolean = containsReferences(c, Some(c))

  def containsReferences(d: DataType): Boolean = containsReferences(d, None)

  def containsReferences(c: ClassSpec, originating: Option[ClassSpec]): Boolean =
    c.seq.exists(t => containsReferences(t.dataType, originating)) ||
      c.instances.exists(i => containsReferences(i._2.dataTypeComposite, originating))

  def containsReferences(d: DataType, originating: Option[ClassSpec]): Boolean = d match {
    case _: BytesType | _: StrType => true
    case t: UserType =>
      t.classSpec match {
        // Recursive types may need references, but the recursion itself
        // will be handled by `Box<>`, so doesn't need a reference
        case Some(inner) if originating.contains(inner) => false
        case Some(inner) => containsReferences(inner, originating.orElse(Some(inner)))
        case None => false
      }
    case t: ArrayType => containsReferences(t.elType, originating)
    case st: SwitchType => st.cases.values.exists(t => containsReferences(t, originating))
    case _ => false
  }
}
