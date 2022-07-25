package io.kaitai.struct.languages

import io.kaitai.struct._
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
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
    with SwitchIfOps
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
      .map(i => s"use $i;")
      .mkString("", "\n", "\n")

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"// $headerComment")
    outHeader.puts

    outHeader.puts("#![allow(unused_imports)]")
    outHeader.puts("#![allow(non_snake_case)]")
    outHeader.puts("#![allow(non_camel_case_types)]")
    outHeader.puts("#![allow(irrefutable_let_patterns)]")
    outHeader.puts

    importList.add(
      "kaitai::*"
    )
    importList.add("std::convert::{TryFrom, TryInto}")
  }

  override def opaqueClassDeclaration(classSpec: ClassSpec): Unit =
    importList.add(
      s"crate::${classSpec.name.last}::${type2class(classSpec.name.last)}"
    )

  override def classHeader(name: List[String]): Unit = {
    out.puts
    out.puts("#[derive(Default, Debug, PartialEq)]")
    out.puts(s"pub struct ${classTypeName(typeProvider.nowClass)} {")
    out.inc

    // Because we can't predict whether opaque types will need lifetimes as a type parameter,
    // everyone gets a phantom data marker
    //out.puts(s"_phantom: std::marker::PhantomData<&$streamLife ()>,")

    typeProvider.nowClass.params.foreach { p =>
      // Make sure the parameter is imported if necessary
      p.dataType match {
        case u: UserType => if (u.isOpaque && u.classSpec.isDefined) opaqueClassDeclaration(u.classSpec.get)
        case _ => ()
      }

      // Declare parameters as if they were attributes
      attributeDeclaration(p.id, p.dataType, isNullable = false)
    }
  }

  // Intentional no-op; Rust has already ended the struct definition by the time we reach this
  override def classFooter(name: List[String]): Unit = {}

  override def classConstructorHeader(name: List[String],
                                      parentType: DataType,
                                      rootClassName: List[String],
                                      isHybrid: Boolean,
                                      params: List[ParamDefSpec]): Unit = {

    // Unlike other OOP languages, implementing an interface happens outside the struct declaration.
    universalFooter

    // If there are any switch types in the struct definition, create the enums for them
    typeProvider.nowClass.seq.foreach(
      a =>
        a.dataType match {
          case st: SwitchType => switchTypeEnum(a.id, st)
          case _ => ()
        }
    )
    typeProvider.nowClass.instances.foreach(
      i =>
        i._2.dataTypeComposite match {
          case st: SwitchType => switchTypeEnum(i._1, st)
          case _ => ()
        }
    )

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

  override def runRead(name: List[String]): Unit = out.puts(s"// runRead($name)")

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
    out.puts(s") -> KResult<()> {")
    out.inc

    // If there aren't any attributes to parse, we need to end the read implementation here
    if (typeProvider.nowClass.seq.isEmpty)
      endRead()
  }

  override def readFooter(): Unit = out.puts(s"// readFooter()")

  override def attributeDeclaration(attrName: Identifier,
                                    attrType: DataType,
                                    isNullable: Boolean): Unit = {
    val typeName = attrName match {
      // For keeping lifetimes simple, we don't store _io, _root, or _parent with the struct
      case IoIdentifier | RootIdentifier | ParentIdentifier => return
      case _ =>
        kaitaiTypeToNativeType(attrName, typeProvider.nowClass, attrType)
    }

    out.puts(s"pub ${idToStr(attrName)}: $typeName,")
  }

  override def attributeReader(attrName: Identifier,
                               attrType: DataType,
                               isNullable: Boolean): Unit = {
     val typeName = attrName match {
      // For keeping lifetimes simple, we don't store _io, _root, or _parent with the struct
      case IoIdentifier | RootIdentifier | ParentIdentifier => return
      case _ =>
        kaitaiTypeToNativeType(attrName, typeProvider.nowClass, attrType)
    }
    val typeNameEx = kaitaiTypeToNativeType(attrName, typeProvider.nowClass, attrType, excludeOptionWrapper = true)
    out.puts(
      s"impl<$readLife, $streamLife: $readLife> ${classTypeName(typeProvider.nowClass)} {")
    out.inc

    var types : Set[DataType] = Set()
    var enum_typename = false
    var simple_typename = attrType match {
      case _: UserType => false
      case _: BytesType => false
      case _: StrType => false
      case _: ArrayType => false
      case st: SwitchType => {
        types = st.cases.values.toSet
        enum_typename = true
        false
      }
      case _: EnumType => {
        // TODO? enum_typename = true
        false
      }
      case _ => true
    }
    var enum_only_numeric = true;
    types.foreach(t => {
      t match {
        case _: NumericType => // leave unchanged
        case _ => enum_only_numeric = false
      }
    })
    if (enum_typename && enum_only_numeric) {
      out.puts(s"pub fn ${idToStr(attrName)}(&self) -> usize {")
      out.inc
      out.puts(s"self.${idToStr(attrName)}.as_ref().unwrap().into()")
    } else {
      if (simple_typename) {
        out.puts(s"pub fn ${idToStr(attrName)}(&self) -> $typeNameEx {")
      } else {
        out.puts(s"pub fn ${idToStr(attrName)}(&self) -> &$typeNameEx {")
      }
      out.inc
      if (typeName != typeNameEx) {
        out.puts(s"self.${idToStr(attrName)}.as_ref().unwrap()")
      } else {
        if (simple_typename) {
          out.puts(s"self.${idToStr(attrName)}")
        } else {
          out.puts(s"&self.${idToStr(attrName)}")
        }
      }
    }
    out.dec
    out.puts("}")
    out.dec
    out.puts("}")
  }

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
    out.puts(s"if ${expression(expr)} {")
    out.inc
  }

  override def condRepeatCommonInit(id: Identifier, dataType: DataType, needRaw: NeedRaw): Unit = {
    out.puts(s"${privateMemberName(id)} = Vec::new();")
  }

  override def condRepeatEosHeader(id: Identifier,
                                   io: String,
                                   dataType: DataType): Unit = {
    out.puts("{")
    out.inc
    out.puts(s"while !_io.is_eof() {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}.push($expr);");
  }

  override def condRepeatEosFooter: Unit = {
    out.dec
    out.puts("}")
    out.dec
    out.puts("}")
  }

  override def condRepeatExprHeader(id: Identifier,
                                    io: String,
                                    dataType: DataType,
                                    repeatExpr: Ast.expr): Unit = {
    val lenVar = s"l_${idToStr(id)}"
    out.puts(s"let $lenVar = ${expression(repeatExpr)};")
    out.puts(s"for _i in 0..$lenVar {")
    out.inc
  }

  override def condRepeatUntilHeader(id: Identifier,
                                     io: String,
                                     dataType: DataType,
                                     repeatExpr: Ast.expr): Unit = {
    // TODO: Actual implementation, this is a shim to enable compiling
    out.puts("{")
    out.inc

    out.puts(
      s"// condRepeatUntilHeader($id, $io, $dataType, $repeatExpr)"
    )
  }

  override def condRepeatUntilFooter(id: Identifier,
                                     io: String,
                                     dataType: DataType,
                                     repeatExpr: Ast.expr): Unit = {
    out.puts(
      s"// condRepeatUntilFooter($id, $io, $dataType, $repeatExpr)"
    )
    out.dec
    out.puts("} {}")
  }

  def getRawIdExpr(varName: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(varName)
    rep match {
      case NoRepeat => memberName
      case _ => s"// TODO $memberName->at($memberName->size() - 1)"
    }
  }

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec): Unit = {
    val srcExpr = getRawIdExpr(varSrc, rep)

    val expr = proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "process_xor_one"
          case _: BytesType => "process_xor_many"
        }
        s"S::$procName(&$srcExpr, ${expression(xorValue)})"
      case ProcessZlib =>
        s"S::process_zlib(&$srcExpr)"
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        s"S::process_rotate_left(&$srcExpr, $expr)"
      case ProcessCustom(name, args) =>
        val procClass = name.map((x) => type2class(x)).mkString("::")
        val procName = s"_process_${idToStr(varSrc)}"

        //importListSrc.addLocal(outFileNameHeader(name.last))

        val argList = args.map(expression).mkString(", ")
        var argListInParens = if (argList.nonEmpty) s"($argList)" else ""
        out.puts(s"$procClass $procName$argListInParens;")
        s"$procName.decode(&$srcExpr)"
    }
    handleAssignment(varDest, expr, rep, false)
  }

  override def useIO(ioEx: Ast.expr): String = s"// useIO($ioEx)"

  override def pushPos(io: String): Unit = out.puts(s"// pushPos($io)")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"// seek($io, $pos)")

  override def popPos(io: String): Unit = out.puts(s"// popPos($io)")

  override def alignToByte(io: String): Unit =
    out.puts(s"${privateMemberName(IoIdentifier)}.align_to_byte()?;")

  override def privateMemberName(id: Identifier): String =
    RustCompiler.privateMemberName(id)

  override def instanceDeclHeader(className: List[String]): Unit = {
    out.puts(
      s"impl<$readLife, $streamLife: $readLife> ${classTypeName(typeProvider.nowClass)} {"
    )
    out.inc
  }

  override def universalFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def instanceDeclaration(attrName: InstanceIdentifier,
                                   attrType: DataType,
                                   isNullable: Boolean): Unit = {
    val typeName = kaitaiTypeToNativeType(
      attrName,
      typeProvider.nowClass,
      attrType,
      excludeOptionWrapper = true
    )
    attrType match {
      case _: ArrayType => out.puts(s"pub ${idToStr(attrName)}: $typeName,")
      case _: UserType => // do nothing (instance return &UserType)
      case _ => out.puts(s"pub ${idToStr(attrName)}: Option<$typeName>,")
    }
  }

  override def idToStr(id: Identifier): String = RustCompiler.idToStr(id)

  override def instanceHeader(className: List[String],
                              instName: InstanceIdentifier,
                              dataType: DataType,
                              isNullable: Boolean): Unit = {

    out.puts(s"pub fn ${idToStr(instName)}<S: $kstreamName>(")
    out.inc
    out.puts("&mut self,")
    out.puts(s"${privateMemberName(IoIdentifier)}: &$streamLife S,")
    out.puts(
      s"${privateMemberName(RootIdentifier)}: Option<&$readLife ${rootClassTypeName(typeProvider.nowClass)}>,"
    )
    out.puts(
      s"${privateMemberName(ParentIdentifier)}: TypedStack<${parentStackTypeName(typeProvider.nowClass)}>"
    )
    out.dec
    val typeName = kaitaiTypeToNativeType(
      instName,
      typeProvider.nowClass,
      dataType,
      excludeOptionWrapper = true
    )
    out.puts(s") -> KResult<&$typeName> {")
    out.inc
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier,
                                           dataType: DataType): Unit = {
    val userType = dataType match {
      case _: UserType => true
      case _ => false
    }
    if (userType) {
      return
    }
    out.puts(s"if ${privateMemberName(instName)}.is_some() {")
    out.inc
    instanceReturn(instName, dataType)
    out.dec
    out.puts("}")
  }

  override def instanceCalculate(instName: Identifier, dataType: DataType, value: Ast.expr): Unit = {
    dataType match {
      case _: UserType => {
        out.puts(s"Ok(${expression(value)})")
      }
      case _: StrType => {
        out.puts(s"${privateMemberName(instName)} = Some(${expression(value)}.to_string());")
      }
      case _: BytesType => {
        out.puts(s"let _t = ${expression(value)};")
        out.puts(s"${privateMemberName(instName)} = Some(_t.to_vec());")
      }
      case _ => {
        val primType = kaitaiPrimitiveToNativeType(dataType)
        out.puts(s"${privateMemberName(instName)} = Some(${expression(value)} as $primType);")
      }
    }
  }

  override def instanceReturn(instName: InstanceIdentifier,
                              attrType: DataType): Unit = {
    val userType = attrType match {
      case _: UserType => true
      case _ => false
    }
    if (!userType) {
      out.puts(s"return Ok(${privateMemberName(instName)}.as_ref().unwrap());")
    }
  }

  override def enumDeclaration(curClass: List[String],
                               enumName: String,
                               enumColl: Seq[(Long, EnumValueSpec)]): Unit = {

    val enumClass = types2class(curClass ::: List(enumName))

    // Set up the actual enum definition
    out.puts(s"#[derive(Debug, PartialEq)]")
    out.puts(s"pub enum $enumClass {")
    out.inc

    enumColl.foreach {
      case (_, label) =>
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
    out.puts(s"type Error = KError;")
    out.puts(s"fn try_from(flag: i64) -> KResult<$enumClass> {")

    out.inc
    out.puts(s"match flag {")

    out.inc
    enumColl.foreach {
      case (value, label) =>
        out.puts(s"$value => Ok($enumClass::${type2class(label.name)}),")
    }
    out.puts("_ => Err(KError::UnknownVariant(flag)),")
    out.dec

    out.puts("}")
    out.dec
    out.puts("}")
    out.dec
    out.puts("}")
    out.puts
  }

  override def universalDoc(doc: DocSpec): Unit = {
    out.puts
    out.puts( "/**")

    doc.summary.foreach(docStr => out.putsLines(" * ", docStr))

    doc.ref.foreach {
      case TextRef(text) =>
        out.putsLines(" * ", s"\\sa $text")
      case UrlRef(url, text) =>
        out.putsLines(" * ", s"\\sa $url $text")
    }

    out.puts( " */")
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit =
    handleAssignmentRepeatEos(id, expr)

  override def handleAssignmentRepeatUntil(id: Identifier,
                                           expr: String,
                                           isRaw: Boolean): Unit =
    out.puts(s"// handleAssignmentRepeatUntil($id, $expr, $isRaw)")

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    val seqId = typeProvider.nowClass.seq.find(s => s.id == id)
    var done = false;
    if (seqId.isDefined) seqId.get.dataType match {
      case et: EnumType =>
        done = true;
        out.puts(
          s"${privateMemberName(id)} = Some(($expr as i64).try_into()?);"
        )
      case st: SwitchType =>
        done = true;
        out.puts(s"${privateMemberName(id)} = Some($expr);")
      case _ => done = false;
    }
    if (!done) {
      out.puts(s"${privateMemberName(id)} = $expr;")
    }
  }

  override def parseExpr(dataType: DataType,
                         assignType: DataType,
                         io: String,
                         defEndian: Option[FixedEndian]): String =
    dataType match {
      case IntMultiType(_, _, None) => "panic!(\"Unable to parse unknown-endian integers\")"
      case t: ReadableType => s"$io.read_${t.apiCall(defEndian)}()?.into()"
      case _: BytesEosType => s"$io.read_bytes_full()?.into()"
      case b: BytesTerminatedType =>
          s"$io.read_bytes_term(${b.terminator}, ${b.include}, ${b.consume}, ${b.eosError})?.into()"
      case b: BytesLimitType => s"$io.read_bytes(${expression(b.size)} as usize)?.into()"
      case BitsType1(bitEndian) => s"$io.read_bits_int(1)? != 0"
      case BitsType(width, bitEndian) => s"$io.read_bits_int($width)?"
      case t: UserType =>
        val addParams = Utils.join(t.args.map((a) => translator.translate(a)), "", ", ", ", ")
        val userType = t match {
          case t: UserType =>
            val baseName = t.classSpec match {
              case Some(spec) => types2class(spec.name)
              case None => types2class(t.name)
            }
            s"$baseName"
        }
        val addArgs = if (t.isOpaque) {
          ""
        } else {
          val currentType = classTypeName(typeProvider.nowClass);
          val parent = t.forcedParent match {
            case Some(USER_TYPE_NO_PARENT) => "KStructUnit::parent_stack()"
            case Some(fp) => translator.translate(fp)
            case None => {
              if (userType contains currentType) {
                s"${privateMemberName(ParentIdentifier)}.push(self)"
              } else {
                s"${privateMemberName(ParentIdentifier)}"
              }
            }
          }
          val addEndian = t.classSpec.get.meta.endian match {
            case Some(InheritedEndian) => s", ${privateMemberName(EndianIdentifier)}"
            case _ => ""
          }
          s", ${privateMemberName(RootIdentifier)}, $parent$addEndian"
        }
        val streamType = if (io == privateMemberName(IoIdentifier)) {
          "S"
        } else {
          "BytesReader"
        }
        s"Self::read_into::<$streamType, $userType>($addParams$io$addArgs)?.into()"
      case _ => s"// parseExpr($dataType, $assignType, $io, $defEndian)"
    }

  override def bytesPadTermExpr(expr0: String,
                                padRight: Option[Int],
                                terminator: Option[Int],
                                include: Boolean): String = {
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

  override def attrFixedContentsParse(attrName: Identifier,
                                      contents: String): Unit =
    out.puts(s"// attrFixedContentsParse($attrName, $contents)")

  override def publicMemberName(id: Identifier): String =
    s"// publicMemberName($id)"

  override def localTemporaryName(id: Identifier): String =
    s"_t_${idToStr(id)}"

  override def switchRequiresIfs(onType: DataType): Boolean = onType match {
    case _: IntType | _: EnumType => false
    case _ => true
  }

  override def switchStart(id: Identifier, on: Ast.expr): Unit = {
    switch_else_exist = false
    out.puts(s"match ${expression(on)} {")
    out.inc
  }

  override def switchCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"${expression(condition)} => {")
    out.inc
  }

  override def switchCaseEnd(): Unit = {
    out.dec
    out.puts("}")
  }

  var switch_else_exist = false

  override def switchElseStart(): Unit = {
    switch_else_exist = true
    out.puts("_ => {")
    out.inc
  }

  override def switchEnd(): Unit = {
    if (!switch_else_exist) {
      out.puts("_ => {}")
    }
    out.dec
    out.puts("}")
  }

  override def switchIfStart(id: Identifier, on: Ast.expr, onType: DataType): Unit = {
    out.puts("{")
    out.inc
    out.puts(s"let on = ${expression(on)};")
  }

  override def switchIfCaseFirstStart(condition: Ast.expr): Unit = {
    out.puts(s"if on == ${expression(condition)} {")
    out.inc
  }

  override def switchIfCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"else if on == ${expression(condition)} {")
    out.inc
  }

  override def switchIfCaseEnd(): Unit = {
    out.dec
    out.puts("}")
  }

  override def switchIfElseStart(): Unit = {
    out.puts("else {")
    out.inc
  }

  override def switchIfEnd(): Unit = {
    out.dec
    out.puts("}")
  }

  override def allocateIO(id: Identifier, rep: RepeatSpec): String = {//= privateMemberName(IoIdentifier)
    val memberName = privateMemberName(id)
    val ioId = IoStorageIdentifier(id)

    val args = rep match {
      case RepeatUntil(_) => translator.doName(Identifier.ITERATOR2)
      case _ => privateMemberName(id)
    }

    val newStreamRaw = s"${memberName}"
    val ioName = rep match {
      case NoRepeat =>
        val newStream = newStreamRaw
        val localIO = localTemporaryName(ioId)
        out.puts(s"let ${localIO} = BytesReader::new(&$newStream);")
        s"&$localIO"
      case _ =>
        newStreamRaw
        // TODO
        //val localIO = s"io_${idToStr(id)}"
        // out.puts(s"$kstreamName* $localIO = $newStreamRaw;")
        // out.puts(s"${privateMemberName(ioId)}->push_back($localIO);")
        //localIO
    }

    ioName
  }

  def switchTypeEnum(id: Identifier, st: SwitchType): Unit = {
    // Because Rust can't handle `AnyType` in the type hierarchy,
    // we generate an enum with all possible variations
    val enum_typeName = kaitaiTypeToNativeType(
      id,
      typeProvider.nowClass,
      st,
      excludeOptionWrapper = true
    )
    out.puts("#[derive(Debug, PartialEq)]")
    out.puts(s"pub enum $enum_typeName {")
    out.inc

    val types = st.cases.values.toSet
    types.foreach(t => {
      // Because this switch type will itself be in an option, we can exclude it from user types
      val variantName = switchVariantName(id, t)
      val typeName = kaitaiTypeToNativeType(
        id,
        typeProvider.nowClass,
        t,
        excludeOptionWrapper = true
      )
      out.puts(s"$variantName($typeName),")
    })

    out.dec
    out.puts("}")

    var enum_only_numeric = true;
    types.foreach(t => {
      t match {
        case _: NumericType => // leave true
        case _ => enum_only_numeric = false
      }
    })

    // add helper methods From
    types.foreach(t => {
      // Because this switch type will itself be in an option, we can exclude it from user types
      val variantName = switchVariantName(id, t)
      var v = "v"
      val typeName = t match {
        case _ : BytesType => {
          v = "v.to_vec()"
          s"&[u8]" // special case for Bytes(Vec[u8]) (else switch)
        }
        case _ => kaitaiTypeToNativeType(
            id,
            typeProvider.nowClass,
            t,
            excludeOptionWrapper = true)
      }
      out.puts(s"impl From<$typeName> for $enum_typeName {")
      out.inc
      out.puts(s"fn from(v: $typeName) -> Self {")
      out.inc
      out.puts(s"Self::$variantName($v)")
      out.dec
      out.puts("}")
      out.dec
      out.puts("}")
      if (enum_only_numeric) {
        out.puts(s"impl From<&$enum_typeName> for $typeName {")
        out.inc
        out.puts(s"fn from(e: &$enum_typeName) -> Self {")
        out.inc
        out.puts(s"if let $enum_typeName::$variantName(v) = e {")
        out.inc
        out.puts(s"return *v")
        out.dec
        out.puts("}")
        out.puts(s"""panic!(\"trying to convert from enum $enum_typeName::$variantName to $typeName, enum value {:?}\", e)""")
        out.dec
        out.puts("}")
        out.dec
        out.puts("}")
      }
    })
    if (enum_only_numeric) {
      out.puts(s"impl From<&$enum_typeName> for usize {")
      out.inc
      out.puts(s"fn from(e: &$enum_typeName) -> Self {")
      out.inc
      out.puts(s"match e {")
      out.inc
      types.foreach(t => {
        val variantName = switchVariantName(id, t)
        out.puts(s"$enum_typeName::$variantName(v) => *v as usize,")
      })
      out.dec
      out.puts("}")
      out.dec
      out.puts("}")
      out.dec
      out.puts("}")
      out.puts
    }
  }

  def switchVariantName(id: Identifier, attrType: DataType): String =
    attrType match {
      // TODO: Not exhaustive
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

      case BitsType(_,_) => "Bits"
      case _: BooleanType => "Boolean"
      case CalcIntType => "Int"
      case CalcFloatType => "Float"
      case _: StrType => "String"
      case _: BytesType => "Bytes"

      case t: UserType =>
        kaitaiTypeToNativeType(
          id,
          typeProvider.nowClass,
          t,
          excludeOptionWrapper = true,
          excludeLifetime = true,
          excludeBox = true
        )
      case t: EnumType =>
        kaitaiTypeToNativeType(
          id,
          typeProvider.nowClass,
          t,
          excludeOptionWrapper = true
        )
      case t: ArrayType => s"Arr${switchVariantName(id, t.elType)}"
    }

  override def ksErrorName(err: io.kaitai.struct.datatype.KSError): String =
    s"KaitaiStream.$err"
}

object RustCompiler
  extends LanguageCompilerStatic
    with StreamStructNames
    with UpperCamelCaseClasses {
  override def getCompiler(tp: ClassTypeProvider,
                           config: RuntimeConfig): LanguageCompiler =
    new RustCompiler(tp, config)

  override def kstreamName = "KStream"

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
    case IoStorageIdentifier(inner) => s"io_${idToStr(inner)}"
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
      s"$kstructUnitName"
    else
      s"(&$readLife ${classTypeName(c.upClass.get)}, <${classTypeName(c.upClass.get)} as $kstructName<$readLife, $streamLife>>::ParentStack)"
  }

  override def kstructName = s"KStruct"

  def readLife = "'r"

  def kstructUnitName = "KStructUnit"

  def classTypeName(c: ClassSpec): String =
    s"${types2class(c.name)}"

  def streamLife = "'s"

  def types2class(names: List[String]): String =
  // TODO: Use `mod` to scope types instead of weird names
    names.map(x => type2class(x)).mkString("_")

  def lifetimeParam(d: DataType): String =
    if (containsReferences(d)) s"<$streamLife>" else ""

  def containsReferences(d: DataType): Boolean = containsReferences(d, None)

  def containsReferences(c: ClassSpec,
                         originating: Option[ClassSpec]): Boolean =
    c.seq.exists(t => containsReferences(t.dataType, originating)) ||
      c.instances.exists(
        i => containsReferences(i._2.dataTypeComposite, originating)
      )

  def containsReferences(d: DataType, originating: Option[ClassSpec]): Boolean =
    d match {
      case _: BytesType | _: StrType => true
      case t: UserType => true
      /*
      t.classSpec match {
        // Recursive types may need references, but the recursion itself
        // will be handled by `Box<>`, so doesn't need a reference
        case Some(inner) if originating.contains(inner) => false
        case Some(inner) => containsReferences(inner, originating.orElse(Some(inner)))
        case None => false
      }
       */
      case t: ArrayType => containsReferences(t.elType, originating)
      case st: SwitchType =>
        st.cases.values.exists(t => containsReferences(t, originating))
      case _ => false
    }

  def kaitaiTypeToNativeType(id: Identifier,
                             cs: ClassSpec,
                             attrType: DataType,
                             excludeOptionWrapper: Boolean = false,
                             excludeLifetime: Boolean = false,
                             excludeBox: Boolean = false): String =
    attrType match {
      // TODO: Not exhaustive
      case _: NumericType => kaitaiPrimitiveToNativeType(attrType)
      case _: BooleanType => kaitaiPrimitiveToNativeType(attrType)
      case _: StrType => kaitaiPrimitiveToNativeType(attrType)
      case _: BytesType => kaitaiPrimitiveToNativeType(attrType)

      case t: UserType =>
        val baseName = t.classSpec match {
          case Some(spec) => types2class(spec.name)
          case None => types2class(t.name)
        }
        //val lifetime = if (!excludeLifetime) s"<$streamLife>" else ""

        // Because we can't predict if opaque types will recurse, we have to box them
        val typeName =
          if (!excludeBox && t.isOpaque) s"Box<$baseName>"
          else s"$baseName"
        if (excludeOptionWrapper) typeName else s"Option<$typeName>"

      case t: EnumType =>
        val typeName = t.enumSpec match {
          case Some(spec) => s"${types2class(spec.name)}"
          case None => s"${types2class(t.name)}"
        }
        if (excludeOptionWrapper) typeName else s"Option<$typeName>"

      case t: ArrayType =>
        s"Vec<${kaitaiTypeToNativeType(id, cs, t.elType, excludeOptionWrapper = true, excludeLifetime = excludeLifetime)}>"

      case st: SwitchType =>
        // val types = st.cases.values.toSet
        // val lifetime =
        //   if (!excludeLifetime && types.exists(containsReferences))
        //     s"<$streamLife>"
        //   else ""
        val typeName = id match {
          case name: NamedIdentifier =>
            s"${types2class(cs.name ::: List(name.name))}"
          case name: InstanceIdentifier =>
            s"${types2class(cs.name ::: List(name.name))}"
          case _ => kstructUnitName
        }

        if (excludeOptionWrapper) typeName else s"Option<$typeName>"

      case KaitaiStreamType => kstreamName
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

    case BitsType(_,_) => "u64"

    case _: BooleanType => "bool"
    case CalcIntType => "i32"
    case CalcFloatType => "f64"

    case _: StrType => s"String"
    case _: BytesType => s"Vec<u8>"
  }
}
