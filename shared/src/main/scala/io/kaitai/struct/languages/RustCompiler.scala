package io.kaitai.struct.languages

import io.kaitai.struct._
//import io.kaitai.struct.datatype.DataType.{ReadableType, _}
import io.kaitai.struct.datatype._
import io.kaitai.struct.datatype.DataType._
//import io.kaitai.struct.datatype.{DataType, FixedEndian, InheritedEndian, KSError}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.RustTranslator

import scala.annotation.tailrec

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
      .mkString("", "\n", "\n")

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"// $headerComment")
    outHeader.puts

    outHeader.puts("#![allow(unused_imports)]")
    outHeader.puts("#![allow(non_snake_case)]")
    outHeader.puts("#![allow(non_camel_case_types)]")
    outHeader.puts("#![allow(irrefutable_let_patterns)]")
    outHeader.puts("#![allow(unused_comparisons)]")
    outHeader.puts
    outHeader.puts("extern crate kaitai;")

    importList.add("use kaitai::*;")
    importList.add("use std::convert::{TryFrom, TryInto};")
    importList.add("use std::cell::{Ref, Cell, RefCell};")
    importList.add("use std::rc::{Rc, Weak};")
  }

  override def externalTypeDeclaration(extType: ExternalType): Unit =
    importList.add(
      s"use super::${extType.name.head}::${types2class(extType.name)};"
    )

  override def classHeader(name: List[String]): Unit = {
    out.puts
    out.puts("#[derive(Default, Debug, Clone)]")
    out.puts(s"pub struct ${classTypeName(typeProvider.nowClass)} {")
    out.inc

    val root = types2class(name.slice(0, 1))
    out.puts(s"pub ${privateMemberName(RootIdentifier)}: SharedType<$root>,")

    val parent = if (typeProvider.nowClass.isTopLevel)
      root
    else {
      kaitaiTypeToNativeType(None, typeProvider.nowClass, typeProvider.nowClass.parentType, cleanTypename = true)
    }
    out.puts(s"pub ${privateMemberName(ParentIdentifier)}: SharedType<$parent>,")
    out.puts(s"pub _self: SharedType<Self>,")

    typeProvider.nowClass.params.foreach { p =>
      // Make sure the parameter is imported if necessary
      p.dataType match {
        case u: UserType => if (u.isExternal(typeProvider.nowClass)) externalTypeDeclaration(ExternalUserType(u.classSpec.get))
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
    typeProvider.nowClass.meta.endian match {
      case Some(_: CalcEndian) | Some(InheritedEndian) =>
        attributeDeclaration(EndianIdentifier, IntMultiType(signed = true, Width4, None), isNullable = false)
      case _ =>
    }

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
      s"impl $kstructName for ${classTypeName(typeProvider.nowClass)} {"
    )
    out.inc
    val root = classTypeName(typeProvider.topClass)
    out.puts(s"type Root = $root;")

    val parent = if (typeProvider.nowClass.isTopLevel)
      root
    else {
      kaitaiTypeToNativeType(None, typeProvider.nowClass, typeProvider.nowClass.parentType, cleanTypename = true)
    }

    out.puts(
      s"type Parent = $parent;"
    )
    out.puts
  }

  override def runRead(name: List[String]): Unit = {}

  override def runReadCalc(): Unit = {
    out.puts(s"if *${privateMemberName(EndianIdentifier)} == 0 {")
    out.inc
    out.puts(s"""return Err(${ksErrorName(UndecidedEndiannessError)} { src_path: "${typeProvider.nowClass.path.mkString("/", "/", "")}".to_string() });""")
    out.dec
    out.puts("}")
  }

  override def readHeader(endian: Option[FixedEndian],
                          isEmpty: Boolean): Unit = {
    RustCompiler.in_reader = true
    val root = privateMemberName(RootIdentifier)
    out.puts(s"fn read<S: $kstreamName>(")
    out.inc
    out.puts(s"self_rc: &OptRc<Self>,")
    out.puts(s"${privateMemberName(IoIdentifier)}: &S,")
    out.puts(
      s"$root: SharedType<Self::Root>,"
    )
    out.puts(
      s"${privateMemberName(ParentIdentifier)}: SharedType<Self::Parent>,"
    )
    out.dec
    out.puts(s") -> KResult<()> {")
    out.inc

    out.puts(s"*self_rc._io.borrow_mut() = _io.clone();")
    out.puts(s"self_rc._root.set(_root.get());")
    out.puts(s"self_rc._parent.set(_parent.get());")
    out.puts(s"self_rc._self.set(Ok(self_rc.clone()));")

    out.puts(s"let _rrc = self_rc._root.get_value().borrow().upgrade();")
    out.puts(s"let _prc = self_rc._parent.get_value().borrow().upgrade();")
    out.puts(s"let _r = _rrc.as_ref().unwrap();")

    // If there aren't any attributes to parse, we need to end the read implementation here
    if (typeProvider.nowClass.seq.isEmpty)
      endRead()
  }

  override def readFooter(): Unit = out.puts(s"// readFooter()")

  override def attributeDeclaration(attrName: Identifier,
                                    attrType: DataType,
                                    isNullable: Boolean): Unit = {
    val typeName = attrName match {
      case RootIdentifier | ParentIdentifier => return
      case _ =>
        kaitaiTypeToNativeType(Some(attrName), typeProvider.nowClass, attrType)
    }

    out.puts(s"${idToStr(attrName)}: RefCell<$typeName>,")
  }

  override def attributeReader(attrName: Identifier,
                               attrType: DataType,
                               isNullable: Boolean): Unit = {
    var typeName = attrName match {
        case RootIdentifier | ParentIdentifier => return
        case _ =>
          kaitaiTypeToNativeType(Some(attrName), typeProvider.nowClass, attrType)
    }

    out.puts(
      s"impl ${classTypeName(typeProvider.nowClass)} {")
    out.inc

    var types : Set[DataType] = Set()
    var enum_typename = false
    var switch_typename = false
    attrType match {
      case st: SwitchType =>
        types = st.cases.values.toSet
        switch_typename = true
      case _: EnumType => enum_typename = true
      case _ =>
    }
    var enum_only_numeric = true
    types.foreach {
      case _: NumericType => // leave unchanged
      case _ => enum_only_numeric = false
    }
    var fn = idToStr(attrName)
    if (switch_typename && enum_only_numeric) {
      out.puts(s"pub fn $fn(&self) -> usize {")
      out.inc
      out.puts(s"self.${idToStr(attrName)}.borrow().as_ref().unwrap().into()")
      out.dec
      out.puts("}")
      fn = s"${fn}_enum"
    }
    {
      out.puts(s"pub fn $fn(&self) -> Ref<$typeName> {")
      out.inc
      out.puts(s"self.${idToStr(attrName)}.borrow()")
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
    RustCompiler.in_reader = false
  }

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {}

  override def condIfHeader(expr: Ast.expr): Unit = {
    out.puts(s"if ${expression(expr)} {")
    out.inc
  }

   override def condRepeatInitAttr(id: Identifier, dataType: DataType): Unit = {
    // this line required for handleAssignmentRepeatUntil
    typeProvider._currentIteratorType = Some(dataType)
    out.puts(s"*${RustCompiler.privateMemberName(id, writeAccess = true)} = Vec::new();")
  }

  override def condRepeatEosHeader(id: Identifier,
                                   io: String,
                                   dataType: DataType): Unit = {
    out.puts("{")
    out.inc
    out.puts(s"let mut _i = 0;")
    out.puts(s"while !_io.is_eof() {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    out.puts(s"${RustCompiler.privateMemberName(id, writeAccess = true)}.push($expr);")
  }

  override def condRepeatEosFooter: Unit = {
    out.puts("_i += 1;")
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
    out.puts("{")
    out.inc
    out.puts("let mut _i = 0;")
    out.puts("while {")
    out.inc
  }

  override def createSubstream(id: Identifier, byteType: BytesType, io: String, rep: RepeatSpec, defEndian: Option[FixedEndian]): String = {
    createSubstreamBuffered(id, byteType, io, rep, defEndian)
  }

  override def handleAssignmentRepeatUntil(id: Identifier,
                                           expr: String,
                                           isRaw: Boolean): Unit = {
    out.puts(s"${RustCompiler.privateMemberName(id, writeAccess = true)}.push($expr);")
    var copy_type = ""
    if (typeProvider._currentIteratorType.isDefined && translator.is_copy_type(typeProvider._currentIteratorType.get)) {
      copy_type = "*"
    }
    val t = localTemporaryName(id)
    out.puts(s"let $t = ${privateMemberName(id)};")
    out.puts(s"let ${translator.doLocalName(Identifier.ITERATOR)} = $copy_type$t.last().unwrap();")
  }

  override def condRepeatUntilFooter(id: Identifier,
                                     io: String,
                                     dataType: DataType,
                                     repeatExpr: Ast.expr): Unit = {
    // this line required by kaitai code
    typeProvider._currentIteratorType = Some(dataType)
    out.puts("_i += 1;")
    out.puts(s"let x = !(${expression(repeatExpr)});")
    out.puts("x")
    out.dec
    out.puts("} {}")
    out.dec
    out.puts("}")
  }

  def getRawIdExpr(varName: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(varName)
    rep match {
      case NoRepeat => memberName
      case _ => s"$memberName[$memberName.len() - 1]"
    }
  }

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec): Unit = {
    val srcExpr = getRawIdExpr(varSrc, rep)

    val expr = proc match {
      case ProcessXor(xorValue) =>
        translator.detectType(xorValue) match {
          case _: IntType =>
            s"process_xor_one(&$srcExpr, ${expression(xorValue)})"
          case _: BytesType =>
            s"process_xor_many(&$srcExpr, &${translator.remove_deref(expression(xorValue))})"
        }
      case ProcessZlib =>
        s"process_zlib(&$srcExpr).map_err(|msg| KError::BytesDecodingError { msg })?"
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        s"process_rotate_left(&$srcExpr, $expr)"
      case ProcessCustom(name, args) =>
        val procClass = name.map(x => type2class(x)).mkString("::")
        val procName = s"_process_${idToStr(varSrc)}"

        val mod_name = name.last
        importList.add(s"use crate::$mod_name::*;")

        val argList = translate_args(args, into = false)
        val argListInParens = s"($argList)"
        out.puts(s"let $procName = $procClass::new$argListInParens;")
        s"$procName.decode(&$srcExpr).map_err(|msg| KError::BytesDecodingError { msg })?"
    }
    handleAssignment(varDest, expr, rep, isRaw = false)
  }

  override def useIO(ioEx: Ast.expr): String = {
    out.puts(s"let io = Clone::clone(&*${expression(ioEx)});")
    "io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"let _pos = $io.pos();")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io.seek(${expression(pos)} as usize)?;")

  override def popPos(io: String): Unit =
    out.puts(s"$io.seek(_pos)?;")

  override def alignToByte(io: String): Unit =
    out.puts(s"${privateMemberName(IoIdentifier)}.align_to_byte()?;")

  override def privateMemberName(id: Identifier): String =
    RustCompiler.privateMemberName(id)

  override def instanceDeclHeader(className: List[String]): Unit = {
    if (typeProvider.nowClass.params.nonEmpty) {
      val paramsArg = Utils.join(typeProvider.nowClass.params.map { p =>
        val n = paramName(p.id)
        val t = kaitaiTypeToNativeType(Some(p.id), typeProvider.nowClass, p.dataType, excludeOptionWrapper = true)
        var byref = ""
        if (!translator.is_copy_type(p.dataType))
          byref = "&"
        // generate param access helper
        attributeReader(p.id, p.dataType, isNullable = false)
        s"$n: $byref$t"
      }, "", ", ", "")

      out.puts(s"impl ${classTypeName(typeProvider.nowClass)} {")
      out.inc
      out.puts(s"pub fn set_params(&mut self, $paramsArg) {")
      out.inc
      typeProvider.nowClass.params.foreach(p => handleAssignmentParams(p.id, paramName(p.id)))
      out.dec
      out.puts("}")
      out.dec
      out.puts("}")
    }
    typeProvider.nowClass.meta.endian match {
      case Some(_: CalcEndian) | Some(InheritedEndian) =>
        out.puts(s"impl ${classTypeName(typeProvider.nowClass)} {")
        out.inc
        val t = kaitaiTypeToNativeType(Some(EndianIdentifier), typeProvider.nowClass, IntMultiType(signed = true, Width4, None), excludeOptionWrapper = true)
        out.puts(s"pub fn set_endian(&mut self, ${idToStr(EndianIdentifier)}: $t) {")
        out.inc
        handleAssignmentSimple(EndianIdentifier, s"${idToStr(EndianIdentifier)}")
        out.dec
        out.puts("}")
        out.dec
        out.puts("}")
      case _ =>
    }
    out.puts(s"impl ${classTypeName(typeProvider.nowClass)} {")
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
      Some(attrName),
      typeProvider.nowClass,
      attrType
    )
    out.puts(s"${calculatedFlagForName(attrName)}: Cell<bool>,")
    out.puts(s"${idToStr(attrName)}: RefCell<$typeName>,")
  }

  def calculatedFlagForName(ksName: Identifier) =
  s"f_${idToStr(ksName)}"

  override def instanceClear(instName: InstanceIdentifier): Unit = {
    var set = false
    val ins = translator.get_instance(typeProvider.nowClass, idToStr(instName))
    if (ins.isDefined) {
      set = ins.get.dataTypeComposite match {
        case _: UserType => true
        case _ => false
      }
    }
    if (!set) {
      out.puts(s"self.${calculatedFlagForName(instName)}.set(false);")
    }
  }

  override def instanceSetCalculated(instName: InstanceIdentifier): Unit = {
    var set = false
    val ins = translator.get_instance(typeProvider.nowClass, idToStr(instName))
    if (ins.isDefined) {
      set = ins.get.dataTypeComposite match {
        case _: UserType => true
        case _ => false
      }
    }
    if (!set) {
      out.puts(s"self.${calculatedFlagForName(instName)}.set(true);")
    }
  }

  override def idToStr(id: Identifier): String = RustCompiler.idToStr(id)

  override def instanceHeader(className: List[String],
                              instName: InstanceIdentifier,
                              dataType: DataType,
                              isNullable: Boolean): Unit = {
    out.puts(s"pub fn ${idToStr(instName)}(")
    out.inc
    out.puts("&self")
    out.dec
    val typeName = kaitaiTypeToNativeType(
      Some(instName),
      typeProvider.nowClass,
      dataType
    )
    out.puts(s") -> KResult<Ref<$typeName>> {")
    out.inc
    out.puts(s"let _io = self._io.borrow();")
    out.puts(s"let _rrc = self._root.get_value().borrow().upgrade();")
    out.puts(s"let _prc = self._parent.get_value().borrow().upgrade();")
    out.puts(s"let _r = _rrc.as_ref().unwrap();")
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier,
                                           dataType: DataType): Unit = {
    out.puts(s"if self.${calculatedFlagForName(instName)}.get() {")
    out.inc
    out.puts(s"return Ok(${privateMemberName(instName)});")
    out.dec
    out.puts("}")
  }

  override def instanceCalculate(instName: Identifier, dataType: DataType, value: Ast.expr): Unit = {
    dataType match {
      case _: UserType =>
        handleAssignmentSimple(instName, s"${translator.remove_deref(expression(value))}.clone()")
      case _: StrType =>
        handleAssignmentSimple(instName, s"${translator.remove_deref(expression(value))}.to_string()")
      case _: BytesType =>
        handleAssignmentSimple(instName, s"${translator.rem_vec_amp(translator.remove_deref(expression(value)))}.to_vec()")
      case _: ArrayType =>
        handleAssignmentSimple(instName, s"${translator.rem_vec_amp(translator.remove_deref(expression(value)))}.to_vec()")
      case _: EnumType =>
        handleAssignmentSimple(instName, s"${translator.remove_deref(expression(value))}")
      case _ =>
        handleAssignmentSimple(instName, s"(${expression(value)}) as ${kaitaiPrimitiveToNativeType(dataType)}")
    }
  }

  override def instanceReturn(instName: InstanceIdentifier,
                              attrType: DataType): Unit = {
    out.puts(s"Ok(${privateMemberName(instName)})")
  }

  override def enumDeclaration(curClass: List[String],
                               enumName: String,
                               enumColl: Seq[(Long, EnumValueSpec)]): Unit = {

    val enumClass = types2class(curClass ::: List(enumName))

    // Set up the actual enum definition
    out.puts(s"#[derive(Debug, PartialEq, Clone)]")
    out.puts(s"pub enum $enumClass {")
    out.inc

    enumColl.foreach {
      case (_, label) =>
        if (label.doc.summary.isDefined)
          universalDoc(label.doc)

        out.puts(s"${type2class(label.name)},")
    }
    out.puts("Unknown(i64),")

    out.dec
    out.puts("}")
    out.puts

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
    out.puts(s"_ => Ok($enumClass::Unknown(flag)),")
    out.dec

    out.puts("}")
    out.dec
    out.puts("}")
    out.dec
    out.puts("}")
    out.puts

    out.puts(s"impl From<&$enumClass> for i64 {")
    out.inc
    out.puts(s"fn from(v: &$enumClass) -> Self {")
    out.inc
    out.puts(s"match *v {")
    out.inc
    enumColl.foreach {
      case (value, label) =>
        out.puts(s"$enumClass::${type2class(label.name)} => $value,")
    }
    out.puts(s"$enumClass::Unknown(v) => v")
    out.dec
    out.puts("}")
    out.dec
    out.puts("}")
    out.dec
    out.puts("}")
    out.puts

    out.puts(s"impl Default for $enumClass {")
    out.inc
    out.puts(s"fn default() -> Self { $enumClass::Unknown(0) }")
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

  def handleAssignmentParams(id: Identifier, expr: String): Unit = {
    val paramId = typeProvider.nowClass.params.find(s => s.id == id)
    var need_clone = false
    if (paramId.isDefined) {
      need_clone = !translator.is_copy_type(paramId.get.dataType)
    }
    paramId.get.dataType match {
      case _: EnumType =>
        out.puts(s"*${RustCompiler.privateMemberName(id, writeAccess = true)} = $expr.clone();")
      case _ =>
        if (need_clone)
          out.puts(s"*${RustCompiler.privateMemberName(id, writeAccess = true)} = $expr.clone();")
        else
          out.puts(s"*${RustCompiler.privateMemberName(id, writeAccess = true)} = $expr;")
    }
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    val seqId = translator.findMember(idToStr(id))
    var done = false
    var refcell = false
    if (seqId.isDefined) {
      val idType = seqId.get.dataType
      idType match {
        case t: UserType =>
          refcell = true
        case _: BytesType => refcell = true
        case _: ArrayType => refcell = true
        case _: StrType => refcell = true
        case _: EnumType =>
          done = true
          out.puts(
            s"*${RustCompiler.privateMemberName(id, writeAccess = true)} = $expr;"
          )
        case _: SwitchType =>
          done = true
          out.puts(s"*${RustCompiler.privateMemberName(id, writeAccess = true)} = Some($expr);")
        case _ =>
      }
      if (refcell) {
          val typeName = kaitaiTypeToNativeType(Some(id), typeProvider.nowClass, idType)
          if (typeName.startsWith("Option<")) {
            out.puts(s"*${RustCompiler.privateMemberName(id, writeAccess = true)} = Some($expr);")
          } else {
            out.puts(s"*${RustCompiler.privateMemberName(id, writeAccess = true)} = $expr;")
          }
          done = true
      }
    }
    if (!done) {
      var inst = false
      id match {
        case _: InstanceIdentifier =>
          inst = true
        case RawIdentifier(inner) => inner match {
          case _: InstanceIdentifier =>
            inst = true
          case _ =>
        }
        case EndianIdentifier =>
          inst = true
        case _ =>
      }
      if (inst) {
        done = true
        out.puts(s"*${RustCompiler.privateMemberName(id, writeAccess = true)} = $expr;")
      }
    }
    if (!done)
      out.puts(s"*${RustCompiler.privateMemberName(id, writeAccess = true)} = $expr;")
  }

  override def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit =
    out.puts(s"let $id = $expr;")

  def translate_args(args: Seq[Ast.expr], into: Boolean): String = {
    Utils.join(args.map { a =>
      val typ = translator.detectType(a)
      var byref = ""
      val t = kaitaiTypeToNativeType(None, typeProvider.nowClass, typ)
      var try_into = ""
      typ match {
        case _: NumericType =>
          if (into) {
            try_into = s".try_into().map_err(|_| KError::CastError)?"
          }
        case _ =>
          if (!translator.is_copy_type(typ))
            byref = "&"
      }
      var translated = translator.translate(a)
      if (translated == "_r") // _root
        translated = "OptRc::new(&_rrc)"
      if (try_into.nonEmpty)
        s"$byref($translated)$try_into"
      else
        s"$byref$translated"
    }, "", ", ", "")
  }

  override def parseExpr(dataType: DataType,
                         assignType: DataType,
                         io: String,
                         defEndian: Option[FixedEndian]): String = {
    var addParams = ""
    dataType match {
      case t: ReadableType =>
        t match {
          case IntMultiType(_, _, None) =>
            s"if *${privateMemberName(EndianIdentifier)} == 1 { $io.read_${t.apiCall(Some(LittleEndian))}()?.into() } else { $io.read_${t.apiCall(Some(BigEndian))}()?.into() }"
          case IntMultiType(_, _, Some(e)) =>
            s"$io.read_${t.apiCall(Some(e))}()?.into()"
          case _ =>
            s"$io.read_${t.apiCall(defEndian)}()?.into()"
        }
      case _: BytesEosType => s"$io.read_bytes_full()?.into()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        val term = terminator.head & 0xff
        s"$io.read_bytes_term($term, $include, $consume, $eosError)?.into()"
      case b: BytesLimitType => s"$io.read_bytes(${expression(b.size)} as usize)?.into()"
      case BitsType1(bitEndian) => s"$io.read_bits_int_${bitEndian.toSuffix}(1)? != 0"
      case BitsType(width: Int, bitEndian) => s"$io.read_bits_int_${bitEndian.toSuffix}($width)?"
      case t: UserType =>
        addParams = translate_args(t.args, into = true)
        val userType = t match {
          case t: UserType =>
            val baseName = t.classSpec match {
              case Some(spec) => types2class(spec.name)
              case None => types2class(t.name)
            }
            s"$baseName"
        }
        val root = s"Some(${self_name()}.${privateMemberName(RootIdentifier)}.clone())"
        val addArgs = if (t.isExternal(typeProvider.nowClass)) {
          ", None, None"
        } else {
          var parent = t.forcedParent match {
            case Some(USER_TYPE_NO_PARENT) => "None"
            case Some(fp) => s"Some(SharedType::new(${translator.translate(fp)}.clone()))"
            case None => s"Some(${self_name()}._self.clone())"
          }
          t.classSpec.get.parentType match {
            case CalcKaitaiStructType(_) => parent = "None"
            case _ =>
          }
          s", $root, $parent"
        }
        var io2 = ""
        var streamType = ""
        if (io == privateMemberName(IoIdentifier)) {
          io2 = s"&*$io"
          streamType = "_"
        } else {
          io2 = translator.ensure_amp(io)
          streamType = "BytesReader"
        }
        if (addParams.isEmpty) {
          if (t.classSpec.isDefined) t.classSpec.get.meta.endian match {
            case Some(InheritedEndian) =>
              out.puts(s"let f = |t : &mut $userType| Ok(t.set_endian(*${privateMemberName(EndianIdentifier)}));")
              out.puts(s"let t = Self::read_into_with_init::<$streamType, $userType>($io2$addArgs, &f)?.into();")
            case _ =>
              out.puts(s"let t = Self::read_into::<$streamType, $userType>($io2$addArgs)?.into();")
          }
        } else {
          out.puts(s"let f = |t : &mut $userType| Ok(t.set_params($addParams));")
          out.puts(s"let t = Self::read_into_with_init::<$streamType, $userType>($io2$addArgs, &f)?.into();")
        }
        return s"t"
      case _ => s"// parseExpr($dataType, $assignType, $io, $defEndian)"
    }
  }

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Seq[Byte]], include: Boolean): String = {
    val ioId = privateMemberName(IoIdentifier)
    val expr1 = padRight match {
      case Some(padByte) => s"bytes_strip_right(&$expr0, $padByte).into()"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) =>
        val t = term.head & 0xff
        s"bytes_terminate(&$expr1, $t, $include).into()"
      case None => expr1
    }
    expr2
  }

  override def attrFixedContentsParse(attrName: Identifier,
                                      contents: String): Unit =
    out.puts(s"// attrFixedContentsParse($attrName, $contents)")

  override def publicMemberName(id: Identifier): String =
    s"// publicMemberName($id)"

  override def localTemporaryName(id: Identifier): String =
    s"_t_${idToStr(id)}"

  override def userTypeDebugRead(id: String, dataType: DataType, assignType: DataType): Unit = {
    // we already have splitted construction of object and read method
  }

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
    out.puts(s"let on = ${translator.remove_deref(expression(on))};")
  }

  override def switchIfCaseFirstStart(condition: Ast.expr): Unit = {
    out.puts(s"if *on == ${expression(condition)} {")
    out.inc
  }

  override def switchIfCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"else if *on == ${expression(condition)} {")
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

    var newStreamRaw = s"$memberName"
    val ioName = rep match {
      case NoRepeat =>
        var newStream = newStreamRaw
        val localIO = localTemporaryName(ioId)
          val ids = idToStr(id)
          out.puts(s"let $ids = $newStream;")
          newStream = ids
        out.puts(s"let $localIO = BytesReader::from($newStream.clone());")
        s"&$localIO"
      case _ =>
        val ids = idToStr(id)
        val localIO = s"io_$ids"
          out.puts(s"let $ids = $newStreamRaw;")
          newStreamRaw = ids
        out.puts(s"let $localIO = BytesReader::from($newStreamRaw.last().unwrap().clone());")
        s"&$localIO"
    }

    ioName
  }

  def switchTypeEnum(id: Identifier, st: SwitchType): Unit = {
    // Because Rust can't handle `AnyType` in the type hierarchy,
    // we generate an enum with all possible variations
    val enum_typeName = kaitaiTypeToNativeType(
      Some(id),
      typeProvider.nowClass,
      st,
      excludeOptionWrapper = true
    )
    out.puts("#[derive(Debug, Clone)]")
    out.puts(s"pub enum $enum_typeName {")
    out.inc

    val types = st.cases.values.toSet

    {
      val types_set = scala.collection.mutable.Set[String]()
      types.foreach(t => {
        // Because this switch type will itself be in an option, we can exclude it from user types
        val variantName = switchVariantName(id, t)
        val typeName = kaitaiTypeToNativeType(
          Some(id),
          typeProvider.nowClass,
          t,
          excludeOptionWrapper = true
        )
        val new_typename = types_set.add(typeName)
        // same typename could be in case of different endianness
        if (new_typename) {
          out.puts(s"$variantName($typeName),")
        }
      })
    }

    out.dec
    out.puts("}")

    var enum_only_numeric = true
    types.foreach {
      case _: NumericType => // leave true
      case _ => enum_only_numeric = false
    }

    // generate only if switch types are different
    {
      val types_set = scala.collection.mutable.Set[String]()
      // add helper methods From
      types.foreach(t => {
        // Because this switch type will itself be in an option, we can exclude it from user types
        val variantName = switchVariantName(id, t)
        var typeName = kaitaiTypeToNativeType(
              Some(id),
              typeProvider.nowClass,
              t,
              excludeOptionWrapper = true)

        val new_typename = types_set.add(typeName)
        if (new_typename) {
          // generate helpers to convert enum into variant (let x : Rc<Var1> = enum1.into())
          if (!enum_only_numeric) {
            val asOption = "^Option<.*".r
            val suffix = kaitaiTypeToNativeType(Some(id), typeProvider.nowClass, t) match {
              case asOption() => s".as_ref().unwrap()"
              case _ => ""
            }
            out.puts(s"impl From<&$enum_typeName> for $typeName {")
            out.inc
            out.puts(s"fn from(v: &$enum_typeName) -> Self {")
            out.inc
            out.puts(s"if let $enum_typeName::$variantName(x) = v {")
            out.inc
            out.puts(s"return x$suffix.clone();")
            out.dec
            out.puts("}")
            out.puts(s"""panic!("expected $enum_typeName::$variantName, got {:?}", v)""")
            out.dec
            out.puts("}")
            out.dec
            out.puts("}")
          }
          // special case for Bytes(Vec[u8]) (else switch)
          t match {
            case _ : BytesType =>
              typeName = s"Vec<u8>"
            case _ =>
          }
          // generate helpers to create enum from variant (let enum1 = Var1.into())
          out.puts(s"impl From<$typeName> for $enum_typeName {")
          out.inc
          out.puts(s"fn from(v: $typeName) -> Self {")
          out.inc
          out.puts(s"Self::$variantName(v)")
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
        }
      })
    }
    if (enum_only_numeric) {
      out.puts(s"impl From<&$enum_typeName> for usize {")
      out.inc
      out.puts(s"fn from(e: &$enum_typeName) -> Self {")
      out.inc
      out.puts(s"match e {")
      out.inc
      val variants_set = scala.collection.mutable.Set[String]()
      types.foreach(t => {
        val variantName = switchVariantName(id, t)
        val new_typename = variants_set.add(variantName)
        if (new_typename) {
          out.puts(s"$enum_typeName::$variantName(v) => *v as usize,")
        }
      })
      out.dec
      out.puts("}")
      out.dec
      out.puts("}")
      out.dec
      out.puts("}")
      out.puts
    }

    // generate helper method with name from variant type (to convert enum into variant and call variant method inside)
    // only if there is only single variant
    // if more than 1 - Kaitai will do casting
    if (types.size == 1) {
      val types_set = scala.collection.mutable.Set[String]()
      val attrs_set = scala.collection.mutable.Set[String]()
      types.foreach(t => {
        val typeName = kaitaiTypeToNativeType(Some(id), typeProvider.nowClass, t, cleanTypename = true)
        if (types_set.add(typeName)) {
          t match {
            case ut: UserType =>
              ut.classSpec.get.seq.foreach(
                attr => {
                  val attrName = attr.id
                  if (attrs_set.add(idToStr(attrName))) {
                    out.puts(s"impl $enum_typeName {")
                    out.inc
                    val fn = idToStr(attrName)
                    var nativeType = kaitaiTypeToNativeType(Some(attrName), typeProvider.nowClass, attr.dataTypeComposite, cleanTypename = true)
                    var nativeTypeEx = kaitaiTypeToNativeType(Some(attrName), typeProvider.nowClass, attr.dataTypeComposite)
                    val typeNameEx = kaitaiTypeToNativeType(Some(id), typeProvider.nowClass, t)
                    val x = if (typeNameEx.startsWith("Option<")) "x.as_ref().unwrap()" else "x"
                    var clone = ""
                    if (nativeTypeEx.startsWith("OptRc<")) {
                      nativeType = s"$nativeTypeEx"
                      clone = ".clone()"
                    } else
                      nativeType = s"Ref<$nativeType>"
                    out.puts(s"pub fn $fn(&self) -> $nativeType {")
                    out.inc
                    out.puts("match self {")
                    out.inc
                    out.puts(s"$enum_typeName::$typeName(x) => $x.$fn.borrow()$clone,")
                    //out.puts("_ => panic!(\"wrong variant: {:?}\", self),")
                    out.dec
                    out.puts("}")
                    out.dec
                    out.puts("}")
                    out.dec
                    out.puts("}")
                  }
                }
              )
            case _ =>
          }
        }
      })
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
          Some(id),
          typeProvider.nowClass,
          t,
          cleanTypename = true
        )
      case t: EnumType =>
        kaitaiTypeToNativeType(
          Some(id),
          typeProvider.nowClass,
          t,
          excludeOptionWrapper = true
        )
      case t: ArrayType => s"Arr${switchVariantName(id, t.elType)}"
    }

  override def ksErrorName(err: KSError): String = RustCompiler.ksErrorName(err)

  override def attrValidateExpr(
    attr: AttrLikeSpec,
    checkExpr: Ast.expr,
    err: KSError,
    errArgs: List[Ast.expr]
  ): Unit = {
    val srcPathStr = translator.translate(Ast.expr.Str(attr.path.mkString("/", "/", "")))
    val validationKind = RustCompiler.validationErrorKind(err.asInstanceOf[ValidationError])
    out.puts(s"if !(${expression(checkExpr)}) {")
    out.inc
    out.puts(s"""return Err(${ksErrorName(err)}(ValidationFailedError { kind: $validationKind, src_path: $srcPathStr.to_string() }));""")
    out.dec
    out.puts("}")
  }

  override def attrParse2(
                           id: Identifier,
                           dataType: DataType,
                           io: String,
                           rep: RepeatSpec,
                           isRaw: Boolean,
                           defEndian: Option[FixedEndian],
                           assignTypeOpt: Option[DataType] = None
                         ): Unit = {
    dataType match {
      case t: EnumType =>
        val expr =
          t.basedOn match {
            case inst: ReadableType =>
              s"($io.read_${inst.apiCall(defEndian)}()? as i64).try_into()?"
            case BitsType(width: Int, bitEndian) =>
              s"($io.read_bits_int_${bitEndian.toSuffix}($width)? as i64).try_into()?"
          }
        handleAssignment(id, expr, rep, isRaw)
      case _ =>
        super.attrParse2(id, dataType, io, rep, isRaw, defEndian, assignTypeOpt)
    }
  }

  override def classToString(toStringExpr: Ast.expr): Unit = {
    importList.add("use std::fmt;")
    out.puts(s"impl fmt::Display for ${classTypeName(typeProvider.nowClass)} {")
    out.inc
    out.puts(s"fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {")
    out.inc
    out.puts(s"""write!(f, "{}", ${translator.translate(toStringExpr)})""")
    out.dec
    out.puts("}")
    out.dec
    out.puts("}")
  }
}

object RustCompiler
  extends LanguageCompilerStatic
    with StreamStructNames
    with UpperCamelCaseClasses
    with ExceptionNames {
  override def getCompiler(tp: ClassTypeProvider,
                           config: RuntimeConfig): LanguageCompiler =
    new RustCompiler(tp, config)

  var in_reader = false

  def self_name(): String = {
    if (in_reader) "self_rc" else "self"
  }

  def privateMemberName(id: Identifier, writeAccess: Boolean = false): String = id match {
    case IoIdentifier => "_io"
    case RootIdentifier => "_root"
    case ParentIdentifier => "_parent"
    case _ =>
      val n = s"${self_name()}.${idToStr(id)}"
      if (writeAccess)
        s"$n.borrow_mut()"
      else
        s"$n.borrow()"
  }

  def idToStr(id: Identifier): String = id match {
    case SpecialIdentifier(n) => n
    case NamedIdentifier(n) => n
    case InstanceIdentifier(n) => n
    case NumberedIdentifier(idx) => s"${NumberedIdentifier.TEMPLATE}$idx"
    case RawIdentifier(inner) => s"${idToStr(inner)}_raw" // use suffix naming, easy to replace, like in anyField()
    case IoStorageIdentifier(inner) => s"${idToStr(inner)}_io" // same here
  }

  @tailrec
  def rootClassTypeName(c: ClassSpec, isRecurse: Boolean = false): String = {
    if (!isRecurse && c.isTopLevel)
      "Self"
    else if (c.isTopLevel)
      classTypeName(c)
    else
      rootClassTypeName(c.upClass.get, isRecurse = true)
  }

  override def kstreamName = "KStream"
  override def kstructName = "KStruct"

  def kstructUnitName = "KStructUnit"

  override def ksErrorName(err: KSError): String = err match {
    case EndOfStreamError => "KError::Eof"
    case UndecidedEndiannessError => "KError::UndecidedEndianness"
    case ConversionError => "KError::CastError"
    case _: ValidationError => "KError::ValidationFailed"
  }

  def validationErrorKind(err: ValidationError): String = {
    val kind = err match {
      case _: ValidationNotEqualError => "NotEqual"
      case _: ValidationLessThanError => "LessThan"
      case _: ValidationGreaterThanError => "GreaterThan"
      case _: ValidationNotAnyOfError => "NotAnyOf"
      case _: ValidationNotInEnumError => "NotInEnum"
      case _: ValidationExprError => "Expr"
    }
    s"ValidationKind::$kind"
  }

  def classTypeName(c: ClassSpec): String =
    s"${types2class(c.name)}"

  def types2class(names: List[String]): String =
  // TODO: Use `mod` to scope types instead of weird names
    names.map(x => type2class(x)).mkString("_")

  def kaitaiTypeToNativeType(id: Option[Identifier],
                             cs: ClassSpec,
                             attrType: DataType,
                             excludeOptionWrapper: Boolean = false,
                             cleanTypename: Boolean = false): String =
    attrType match {
      // TODO: Not exhaustive
      case _: NumericType | _: BooleanType | _: StrType | _: BytesType =>
          kaitaiPrimitiveToNativeType(attrType)

      case t: UserType =>
        val baseName = t.classSpec match {
          case Some(spec) => types2class(spec.name)
          case None => types2class(t.name)
        }
        if (cleanTypename)
          baseName
        else
          s"OptRc<$baseName>"

      case t: EnumType =>
        val baseName = t.enumSpec match {
          case Some(spec) => s"${types2class(spec.name)}"
          case None => s"${types2class(t.name)}"
        }
        baseName

      case t: ArrayType =>
        s"Vec<${kaitaiTypeToNativeType(id, cs, t.elType, excludeOptionWrapper = true)}>"

      case _: SwitchType =>
        val typeName = id.get match {
          case name: NamedIdentifier =>
            s"${types2class(cs.name ::: List(name.name))}"
          case name: InstanceIdentifier =>
            s"${types2class(cs.name ::: List(name.name))}"
          case _ => kstructUnitName
        }

        if (excludeOptionWrapper) typeName else s"Option<$typeName>"

      case KaitaiStreamType => "BytesReader"
      case CalcKaitaiStructType(_) => kstructUnitName
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

    case _: StrType => "String"
    case _: BytesType => "Vec<u8>"

    case ArrayTypeInStream(inType) => s"Vec<${kaitaiPrimitiveToNativeType(inType)}>"
  }
}
