package io.kaitai.struct.languages

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.{GoTranslator, ResultLocalVar, ResultString, TranslatorResult}
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig, Utils}

class GoCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with SingleOutputFile
    with UpperCamelCaseClasses
    with ObjectOrientedLanguage
    with UniversalFooter
    with UniversalDoc
    with AllocateIOLocalVar
    with GoReads
    with EveryWriteIsExpression
    with GenericChecks
    with FetchInstances {
  import GoCompiler._

  override val translator = new GoTranslator(out, typeProvider, importList) {
    override def numericType(value: Ast.expr): String = {
      if (!config.readWrite) return super.numericType(value)
      value match {
        case Ast.expr.Name(name) =>
          unsignedValueInstance(typeProvider.nowClass, name.name)
            .getOrElse(super.numericType(value))
        case Ast.expr.Attribute(owner, name) =>
          detectType(owner) match {
            case ut: UserType => unsignedValueInstance(ut.classSpec.get, name.name)
              .getOrElse(super.numericType(value))
            case et: EnumType if name.name == "to_i" && unsignedEnum(et.enumSpec.get.map.keys) => "uint64"
            case _: ArrayType if Set("first", "last", "min", "max").contains(name.name) =>
              unsignedArrayType(owner).map(_.stripPrefix("[]")).getOrElse(super.numericType(value))
            case _ => super.numericType(value)
          }
        case Ast.expr.Subscript(container, _) =>
          unsignedArrayType(container).map(_.stripPrefix("[]")).getOrElse(super.numericType(value))
        case _ => super.numericType(value)
      }
    }

    override def enumToInt(value: Ast.expr, et: EnumType): TranslatorResult =
      if (config.readWrite && unsignedEnum(et.enumSpec.get.map.keys))
        ResultString(s"uint64(${translate(value)})")
      else super.enumToInt(value, et)

    override def translateExpr(value: Ast.expr, precedence: Int): TranslatorResult =
      capturedChildSize match {
        case Some((expr, name)) if value == expr => ResultString(name)
        case _ => super.translateExpr(value, precedence)
      }

    override def trLocalName(name: String): TranslatorResult = {
      if (writing && name == Identifier.IO) {
        // Output metadata remains available when another object refers to this
        // stream after its write scope has ended. Actual writes use active IO.
        ResultString("this._writer")
      } else if (writing && writerParam(typeProvider.nowClass, name).isDefined) {
        ResultString(s"this._writer_${Utils.upperCamelCase(name)}")
      } else if (writing && typeProvider.nowClass.instances.keys.exists(_.humanReadable == name)) {
        outVarCheckRes(s"this._writeGet${Utils.upperCamelCase(name)}()")
      } else if (checking && parseInstance(typeProvider.nowClass, name)) {
        ResultString(s"this.${Utils.lowerCamelCase(name)}")
      } else if (config.readWrite) {
        typeProvider.nowClass.seq.find(_.id.humanReadable == name) match {
          case Some(attr) => ResultString(GoCompiler.this.privateMemberName(attr.id))
          case None => super.trLocalName(name)
        }
      } else {
        super.trLocalName(name)
      }
    }

    override def trInternalName(id: Identifier): TranslatorResult =
      if (writing && id == IoIdentifier) ResultString(normalIO)
      else if (config.readWrite) ResultString(GoCompiler.this.privateMemberName(id))
      else super.trInternalName(id)

    override def doArrayLiteral(t: DataType, value: Seq[Ast.expr]): ResultString =
      if (writing && isIoType(t))
        ResultString(s"[]${writerParamType(t)}{${value.map(translate).mkString(", ")}}")
      else if (config.readWrite && calculatedIntegerArrayElement(t)) {
        val native = unsignedArrayType(Ast.expr.List(value)).get
        ResultString(s"$native{${value.map(translateArrayElementAs(_, native.stripPrefix("[]"))).mkString(", ")}}")
      }
      else super.doArrayLiteral(t, value)

    override def doCast(value: Ast.expr, typeName: DataType): TranslatorResult =
      if (writing && isIoType(typeName)) {
        val nativeType = writerParamType(typeName)
        detectType(value) match {
          case KaitaiStructType | CalcKaitaiStructType(_) | AnyType =>
            ResultString(s"(${translate(value)}).($nativeType)")
          case _ =>
            ResultString(s"($nativeType)(${translate(value)})")
        }
      } else super.doCast(value, typeName)

    override def trIfExp(condition: Ast.expr, ifTrue: Ast.expr, ifFalse: Ast.expr): ResultLocalVar = {
      val dataType = detectType(Ast.expr.IfExp(condition, ifTrue, ifFalse))
      val native = if (writing && isIoType(dataType)) Some(writerParamType(dataType))
        else if (config.readWrite) unsignedArrayType(Ast.expr.IfExp(condition, ifTrue, ifFalse))
        else None
      if (native.isEmpty) return super.trIfExp(condition, ifTrue, ifFalse)
      val result = allocateLocalVar()
      out.puts(s"var ${localVarName(result)} ${native.get}")
      out.puts(s"if (${translate(condition)}) {")
      out.inc
      out.puts(s"${localVarName(result)} = ${if (isIoType(dataType)) translate(ifTrue) else translateArrayElementAs(ifTrue, native.get)}")
      out.dec
      out.puts("} else {")
      out.inc
      out.puts(s"${localVarName(result)} = ${if (isIoType(dataType)) translate(ifFalse) else translateArrayElementAs(ifFalse, native.get)}")
      out.dec
      out.puts("}")
      ResultLocalVar(result)
    }

    override def kaitaiStreamSize(value: Ast.expr): TranslatorResult =
      if (writing) ResultString(writerSize(translate(value))) else super.kaitaiStreamSize(value)

    override def kaitaiStreamEof(value: Ast.expr): TranslatorResult =
      if (writing) ResultString(writerEof(translate(value))) else super.kaitaiStreamEof(value)

    override def kaitaiStructField(value: Ast.expr, name: String): TranslatorResult = {
      if (!writing || name != Identifier.IO) return super.kaitaiStructField(value, name)
      val owner = translate(value)
      importList.add("fmt")
      importList.add("reflect")
      // Keep runtime Struct's read-only contract intact. Writer metadata is an
      // optional capability, including when the selected value is opaque.
      outVarCheckRes(
        s"""func(value kaitai.Struct) (*kaitai.Writer, error) {
           |  if value == nil || (reflect.ValueOf(value).Kind() == reflect.Ptr && reflect.ValueOf(value).IsNil()) {
           |    return nil, fmt.Errorf("cannot access output stream of %T: object is nil", value)
           |  }
           |  provider, ok := value.(interface { Writer_() *kaitai.Writer })
           |  if !ok { return nil, fmt.Errorf("cannot access output stream of %T: missing Writer_ capability", value) }
           |  writer := provider.Writer_()
           |  if writer == nil { return nil, fmt.Errorf("cannot access output stream of %T: writer is nil", value) }
           |  return writer, nil
           |}($owner)""".stripMargin)
    }

    override def userTypeField(ut: UserType, value: Ast.expr, name: String): TranslatorResult =
      if (writing && name == Identifier.IO)
        ResultString(s"${translate(value)}._writer")
      else if (writing && ut.classSpec.exists(writerParam(_, name).isDefined))
        ResultString(s"${translate(value)}._writer_${Utils.upperCamelCase(name)}")
      else if (writing && ut.classSpec.exists(_.instances.keys.exists(_.humanReadable == name)))
        outVarCheckRes(s"${translate(value)}._writeGet${Utils.upperCamelCase(name)}()")
      else if (checking && ut.classSpec.exists(parseInstance(_, name))) {
        val owner = translate(value)
        ResultString(s"$owner.${Utils.lowerCamelCase(name)}")
      }
      else if (config.readWrite && ut.classSpec.exists(_.seq.exists(_.id.humanReadable == name)))
        ResultString(s"${translate(value)}.Get${Utils.upperCamelCase(name)}()")
      else
        super.userTypeField(ut, value, name)
  }

  private def unsignedEnum(values: Iterable[BigInt]): Boolean =
    values.exists(_ > Long.MaxValue) && values.forall(n => n >= 0 && n <= Utils.MAX_UINT64)

  private def calculatedIntegerArrayElement(t: DataType): Boolean = t match {
    case CalcIntType => true
    case array: ArrayType => calculatedIntegerArrayElement(array.elType)
    case _ => false
  }

  private def expressionNativeType(value: Ast.expr): String = translator.detectType(value) match {
    case _: ArrayType => unsignedArrayType(value).getOrElse(kaitaiType2NativeType(translator.detectType(value)))
    case _: IntType => translator.numericType(value)
    case t => kaitaiType2NativeType(t)
  }

  private def inInstance[A](owner: ClassSpec, name: String)(f: Ast.expr => A): Option[A] =
    owner.instances.collectFirst { case (id, inst: ValueInstanceSpec) if id.humanReadable == name => inst }.map { inst =>
      val previous = typeProvider.nowClass
      typeProvider.nowClass = owner
      try f(inst.value) finally typeProvider.nowClass = previous
    }

  /** Conservative mathematical bounds for homogeneous array representation.
    * Literals and +0 retain exact/primitive bounds; other arithmetic falls
    * back to the whole range of the concrete Go expression type.
    */
  private def integerArrayBounds(value: Ast.expr): (BigInt, BigInt) = {
    def combine(values: Seq[Ast.expr]): (BigInt, BigInt) = {
      val ranges = values.map(integerArrayBounds)
      (ranges.map(_._1).min, ranges.map(_._2).max)
    }
    value match {
      // Do not use the shared constant evaluator here: its integer division
      // truncates, whereas Go's mixed signed/unsigned division rounds down.
      // Other expressions use conservative native bounds unless proven below.
      case Ast.expr.IntNum(n) => (n, n)
      case Ast.expr.UnaryOp(Ast.unaryop.Minus, Ast.expr.IntNum(n)) => (-n, -n)
      case Ast.expr.List(elements) => combine(elements)
      case Ast.expr.Name(name) =>
        inInstance(typeProvider.nowClass, name.name)(integerArrayBounds).getOrElse(nativeIntegerBounds(expressionNativeType(value)))
      case Ast.expr.Attribute(owner, name) => translator.detectType(owner) match {
        case ut: UserType => inInstance(ut.classSpec.get, name.name)(integerArrayBounds)
          .getOrElse(nativeIntegerBounds(expressionNativeType(value)))
        case _: ArrayType if Set("first", "last", "min", "max").contains(name.name) => integerArrayBounds(owner)
        case _ => nativeIntegerBounds(expressionNativeType(value))
      }
      case Ast.expr.Subscript(owner, _) => integerArrayBounds(owner)
      case Ast.expr.IfExp(_, left, right) => combine(Seq(left, right))
      case Ast.expr.CastToType(inner, _) =>
        val target = nativeIntegerBounds(expressionNativeType(value))
        if (!translator.detectType(inner).isInstanceOf[IntType]) target
        else {
          val source = integerArrayBounds(inner)
          if (target._1 <= source._1 && source._2 <= target._2) source else target
        }
      case Ast.expr.BinOp(left, Ast.operator.Add, Ast.expr.IntNum(n)) if n == 0 => integerArrayBounds(left)
      case Ast.expr.BinOp(Ast.expr.IntNum(n), Ast.operator.Add, right) if n == 0 => integerArrayBounds(right)
      case _ => nativeIntegerBounds(expressionNativeType(value))
    }
  }

  private def nativeIntegerBounds(native: String): (BigInt, BigInt) = {
    if (native.startsWith("[]")) return nativeIntegerBounds(native.stripPrefix("[]"))
    val unsigned = native.startsWith("uint")
    val width = if (native == "int") 64 else native.drop(if (unsigned) 4 else 3).toInt
    if (unsigned) (BigInt(0), (BigInt(1) << width) - 1)
    else (-(BigInt(1) << (width - 1)), (BigInt(1) << (width - 1)) - 1)
  }

  private def commonArrayNative(types: Seq[String], values: Seq[Ast.expr]): String = {
    if (types.forall(_.startsWith("[]"))) return "[]" + commonArrayNative(types.map(_.stripPrefix("[]")), values)
    if (types.contains("uint64")) {
      val bounds = values.map(integerArrayBounds)
      val minimum = bounds.map(_._1).min
      val maximum = bounds.map(_._2).max
      if (minimum < 0 && maximum > Long.MaxValue)
        throw new io.kaitai.struct.precompile.TypeMismatchError("Go read-write cannot represent an integer array containing both negative values and values above 9223372036854775807 without loss")
      if (minimum < 0) "int64" else "uint64"
    } else "int"
  }

  private def unsignedArrayType(value: Ast.expr): Option[String] = {
    if (!config.readWrite) return None
    translator.detectType(value) match {
      case t: ArrayType if calculatedIntegerArrayElement(t.elType) =>
      case _ => return None
    }
    value match {
      case Ast.expr.List(elements) => Some("[]" + commonArrayNative(elements.map(expressionNativeType), elements))
      case Ast.expr.Name(name) => unsignedValueInstance(typeProvider.nowClass, name.name)
      case Ast.expr.Attribute(owner, name) => translator.detectType(owner) match {
        case ut: UserType => unsignedValueInstance(ut.classSpec.get, name.name)
        case _: ArrayType if Set("first", "last", "min", "max").contains(name.name) =>
          unsignedArrayType(owner).map(_.stripPrefix("[]"))
        case _ => None
      }
      case Ast.expr.Subscript(owner, _) => unsignedArrayType(owner).map(_.stripPrefix("[]"))
      case Ast.expr.IfExp(_, ifTrue, ifFalse) =>
        val values = Seq(ifTrue, ifFalse)
        Some(commonArrayNative(values.map(expressionNativeType), values))
      case _ => None
    }
  }

  /** Go cannot convert slices element-wise with a cast. Literal branches can
    * use the common type directly; other branches need a recursive copy.
    */
  private def translateArrayElementAs(value: Ast.expr, target: String): String = {
    if (!target.startsWith("[]")) {
      val sourceType = expressionNativeType(value)
      val sourceBounds = nativeIntegerBounds(sourceType)
      val targetBounds = nativeIntegerBounds(target)
      val literal = value match {
        case Ast.expr.IntNum(n) => Some(n)
        case Ast.expr.UnaryOp(Ast.unaryop.Minus, Ast.expr.IntNum(n)) => Some(-n)
        case _ => None
      }
      if (literal.exists(n => targetBounds._1 <= n && n <= targetBounds._2) ||
        (targetBounds._1 <= sourceBounds._1 && sourceBounds._2 <= targetBounds._2))
        return translator.translateNumericAs(value, target)
      return checkedArrayInteger(translator.translate(value), sourceType, target)
    }
    value match {
      case Ast.expr.List(elements) =>
        s"$target{${elements.map(translateArrayElementAs(_, target.stripPrefix("[]"))).mkString(", ")}}"
      case _ =>
        def convert(source: String, sourceType: String, targetType: String): String = {
          if (sourceType == targetType) return source
          if (!targetType.startsWith("[]")) return checkedArrayInteger(source, sourceType, targetType)
          val result = translator.localVarName(translator.allocateLocalVar())
          val index = translator.localVarName(translator.allocateLocalVar())
          out.puts(s"$result := make($targetType, len($source))")
          out.puts(s"for $index := range $source {")
          out.inc
          val element = convert(s"$source[$index]", sourceType.stripPrefix("[]"), targetType.stripPrefix("[]"))
          out.puts(s"$result[$index] = $element")
          out.dec
          out.puts("}")
          result
        }
        val source = translator.translate(value)
        convert(source, expressionNativeType(value), target)
    }
  }

  /** Cached array getters expose mutable slices, so their initializer bounds
    * cannot justify an unchecked conversion of values observed later.
    */
  private def checkedArrayInteger(source: String, sourceType: String, targetType: String): String = {
    val sourceBounds = nativeIntegerBounds(sourceType)
    val targetBounds = nativeIntegerBounds(targetType)
    val guards = List(
      if (sourceBounds._1 < targetBounds._1) Some(s"value < ${targetBounds._1}") else None,
      if (sourceBounds._2 > targetBounds._2) Some(s"value > ${targetBounds._2}") else None
    ).flatten
    if (guards.isEmpty) return s"$targetType($source)"
    importList.add("errors")
    val error = s"Go read-write integer array element cannot be converted from $sourceType to $targetType without loss"
    translator.resToStr(translator.outVarCheckRes(
      s"""func(value $sourceType) ($targetType, error) {
         |if ${guards.mkString(" || ")} { return 0, errors.New("$error") }
         |return $targetType(value), nil
         |}($sourceType($source))""".stripMargin
    ))
  }

  /** CalcIntType normally maps to int. Preserve uint64 expressions and arrays
    * in RW instance storage and signatures when int cannot represent their range.
    * Resolve in the instance owner's scope, including nested member access.
    */
  private def unsignedValueInstance(owner: ClassSpec, name: String): Option[String] = {
    if (!config.readWrite) return None
    owner.instances.collectFirst {
      case (id, inst: ValueInstanceSpec) if id.humanReadable == name &&
        (inst.dataType == CalcIntType || inst.dataType.isInstanceOf[ArrayType]) => inst
    }.flatMap { inst =>
      val previous = typeProvider.nowClass
      typeProvider.nowClass = owner
      try {
        if (inst.dataType.isInstanceOf[ArrayType]) unsignedArrayType(inst.value)
        else translator.numericType(inst.value) match {
          case "uint64" => Some("uint64")
          case "int64" => inst.value match {
            case Ast.expr.Subscript(container, _) => unsignedArrayType(container).map(_.stripPrefix("[]"))
            case Ast.expr.Attribute(container, _) if translator.detectType(container).isInstanceOf[ArrayType] =>
              unsignedArrayType(container).map(_.stripPrefix("[]"))
            case Ast.expr.Name(name) => unsignedValueInstance(typeProvider.nowClass, name.name)
            case _ => None
          }
          case _ => None
        }
      } finally typeProvider.nowClass = previous
    }
  }

  private def instanceNativeType(id: Identifier, dataType: DataType): String =
    unsignedValueInstance(typeProvider.nowClass, id.humanReadable)
      .getOrElse(kaitaiType2NativeType(dataType))

  private def isPrivateSequenceField(id: Identifier): Boolean =
    config.readWrite && typeProvider.nowClass.seq.exists(_.id == id)

  private var writing = false
  private var checking = false
  private var writeMain = false
  private var writingInstance: Option[InstanceIdentifier] = None
  private var capturedChildSize: Option[(Ast.expr, String)] = None

  private def parseInstance(cls: ClassSpec, name: String): Boolean =
    cls.instances.exists { case (id, spec) => id.humanReadable == name && spec.isInstanceOf[ParseInstanceSpec] }

  private def parseInstances: Iterable[InstanceIdentifier] =
    typeProvider.nowClass.instances.collect { case (id, _: ParseInstanceSpec) => id }

  private def isIoType(dataType: DataType): Boolean = getArrayItemType(dataType) match {
    case KaitaiStreamType | OwnedKaitaiStreamType => true
    case _ => false
  }

  private def ioValueInstances: Iterable[InstanceIdentifier] =
    typeProvider.nowClass.instances.collect {
      case (id, value: ValueInstanceSpec) if isIoType(value.dataTypeComposite) => id
    }

  private def writerValueInstance(id: Identifier): Boolean =
    writing && ioValueInstances.exists(_ == id)

  private def instanceValueName(id: Identifier): String =
    if (writerValueInstance(id)) s"this._wv_${idToStr(id)}" else privateMemberName(id)

  private def instanceCacheFlag(id: InstanceIdentifier): String =
    if (writerValueInstance(id)) s"_wf_${idToStr(id)}" else calculatedFlagForName(id)

  private def writerParam(cls: ClassSpec, name: String): Option[ParamDefSpec] =
    cls.params.find(p => p.id.humanReadable == name && (getArrayItemType(p.dataType) match {
      case KaitaiStreamType | OwnedKaitaiStreamType => true
      case _ => false
    }))

  private def writerParamType(t: DataType): String = t match {
    case array: ArrayType => s"[]${writerParamType(array.elType)}"
    case _ => "*kaitai.Writer"
  }

  override def normalIO: String = if (writing) "this._activeWriter" else super.normalIO

  override def innerClasses = false

  override def headerComment = "Code generated by kaitai-struct-compiler from a .ksy source file. DO NOT EDIT."

  override def universalFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def indent: String = "\t"
  override def outFileName(topClassName: String): String =
    s"${config.goPackage}/$topClassName.go"

  override def outImports(topClass: ClassSpec): String = {
    val imp = importList.toList
    imp.size match {
      case 0 => ""
      case 1 => "import \"" + imp.head + "\"\n"
      case _ =>
        "import (\n" +
        imp.map((x) => indent + "\"" + x + "\"").mkString("", "\n", "\n") +
        ")\n"
    }
  }

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"// $headerComment")
    if (config.goPackage.nonEmpty) {
      outHeader.puts
      outHeader.puts(s"package ${config.goPackage}")
    }
    outHeader.puts

    importList.add("github.com/kaitai-io/kaitai_struct_go_runtime/kaitai")

    out.puts
  }

  override def classHeader(name: List[String]): Unit = {
    out.puts(s"type ${types2class(name)} struct {")
    out.inc
  }

  override def classFooter(name: List[String]): Unit = {
    if (config.readWrite) {
      // The zero value is unchecked, including objects created without New.
      out.puts("_checked bool")
      out.puts("_writer *kaitai.Writer")
      out.puts("_activeWriter *kaitai.Writer")
      typeProvider.nowClass.params.filter(p => writerParam(typeProvider.nowClass, p.id.humanReadable).isDefined)
        .foreach(p => out.puts(s"_writer_${idToStr(p.id)} ${writerParamType(p.dataType)}"))
    }
    // TODO(jchw): where should this attribute actually be generated at?
    typeProvider.nowClass.meta.endian match {
      case Some(_: CalcEndian) | Some(InheritedEndian) =>
        out.puts(s"${idToStr(EndianIdentifier)} int")
      case _ =>
    }
    universalFooter
  }

  override def classConstructorHeader(name: List[String], parentType: DataType, rootClassName: List[String], isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    val paramsArg = params.map((p) =>
      s"${paramName(p.id)} ${kaitaiType2NativeType(p.dataType)}"
    ).mkString(", ")
    out.puts(s"func New${types2class(name)}($paramsArg) *${types2class(name)} {")
    out.inc
    out.puts(s"return &${types2class(name)}{")
    out.inc
    params.foreach(p => out.puts(s"${idToStr(p.id)}: ${paramName(p.id)},"))
    if (config.readWrite) {
      out.puts("_checked: false,")
      parseInstances.foreach(id => out.puts(s"_enabled_${idToStr(id)}: true,"))
      typeProvider.nowClass.meta.endian match {
        case Some(_: CalcEndian) | Some(InheritedEndian) => out.puts("_is_le: -1,")
        case _ =>
      }
    }
    out.dec
    out.puts("}")
    universalFooter

    ioAccessor()
    parentAccessor()
    if (config.readWrite) {
      // A trailing underscore cannot collide with a KSY-derived field setter.
      // These let callers assemble nested graphs without first parsing bytes.
      Seq(("Parent", "_parent", kaitaiType2NativeType(parentType)),
        ("Root", "_root", s"*${types2class(rootClassName)}")).foreach { case (property, field, native) =>
        out.puts(s"func (this *${types2class(name)}) Set${property}_(v $native) {")
        out.inc
        out.puts(s"this.$field = v")
        out.puts("this._checked = false")
        universalFooter
      }
      typeProvider.nowClass.params.filter(p => writerParam(typeProvider.nowClass, p.id.humanReadable).isDefined).foreach { p =>
        val name = idToStr(p.id)
        val owner = types2class(typeProvider.nowClass.name)
        val native = writerParamType(p.dataType)
        out.puts(s"func (this *$owner) ${name}Writer_() $native {")
        out.inc
        out.puts(s"return this._writer_$name")
        universalFooter
        out.puts(s"func (this *$owner) Set${name}Writer_(v $native) {")
        out.inc
        out.puts(s"this._writer_$name = v")
        out.puts("this._checked = false")
        universalFooter
      }
    }
  }

  override def classConstructorFooter: Unit = {}

  override def runRead(name: List[String]): Unit = {
    out.puts("this.Read()")
  }

  override def runReadCalc(): Unit = {
    out.puts
    out.puts(s"switch ${privateMemberName(EndianIdentifier)} {")
    out.puts("case 0:")
    out.inc
    out.puts("err = this._read_be()")
    out.dec
    out.puts("case 1:")
    out.inc
    out.puts("err = this._read_le()")
    out.dec
    out.puts("default:")
    out.inc
    out.puts(s"err = ${GoCompiler.ksErrorName(UndecidedEndiannessError)}{}")
    out.dec
    out.puts("}")
  }

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = {
    if (config.readWrite) {
      writing = false
      translator.returnRes = None
    }
    endian match {
      case None =>
        out.puts
        out.puts(
          s"func (this *${types2class(typeProvider.nowClass.name)}) Read(" +
            s"io *$kstreamName, " +
            s"parent ${kaitaiType2NativeType(typeProvider.nowClass.parentType)}, " +
            s"root *${types2class(typeProvider.topClass.name)}) (err error) {"
        )
        out.inc
        if (config.readWrite) {
          out.puts("this._checked = false")
          typeProvider.nowClass.instances.keys.foreach(id =>
            out.puts(s"this.${calculatedFlagForName(id)} = false"))
          parseInstances.foreach { id =>
            instanceWriteFlagInit(id)
            out.puts(s"this._enabled_${idToStr(id)} = true")
          }
        }
        out.puts(s"${privateMemberName(IoIdentifier)} = io")
        out.puts(s"${privateMemberName(ParentIdentifier)} = parent")
        out.puts(s"${privateMemberName(RootIdentifier)} = root")
        typeProvider.nowClass.meta.endian match {
          case Some(_: CalcEndian) =>
            out.puts(s"${privateMemberName(EndianIdentifier)} = -1")
          case Some(InheritedEndian) =>
            out.puts(s"${privateMemberName(EndianIdentifier)} = " +
              s"${privateMemberName(ParentIdentifier)}." +
              s"${idToStr(EndianIdentifier)}")
          case _ =>
        }
        out.puts
      case Some(e) =>
        out.puts
        out.puts(
          s"func (this *${types2class(typeProvider.nowClass.name)}) " +
            s"_read_${e.toSuffix}() (err error) {")
        out.inc
    }

  }
  override def readFooter(): Unit = {
    if (config.readWrite) {
      out.puts("this._checked = err == nil")
    }
    out.puts("return err")
    universalFooter
  }

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"${idToStr(attrName)} ${kaitaiType2NativeType(attrType)}")
    translator.returnRes = None
  }

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    if (isPrivateSequenceField(attrName)) {
      out.puts
      attrType match {
        case _: BytesType | _: ArrayType =>
          out.puts("// The returned slice aliases the field; in-place mutations do not invalidate checks.")
          out.puts("// Call Check() after mutating its contents and before Write().")
        case _ =>
      }
      out.puts(s"func (this *${types2class(typeProvider.nowClass.name)}) Get${GoCompiler.idToStr(attrName)}() ${kaitaiType2NativeType(attrType)} {")
      out.inc
      out.puts(s"return ${privateMemberName(attrName)}")
      universalFooter
    }
  }

  override def attributeSetter(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    if (isPrivateSequenceField(attrName) || attrName.isInstanceOf[InstanceIdentifier]) {
      val name = Utils.upperCamelCase(attrName.humanReadable)
      out.puts
      out.puts(s"func (this *${types2class(typeProvider.nowClass.name)}) Set$name(v ${kaitaiType2NativeType(attrType)}) {")
      out.inc
      out.puts("this._checked = false")
      out.puts(s"${privateMemberName(attrName)} = v")
      attrName match {
        case inst: InstanceIdentifier =>
          instanceSetCalculated(inst)
          instanceSetWriteFlag(inst)
        case _ =>
      }
      universalFooter
    }
  }

  private def errorMethodHeader(name: String, args: String = ""): Unit = {
    translator.returnRes = None
    out.puts
    out.puts(s"func (this *${types2class(typeProvider.nowClass.name)}) $name($args) (err error) {")
    out.inc
  }

  override def fetchInstancesHeader(): Unit = errorMethodHeader("FetchInstances")
  override def fetchInstancesFooter(): Unit = {
    out.puts("return err")
    universalFooter
  }

  override def attrInvokeFetchInstances(baseExpr: Ast.expr, exprType: DataType, dataType: DataType): Unit = {
    val raw = expression(baseExpr)
    val value = if (config.readWrite && !exprType.asCombined.isInstanceOf[UserType])
      s"$raw.(${kaitaiType2NativeType(dataType)})" else raw
    out.puts(s"err = $value.FetchInstances()")
    translator.outAddErrCheck()
  }

  override def attrInvokeInstance(instName: InstanceIdentifier): Unit = {
    out.puts(s"_, err = this.${publicMemberName(instName)}()")
    translator.outAddErrCheck()
  }

  override def writeHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = {
    writing = true
    writeMain = endian.isEmpty
    endian match {
      case None =>
        errorMethodHeader("Write", "writer *kaitai.Writer")
        writerCall("this.WriteNoAlign_(writer)")
        writerCall("writer.AlignToByte()")
        out.puts("return err")
        universalFooter
        out.puts
        out.puts("// WriteNoAlign_ serializes into a shared writer without final byte alignment.")
        out.puts("// Generated parents use this method for nested objects; callers finishing")
        out.puts("// an independent stream should use Write or finalize the writer themselves.")
        errorMethodHeader("WriteNoAlign_", "writer *kaitai.Writer")
        out.puts("if !this._checked {")
        out.inc
        out.puts("return kaitai.NewConsistencyNotCheckedError(this)")
        universalFooter
        out.puts("previousWriter := this._activeWriter")
        parseInstances.foreach { id =>
          val name = idToStr(id)
          out.puts(s"previousPending_$name := this._w_$name")
        }
        ioValueInstances.foreach { id =>
          val name = idToStr(id)
          out.puts(s"previousValue_$name, previousFlag_$name := this._wv_$name, this._wf_$name")
          out.puts(s"this._wv_$name, this._wf_$name = nil, false")
        }
        out.puts("this._activeWriter = writer")
        out.puts("defer func() {")
        out.inc
        out.puts("this._activeWriter = previousWriter")
        ioValueInstances.foreach { id =>
          val name = idToStr(id)
          out.puts(s"this._wv_$name, this._wf_$name = nil, false")
        }
        out.puts("if previousWriter != nil {")
        out.inc
        out.puts("this._writer = previousWriter")
        parseInstances.foreach { id =>
          val name = idToStr(id)
          out.puts(s"this._w_$name = previousPending_$name")
        }
        ioValueInstances.foreach { id =>
          val name = idToStr(id)
          out.puts(s"this._wv_$name, this._wf_$name = previousValue_$name, previousFlag_$name")
        }
        universalFooter
        out.dec
        out.puts("}()")
        out.puts("this._writer = writer")
        typeProvider.nowClass.params.filter(p => writerParam(typeProvider.nowClass, p.id.humanReadable).isDefined).foreach { p =>
          importList.add("fmt")
          val field = s"this._writer_${idToStr(p.id)}"
          val isArray = p.dataType.isInstanceOf[ArrayType]
          if (isArray) {
            out.puts(s"for _, output := range $field {")
            out.inc
          }
          out.puts(s"if ${if (isArray) "output" else field} == nil {")
          out.inc
          out.puts(s"return fmt.Errorf(\"missing output stream parameter ${p.id.humanReadable}: %w\", kaitai.ErrWriterNotSeekable)")
          universalFooter
          if (isArray) universalFooter
        }
      case Some(e) => errorMethodHeader(s"_write_${e.toSuffix}")
    }
  }

  override def writeFooter(): Unit = {
    if (writeMain) {
      translator.outAddErrCheck()
    }
    parseInstances.foreach(instanceCheckWriteFlagAndWrite)
    out.puts("return err")
    universalFooter
    writing = false
  }

  override def runWriteCalc(): Unit = {
    typeProvider.nowClass.meta.endian match {
      case Some(ce: CalcEndian) =>
        out.puts("this._is_le = -1")
        def render(e: FixedEndian): Unit =
          out.puts(s"this._is_le = ${if (e == LittleEndian) 1 else 0}")
        switchCases[FixedEndian](EndianIdentifier, ce.on, ce.cases, render, render)
      case Some(InheritedEndian) =>
        out.puts("this._is_le = this._parent._is_le")
      case _ =>
    }
    out.puts("switch this._is_le {")
    out.puts("case 0:")
    out.inc
    out.puts("err = this._write_be()")
    out.dec
    out.puts("case 1:")
    out.inc
    out.puts("err = this._write_le()")
    out.dec
    out.puts("default:")
    out.inc
    out.puts(s"err = ${GoCompiler.ksErrorName(UndecidedEndiannessError)}{}")
    out.dec
    out.puts("}")
  }

  override def checkHeader(): Unit = {
    checking = true
    errorMethodHeader("Check")
    out.puts("this._checked = false")
  }

  override def checkFooter(): Unit = {
    out.puts("this._checked = true")
    out.puts("return err")
    universalFooter
    checking = false
  }

  override def condRepeatCommonHeader(id: Identifier, io: String, dataType: DataType): Unit = {
    out.puts(s"for i := range ${privateMemberName(id)} {")
    out.inc
    out.puts("_ = i")
  }

  override def instanceWriteFlagDeclaration(attrName: InstanceIdentifier): Unit = {
    out.puts(s"_w_${idToStr(attrName)} bool")
    out.puts(s"_enabled_${idToStr(attrName)} bool")
  }

  override def instanceWriteFlagInit(instName: InstanceIdentifier): Unit =
    instanceClearWriteFlag(instName)

  override def instanceSetWriteFlag(instName: InstanceIdentifier): Unit = {
    out.puts(s"this._w_${idToStr(instName)} = this._enabled_${idToStr(instName)} && this.${calculatedFlagForName(instName)}")
  }

  override def instanceClearWriteFlag(instName: InstanceIdentifier): Unit =
    out.puts(s"this._w_${idToStr(instName)} = false")

  override def instanceEnabledSetter(instName: InstanceIdentifier): Unit = {
    out.puts(s"func (this *${types2class(typeProvider.nowClass.name)}) Set${publicMemberName(instName)}Enabled_(v bool) {")
    out.inc
    out.puts(s"this._enabled_${idToStr(instName)} = v")
    instanceSetWriteFlag(instName)
    out.puts("this._checked = false")
    universalFooter
  }

  override def instanceCheckWriteFlagAndWrite(instName: InstanceIdentifier): Unit = {
    out.puts(s"if this._activeWriter != nil && this._w_${idToStr(instName)} {")
    out.inc
    writerCall(s"this._write${publicMemberName(instName)}()")
    universalFooter
  }

  override def writeInstanceHeader(instName: InstanceIdentifier): Unit = {
    writing = true
    writingInstance = Some(instName)
    val dataType = typeProvider.nowClass.instances(instName).dataTypeComposite
    instanceHeader(typeProvider.nowClass.name, instName, dataType, false)
    if (!parseInstance(typeProvider.nowClass, instName.humanReadable)) {
      instanceCheckCacheAndReturn(instName, dataType)
      return
    }
    out.puts(s"if !this.${calculatedFlagForName(instName)} {")
    out.inc
    out.puts(s"return v, kaitai.NewConsistencyError(${expression(Ast.expr.Str(instName.humanReadable))}, \"materialized instance\", \"unset instance\")")
    universalFooter
    instanceCheckWriteFlagAndWrite(instName)
    instanceReturn(instName, dataType, false)
    universalFooter
    errorMethodHeader(s"_write${publicMemberName(instName)}")
    // Clear before evaluating expressions to avoid recursive writes when an
    // instance refers to its own cached value; retry failed writes next time.
    instanceClearWriteFlag(instName)
    out.puts("defer func() {")
    out.inc
    out.puts("if err != nil {")
    out.inc
    instanceSetWriteFlag(instName)
    universalFooter
    out.dec
    out.puts("}()")
  }

  override def writeInstanceFooter(): Unit = {
    val instName = writingInstance.get
    if (parseInstance(typeProvider.nowClass, instName.humanReadable)) {
      out.puts("return err")
    } else {
      instanceSetCalculated(instName)
      instanceReturn(instName, typeProvider.nowClass.instances(instName).dataTypeComposite, false)
    }
    universalFooter
    writingInstance = None
    writing = false
  }

  override def checkInstanceHeader(instName: InstanceIdentifier): Unit = {
    out.puts(s"if this._enabled_${idToStr(instName)} && this.${calculatedFlagForName(instName)} {")
    out.inc
  }

  override def instanceInvalidate(instName: InstanceIdentifier): Unit = {
    out.puts
    val suffix = if (parseInstance(typeProvider.nowClass, instName.humanReadable)) "_" else ""
    out.puts(s"func (this *${types2class(typeProvider.nowClass.name)}) Invalidate${publicMemberName(instName)}$suffix() {")
    out.inc
    out.puts(s"this.${calculatedFlagForName(instName)} = false")
    if (ioValueInstances.exists(_ == instName)) {
      val name = idToStr(instName)
      out.puts(s"this._wv_$name, this._wf_$name = nil, false")
    }
    if (suffix.nonEmpty) instanceClearWriteFlag(instName)
    out.puts("this._checked = false")
    universalFooter
  }

  // The lifecycle can be generated before the field-writing and consistency
  // hooks are implemented. Never silently accept an unsupported operation.
  private def unsupportedSerialization(feature: String): Unit = {
    importList.add("errors")
    out.puts(s"err = errors.New(\"Go serialization $feature is not implemented\")")
    translator.outAddErrCheck()
  }

  override def internalEnumIntType(basedOn: IntType): DataType = basedOn

  override def handleAssignment(id: Identifier, expr: String, rep: RepeatSpec, isRaw: Boolean, exprType: DataType, assignType: DataType): Unit =
    handleAssignment(id, ResultString(expr), rep, isRaw)

  private def writerCall(call: String): Unit = {
    out.puts(s"err = $call")
    translator.outAddErrCheck()
  }

  override def attrPrimitiveWrite(io: String, expr: Ast.expr, dt: DataType, defEndian: Option[FixedEndian], exprTypeOpt: Option[DataType]): Unit = {
    lazy val value = {
      val raw = expression(expr)
      if (translator.detectType(expr).asCombined == AnyType) s"$raw.(${kaitaiType2NativeType(dt)})" else raw
    }
    val call = dt match {
      case t: ReadableType =>
        s"$io.Write${Utils.capitalize(t.apiCall(defEndian))}(${kaitaiType2NativeType(t)}($value))"
      case BitsType1(endian) =>
        val asInt = translator.localVarName(translator.allocateLocalVar())
        out.puts(s"$asInt := uint64(0)")
        out.puts(s"if $value {")
        out.inc
        out.puts(s"$asInt = 1")
        universalFooter
        s"$io.WriteBitsInt${Utils.upperCamelCase(endian.toSuffix)}(1, $asInt)"
      case BitsType(width, endian) =>
        s"$io.WriteBitsInt${Utils.upperCamelCase(endian.toSuffix)}($width, uint64($value))"
      case _: BytesType => s"$io.WriteBytes($value)"
    }
    writerCall(call)
  }

  override def attrBytesLimitWrite(io: String, expr: Ast.expr, size: String, term: Int, padRight: Int): Unit =
    writerCall(s"$io.WriteBytesLimit(${expression(expr)}, int($size), $term, $padRight)")

  override def attrSwitchTypeParse(id: Identifier, on: Ast.expr, cases: Map[Ast.expr, DataType], io: String, rep: RepeatSpec, defEndian: Option[FixedEndian], isNullable: Boolean, assignType: SwitchType): Unit = {
    if (config.readWrite) {
      // Read-write switches share one typed/interface field between Read and
      // Write, including their default byte-array case.
      def parse(t: DataType): Unit = attrParse2(id, t, io, rep, false, defEndian, Some(assignType))
      switchCases[DataType](id, on, cases, parse, parse)
    } else {
      super.attrSwitchTypeParse(id, on, cases, io, rep, defEndian, isNullable, assignType)
    }
  }

  override def attrBytesTypeWrite2(id: Identifier, io: String, expr: Ast.expr, t: BytesType, checksShouldDependOnIo: Option[Boolean], exprTypeOpt: Option[DataType]): Unit = t match {
    // Size translation can emit statements in Go. Plain writes need the size
    // only for the consistency check; do not evaluate it a second time as an
    // unused argument to the padding hook.
    case b: BytesLimitType if b.padRight.isEmpty && b.terminator.isEmpty =>
      attrBytesCheck(id, expr, t, checksShouldDependOnIo)
      attrPrimitiveWrite(io, expr, t, None, exprTypeOpt)
    case b: BytesEosType if b.padRight.isEmpty && b.terminator.isEmpty =>
      attrBytesCheck(id, expr, t, checksShouldDependOnIo)
      attrPrimitiveWrite(io, expr, t, None, exprTypeOpt)
      attrIsEofCheck(id, true, io)
    case _ => super.attrBytesTypeWrite2(id, io, expr, t, checksShouldDependOnIo, exprTypeOpt)
  }

  private def writerQuery(writer: String, eof: Boolean): String = {
    importList.add("io")
    importList.add("fmt")
    val resultType = if (eof) "bool" else "int64"
    val zero = if (eof) "false" else "0"
    val result = if (eof) "pos >= size" else "size"
    // Writer.Seek aligns pending bits. Query its underlying seeker instead so
    // observing size/EOF cannot insert padding in the middle of a bit group.
    val call = s"""func() ($resultType, error) {
                  |\tif $writer == nil {
                  |\t\treturn $zero, fmt.Errorf("query missing output stream: %w", kaitai.ErrWriterNotSeekable)
                  |\t}
                  |\tseeker, ok := $writer.Writer.(io.Seeker)
                  |\tif !ok {
                  |\t\treturn $zero, fmt.Errorf("query output stream: %w", kaitai.ErrWriterNotSeekable)
                  |\t}
                  |\tpos, err := seeker.Seek(0, io.SeekCurrent)
                  |\tif err != nil {
                  |\t\treturn $zero, fmt.Errorf("query output position: %w", err)
                  |\t}
                  |\tsize, seekErr := seeker.Seek(0, io.SeekEnd)
                  |\t_, restoreErr := seeker.Seek(pos, io.SeekStart)
                  |\tif seekErr != nil {
                  |\t\treturn $zero, fmt.Errorf("query output size: %w", seekErr)
                  |\t}
                  |\tif restoreErr != nil {
                  |\t\treturn $zero, fmt.Errorf("restore output position: %w", restoreErr)
                  |\t}
                  |\treturn $result, nil
                  |}()""".stripMargin
    translator.resToStr(translator.outVarCheckRes(call))
  }

  private def writerSize(io: String): String = writerQuery(io, eof = false)

  private def writerEof(io: String): String = writerQuery(io, eof = true)

  override def exprIORemainingSize(io: String): String = {
    val pos = translator.resToStr(translator.outVarCheckRes(s"$io.Pos()"))
    s"(${writerSize(io)} - $pos)"
  }

  override def attrUserTypeWrite(id: Identifier, t: UserType, io: String, rep: RepeatSpec, isRaw: Boolean, defEndian: Option[FixedEndian], checksShouldDependOnIo: Option[Boolean], exprTypeOpt: Option[DataType]): Unit = {
    val item = Identifier.itemExpr(id, rep)
    val expr = if (exprTypeOpt.exists(!_.isInstanceOf[UserType]))
      Ast.expr.CastToType(item, Ast.typeId(true, t.classSpec.get.name)) else item
    attrUserTypeCheck(id, expr, t, checksShouldDependOnIo)
    t.classSpec.get.params.lazyZip(t.args).foreach { (param, arg) =>
      getArrayItemType(param.dataType) match {
        case KaitaiStreamType | OwnedKaitaiStreamType => attrSetProperty(expr, param.id, expression(arg))
        case _ =>
      }
    }
    t match {
      case _: UserTypeInstream => attrUserTypeInstreamWrite(io, expr, t, t)
      case ut: UserTypeFromBytes =>
        val bt = ut.bytes
        val outer = localValue(bt match {
          case b: BytesLimitType => s"int(${expression(b.size)})"
          case _: BytesEosType => s"int(${exprIORemainingSize(io)})"
          case _: BytesTerminatedType => expression(Identifier.itemExpr(OuterSizeIdentifier(id), rep))
        })
        val inner = if (writeNeedsInnerSize(bt)) expression(Identifier.itemExpr(InnerSizeIdentifier(id), rep)) else outer
        val extraTerm = bt match {
          case b: BytesTerminatedType if !b.include =>
            if (b.eosError) "true" else {
              val pos = translator.resToStr(translator.outVarCheckRes(s"$io.Pos()"))
              localValue(s"$pos + int64($outer) < ${writerSize(io)}")
            }
          case _ => "false"
        }
        val reserved = if (extraTerm == "false") outer else {
          val size = localValue(outer)
          out.puts(s"if $extraTerm {")
          out.inc
          out.puts(s"$size++")
          universalFooter
          size
        }
        val encoder = translator.localVarName(translator.allocateLocalVar())
        val rawId = RawIdentifier(id)
        importList.add("bytes")
        out.puts(s"$encoder := kaitai.ByteEncoderFunc(func(data []byte) ([]byte, error) {")
        out.inc
        out.puts("buffer := kaitai.NewSeekableBuffer(nil)")
        out.puts("output := kaitai.NewWriter(buffer)")
        // The inner error-only closure permits the ordinary checks/writes to
        // retain their early-return contract inside a ByteEncoder.
        out.puts("err := func() (err error) {")
        out.inc
        assignWrittenBytes(rawId, rep, "bytes.Clone(data)")
        val encodedId = bt.process match {
          case Some(proc) =>
            val dest = RawIdentifier(rawId)
            attrUnprocess(proc, rawId, dest, rep, bt, None)
            dest
          case None => rawId
        }
        val encoded = Identifier.itemExpr(encodedId, rep)
        // A reservation advances the parent. Its size expression must be
        // checked against the value evaluated before that advancement.
        capturedChildSize = bt match {
          case b: BytesLimitType if userExprDependsOnIo(b.size) => Some((b.size, outer))
          case _ => None
        }
        try attrBytesCheck(id, encoded, bt, None) finally capturedChildSize = None
        bt match {
          case b: BytesLimitType => attrBytesLimitWrite2("output", encoded, b, outer, b.padRight, b.terminator, b.include, None)
          case b: BytesEosType => attrBytesLimitWrite2("output", encoded, b, outer, b.padRight, b.terminator, b.include, None)
          case b: BytesTerminatedType =>
            attrPrimitiveWrite("output", encoded, bt, None, None)
            if (b.include && !b.eosError) {
              // Missing included terminators are valid only at the end of the
              // parent reservation's stream, not merely the encoder's buffer.
              condIfHeader(Ast.expr.Compare(exprByteArrayIndexOf(encoded, b.terminator.head & 0xff), Ast.cmpop.Eq, Ast.expr.IntNum(-1)))
              attrIsEofCheck(id, true, io)
              condIfFooter
            }
            if (extraTerm != "false") {
              out.puts(s"if $extraTerm {")
              out.inc
              attrPrimitiveWrite("output", Ast.expr.IntNum(b.terminator.head & 0xff), Int1Type(false), None, None)
              universalFooter
            }
        }
        out.puts("return nil")
        out.dec
        out.puts("}()")
        out.puts("return buffer.Bytes(), err")
        out.dec
        out.puts("})")
        val child = translator.resToStr(translator.outVarCheckRes(s"$io.NewChild(int($inner), $reserved, $encoder)"))
        attrUserTypeInstreamWrite(s"$child.Writer", expr, t, t)
        writerCall(s"$child.WriteBack()")
        bt match {
          case b: BytesTerminatedType if !b.consume =>
            val hasTerm = if (!b.include) extraTerm else {
              val data = expression(Identifier.itemExpr(encodedId, rep))
              s"len($data) > 0 && $data[len($data) - 1] == ${b.terminator.head & 0xff}"
            }
            out.puts(s"if $hasTerm {")
            out.inc
            importList.add("io")
            out.puts(s"_, err = $io.Seek(-1, io.SeekCurrent)")
            translator.outAddErrCheck()
            universalFooter
          case _ =>
        }
    }
  }

  override def attrUserTypeInstreamWrite(io: String, expr: Ast.expr, t: DataType, exprType: DataType): Unit =
    writerCall(s"${expression(expr)}.WriteNoAlign_($io)")

  override def exprStreamToByteArray(ioFixed: String): String =
    throw new NotImplementedError("Go serialization child writer extraction")

  private def localValue(value: String): String = {
    val name = translator.localVarName(translator.allocateLocalVar())
    out.puts(s"$name := $value")
    name
  }

  private def assignWrittenBytes(id: Identifier, rep: RepeatSpec, value: String): Unit = {
    val dest = privateMemberName(id)
    rep match {
      case NoRepeat => out.puts(s"$dest = $value")
      case _ =>
        out.puts(s"for len($dest) <= i {")
        out.inc
        out.puts(s"$dest = append($dest, nil)")
        universalFooter
        out.puts(s"$dest[i] = $value")
    }
  }

  private def customProcessor(name: List[String], args: Seq[Ast.expr]): String = {
    val namespace = name.init.mkString(".")
    if (namespace.nonEmpty) importList.add(name.init.mkString("/"))
    s"${if (namespace.nonEmpty) name(name.size - 2) + "." else ""}New${type2class(name.last)}(${args.map(expression).mkString(", ")})"
  }

  override def attrUnprocess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec, dataType: BytesType, exprTypeOpt: Option[DataType]): Unit = {
    val raw = expression(Identifier.itemExpr(varSrc, rep))
    val src = if (exprTypeOpt.exists(!_.isInstanceOf[BytesType])) s"$raw.([]byte)" else raw
    val value = proc match {
      case ProcessXor(key) =>
        val arg = translator.detectType(key) match {
          case _: IntType => s"[]byte{byte(${expression(key)})}"
          case _ => expression(key)
        }
        s"kaitai.ProcessXOR($src, $arg)"
      case ProcessRotate(left, count) =>
        s"kaitai.ProcessRotate${if (left) "Right" else "Left"}($src, int(${expression(count)}))"
      case ProcessZlib => translator.resToStr(translator.outVarCheckRes(s"kaitai.UnprocessZlib($src)"))
      case ProcessCustom(name, args) =>
        importList.add("bytes")
        translator.resToStr(translator.outVarCheckRes(s"${customProcessor(name, args)}.Encode(bytes.Clone($src))"))
    }
    assignWrittenBytes(varDest, rep, value)
  }

  override def attrUnprocessPrepareBeforeSubIOHandler(proc: ProcessExpr, varSrc: Identifier): Unit =
    unsupportedSerialization("child writer reverse processing")

  override def condIfIsEofHeader(io: String, wantedIsEof: Boolean): Unit = {
    val eof = writerEof(io)
    out.puts(s"if ($eof) == $wantedIsEof {")
    out.inc
  }
  override def condIfIsEofFooter: Unit = universalFooter

  override def attrSetProperty(base: Ast.expr, propName: Identifier, value: String): Unit =
    out.puts(s"${expression(base)}._writer_${idToStr(propName)} = $value")

  override def attrBasicCheck(checkExpr: Ast.expr, actual: Ast.expr, expected: Ast.expr, msg: String): Unit = {
    val failCond = checkExpr match {
      case Ast.expr.Compare(left @ Ast.expr.Attribute(_, Ast.identifier("size")), op, right)
        if !right.isInstanceOf[Ast.expr.IntNum] =>
        // len returns int, whereas a declared length/count can be any integer
        // width. Widen both operands so a large slice never wraps at uint8/16.
        val wideType = translator.detectType(right) match {
          case Int1Type(false) | IntMultiType(false, _, _) | _: BitsType => "uint64"
          case _ => "int64"
        }
        s"$wideType(${expression(left)}) ${translator.cmpOp(op)} $wideType(${expression(right)})"
      case _ => expression(checkExpr)
    }
    out.puts(s"if $failCond {")
    out.inc
    out.puts(s"return kaitai.NewConsistencyError(${expression(Ast.expr.Str(msg))}, ${expression(expected)}, ${expression(actual)})")
    universalFooter
  }

  override def attrObjectsEqualCheck(actual: Ast.expr, expected: Ast.expr, msg: String): Unit = {
    val actualStr = expression(actual)
    val expectedStr = expression(expected)
    val failCond = translator.detectType(actual) match {
      case t: ArrayType => s"!(${consistencyEquals(actualStr, expectedStr, t)})"
      case AnyType => s"!(${consistencyEquals(actualStr, expectedStr, AnyType)})"
      case _ => s"$actualStr != $expectedStr"
    }
    out.putsLines("", s"if $failCond {")
    out.inc
    out.puts(s"return kaitai.NewConsistencyError(${expression(Ast.expr.Str(msg))}, $expectedStr, $actualStr)")
    universalFooter
  }

  private def consistencyEquals(actual: String, expected: String, dataType: DataType): String = dataType match {
    case t: ArrayType =>
      val nativeType = kaitaiType2NativeType(t)
      val elementEquals = consistencyEquals("a[i]", "b[i]", t.elType).replace("\n", "\n\t\t")
      s"""func(a, b $nativeType) bool {
         |\tif (a == nil) != (b == nil) || len(a) != len(b) {
         |\t\treturn false
         |\t}
         |\tfor i := range a {
         |\t\tif !($elementEquals) {
         |\t\t\treturn false
         |\t\t}
         |\t}
         |\treturn true
         |}($actual, $expected)""".stripMargin
    case _: BytesType =>
      importList.add("bytes")
      s"bytes.Equal($actual, $expected)"
    case AnyType =>
      // Only Any needs runtime type inspection. Recurse through collections,
      // but use Go equality for pointer leaves, preserving object identity.
      importList.add("reflect")
      s"""func(a, b interface{}) bool {
         |\ttype slicePair struct {
         |\t\ta, b uintptr
         |\t\tlength int
         |\t\ttyp reflect.Type
         |\t}
         |\tvisited := make(map[slicePair]bool)
         |\tvar equal func(reflect.Value, reflect.Value) bool
         |\tequal = func(a, b reflect.Value) bool {
         |\t\tif !a.IsValid() || !b.IsValid() {
         |\t\t\treturn a.IsValid() == b.IsValid()
         |\t\t}
         |\t\tif a.Type() != b.Type() {
         |\t\t\treturn false
         |\t\t}
         |\t\tswitch a.Kind() {
         |\t\tcase reflect.Interface:
         |\t\t\treturn equal(a.Elem(), b.Elem())
         |\t\tcase reflect.Slice, reflect.Array:
         |\t\t\tif a.Kind() == reflect.Slice && a.IsNil() != b.IsNil() {
         |\t\t\t\treturn false
         |\t\t\t}
         |\t\t\tif a.Len() != b.Len() {
         |\t\t\t\treturn false
         |\t\t\t}
         |\t\t\tif a.Kind() == reflect.Slice {
         |\t\t\t\tpair := slicePair{a.Pointer(), b.Pointer(), a.Len(), a.Type()}
         |\t\t\t\tif visited[pair] {
         |\t\t\t\t\treturn true
         |\t\t\t\t}
         |\t\t\t\tvisited[pair] = true
         |\t\t\t}
         |\t\t\tfor i := 0; i < a.Len(); i++ {
         |\t\t\t\tif !equal(a.Index(i), b.Index(i)) {
         |\t\t\t\t\treturn false
         |\t\t\t\t}
         |\t\t\t}
         |\t\t\treturn true
         |\t\tdefault:
         |\t\t\treturn a.Comparable() && b.Comparable() && a.Interface() == b.Interface()
         |\t\t}
         |\t}
         |\treturn equal(reflect.ValueOf(a), reflect.ValueOf(b))
         |}($actual, $expected)""".stripMargin
    case _ => s"$actual == $expected"
  }

  override def attrIsEofCheck(io: String, expectedIsEof: Boolean, msg: String): Unit = {
    val eof = writerEof(io)
    out.puts(s"if ($eof) != $expectedIsEof {")
    out.inc
    out.puts(s"return kaitai.NewConsistencyError(${expression(Ast.expr.Str(msg))}, $expectedIsEof, !$expectedIsEof)")
    universalFooter
  }

  override def attrUserTypeCheck(id: Identifier, utExpr: Ast.expr, ut: UserType, shouldDependOnIo: Option[Boolean]): Unit = {
    if (!shouldDependOnIo.contains(true)) {
      val value = expression(utExpr)
      out.puts(s"if $value == nil {")
      out.inc
      out.puts(s"return kaitai.NewConsistencyError(${expression(Ast.expr.Str(id.humanReadable))}, \"non-nil value\", $value)")
      universalFooter
    }
    super.attrUserTypeCheck(id, utExpr, ut, shouldDependOnIo)
  }

  override def attrParentParamCheck(actualParentExpr: Ast.expr, ut: UserType, shouldDependOnIo: Option[Boolean], msg: String): Unit = {
    val dependsOnIo = ut.forcedParent match {
      case Some(USER_TYPE_NO_PARENT) | None => false
      case Some(fp) => userExprDependsOnIo(fp)
    }
    if (shouldDependOnIo.exists(_ != dependsOnIo))
      return
    // Go expression translation may emit statements, including IO calls.
    // Classify the expression before translating a deferred check.
    val expectedParent = ut.forcedParent match {
      case Some(USER_TYPE_NO_PARENT) => "nil"
      case Some(fp) => expression(fp)
      case None => "this"
    }
    val actualParent = expression(actualParentExpr)
    out.puts(s"if $actualParent != $expectedParent {")
    out.inc
    out.puts(s"return kaitai.NewConsistencyError(${expression(Ast.expr.Str(msg))}, $expectedParent, $actualParent)")
    universalFooter
  }

  override def universalDoc(doc: DocSpec): Unit = {
    out.puts
    out.puts( "/**")

    doc.summary.foreach(summary => out.putsLines(" * ", summary))

    doc.ref.foreach {
      case TextRef(text) =>
        out.putsLines(" * ", "@see \"" + text + "\"")
      case ref: UrlRef =>
        out.putsLines(" * ", s"@see ${ref.toAhref}")
    }

    out.puts( " */")
  }

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {
    out.puts(s"switch ${privateMemberName(EndianIdentifier)} {")
    out.puts("case 0:")
    out.inc
    beProc()
    out.dec
    out.puts("case 1:")
    out.inc
    leProc()
    out.dec
    out.puts("default:")
    out.inc
    out.puts(s"err = ${GoCompiler.ksErrorName(UndecidedEndiannessError)}{}")
    out.dec
    out.puts("}")
  }

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, rep: RepeatSpec): String = {
    val srcExpr = getRawIdExpr(varSrc, rep)

    proc match {
      case ProcessXor(xorValue) =>
        translator.detectType(xorValue) match {
          case _: IntType =>
            s"kaitai.ProcessXOR($srcExpr, []byte{${translator.translateAs(xorValue, Int1Type(false))}})"
          case _: BytesType =>
            s"kaitai.ProcessXOR($srcExpr, ${expression(xorValue)})"
        }
      case ProcessZlib =>
        translator.resToStr(translator.outVarCheckRes(s"kaitai.ProcessZlib($srcExpr)"))
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        s"kaitai.ProcessRotateLeft($srcExpr, int($expr))"
      case ProcessCustom(name, args) =>
        if (config.readWrite) {
          importList.add("bytes")
          translator.resToStr(translator.outVarCheckRes(s"${customProcessor(name, args)}.Decode(bytes.Clone($srcExpr))"))
        } else {
          // Preserve the historical read-only custom processor behavior.
          s"$srcExpr"
        }
    }
  }

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = {
    val javaName = privateMemberName(varName)

    val ioName = idToStr(IoStorageIdentifier(varName))

    val args = rep match {
      case RepeatUntil(_) => translator.specialName(Identifier.ITERATOR2)
      case _ => getRawIdExpr(varName, rep)
    }

    importList.add("bytes")

    out.puts(s"$ioName := kaitai.NewStream(bytes.NewReader($args))")
    ioName
  }

  override def attrUserTypeParse(id: Identifier, dataType: UserType, io: String, rep: RepeatSpec, defEndian: Option[FixedEndian]): Unit = {
    super.attrUserTypeParse(id, dataType, io, rep, defEndian)
    if (config.readWrite) dataType match {
      case t: UserTypeFromBytes =>
        val rawId = RawIdentifier(id)
        def storeSize(sizeId: Identifier, raw: Identifier): Unit = {
          val value = ResultString(s"len(${getRawIdExpr(raw, rep)})")
          if (rep == NoRepeat) handleAssignmentSimple(sizeId, value) else handleAssignmentRepeatEos(sizeId, value)
        }
        if (writeNeedsOuterSize(t.bytes)) storeSize(OuterSizeIdentifier(id), if (t.bytes.process.isDefined) RawIdentifier(rawId) else rawId)
        if (writeNeedsInnerSize(t.bytes)) storeSize(InnerSizeIdentifier(id), rawId)
      case _ =>
    }
  }

  def getRawIdExpr(varName: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(varName)
    rep match {
      case NoRepeat => memberName
      case RepeatExpr(_) => s"$memberName[i]"
      case _ => s"$memberName[len($memberName) - 1]"
    }
  }

  override def useIO(ioEx: Ast.expr): String = {
    out.puts(s"thisIo := ${expression(ioEx)}")
    "thisIo"
  }

  override def pushPos(io: String): Unit = {
    out.puts(s"_pos, err := $io.Pos()")
    translator.outAddErrCheck()
  }

  override def seek(io: String, pos: Ast.expr): Unit = {
    importList.add("io")

    out.puts(s"_, err = $io.Seek(int64(${expression(pos)}), io.SeekStart)")
    translator.outAddErrCheck()
  }

  override def popPos(io: String): Unit = {
    importList.add("io")

    out.puts(s"_, err = $io.Seek(_pos, io.SeekStart)")
    translator.outAddErrCheck()
  }

  override def condIfHeader(expr: Ast.expr): Unit = {
    out.puts(s"if (${expression(expr)}) {")
    out.inc
  }

  override def condRepeatInitAttr(id: Identifier, dataType: DataType): Unit = {
    // slices don't have to be manually initialized in Go: the built-in append()
    // function works even on `nil` slices (https://go.dev/tour/moretypes/15)
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType): Unit = {
    out.puts(s"for i := 0;; i++ {")
    out.inc

    val eofVar = translator.allocateLocalVar()
    out.puts(s"${translator.localVarName(eofVar)}, err := this._io.EOF()")
    translator.outAddErrCheck()
    out.puts(s"if ${translator.localVarName(eofVar)} {")
    out.inc
    out.puts("break")
    out.dec
    out.puts("}")
  }

  override def handleAssignmentRepeatEos(id: Identifier, r: TranslatorResult): Unit = {
    val name = privateMemberName(id)
    val expr = translator.resToStr(castInstanceElement(id, r))
    out.puts(s"$name = append($name, $expr)")
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, repeatExpr: Ast.expr): Unit = {
    out.puts(s"for i := 0; i < int(${expression(repeatExpr)}); i++ {")
    out.inc
    // FIXME: Go throws a fatal compile error when the `i` variable is not used (unused variables
    // can only use the blank identifier `_`, see https://go.dev/doc/effective_go#blank), so we have
    // to silence it like this. It would be nice to be able to analyze all expressions that appear
    // in the loop body to decide whether to generate `for _ := range` or `for i := range` here, but
    // that would be really difficult to do properly in KSC with the current architecture.
    out.puts("_ = i")
  }

  override def handleAssignmentRepeatExpr(id: Identifier, r: TranslatorResult): Unit =
    handleAssignmentRepeatEos(id, r)

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, untilExpr: Ast.expr): Unit = {
    // Read-write predicates use the same zero-based index as the slice loop
    // in Check/Write. Keep the historical read-only output unchanged.
    val initialIndex = if (config.readWrite) 0 else 1
    out.puts(s"for i := $initialIndex;; i++ {")
    out.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, r: TranslatorResult, isRaw: Boolean): Unit = {
    val expr = translator.resToStr(castInstanceElement(id, r))
    val tempVar = translator.specialName(if (isRaw) Identifier.ITERATOR2 else Identifier.ITERATOR)
    out.puts(s"$tempVar := $expr")
    out.puts(s"${privateMemberName(id)} = append(${privateMemberName(id)}, $tempVar)")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, untilExpr: Ast.expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.puts(s"if ${expression(untilExpr)} {")
    out.inc
    out.puts("break")
    out.dec
    out.puts("}")
    out.dec
    out.puts("}")
  }

  private def castToType(r: TranslatorResult, dataType: DataType): TranslatorResult = {
    dataType match {
      case t @ (_: IntMultiType | _: FloatMultiType | CalcIntType) =>
        ResultString(s"${kaitaiType2NativeType(t)}(${translator.resToStr(r)})")
      case _ =>
        r
    }
  }

  private def castInstanceElement(id: Identifier, r: TranslatorResult): TranslatorResult = id match {
    case InstanceIdentifier(name) if config.readWrite =>
      typeProvider.determineType(name) match {
        case array: ArrayType => castToType(r, array.elType.asCombined)
        case _ => r
      }
    case _ => r
  }

  private def handleCompositeTypeCast(id: Identifier, r: TranslatorResult): TranslatorResult = {
    id match {
      case NamedIdentifier(name) =>
        castToType(r, typeProvider.determineType(name).asCombined)
      case InstanceIdentifier(name) if config.readWrite =>
        castToType(r, typeProvider.determineType(name).asCombined)
      case _ =>
        r
    }
  }

  override def handleAssignmentSimple(id: Identifier, r: TranslatorResult): Unit = {
    val expr = translator.resToStr(handleCompositeTypeCast(id, r))
    out.puts(s"${privateMemberName(id)} = $expr")
  }

  def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit = {
    out.puts(s"$id := $expr")
    // A repeat-until predicate can depend solely on _index or _io.
    if (config.readWrite && id == translator.specialName(Identifier.ITERATOR))
      out.puts(s"_ = $id")
  }

  override def blockScopeHeader: Unit = {
    out.puts("{")
    out.inc
  }
  override def blockScopeFooter: Unit = universalFooter

  override def parseExpr(dataType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    dataType match {
      case t: ReadableType =>
        s"$io.Read${Utils.capitalize(t.apiCall(defEndian))}()"
      case blt: BytesLimitType =>
        s"$io.ReadBytes(int(${expression(blt.size)}))"
      case _: BytesEosType =>
        s"$io.ReadBytesFull()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        // The single-byte runtime reader drops the last byte (and panics on
        // empty input) at optional EOF. Multi preserves all bytes and position.
        // Keep the established read-only output unchanged.
        if (terminator.length == 1 && !(config.readWrite && !eosError)) {
          val term = terminator.head & 0xff
          s"$io.ReadBytesTerm($term, $include, $consume, $eosError)"
        } else {
          s"$io.ReadBytesTermMulti(${translator.resToStr(translator.doByteArrayLiteral(terminator))}, $include, $consume, $eosError)"
        }
      case BitsType1(bitEndian) =>
        s"$io.ReadBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}(1)"
      case BitsType(width: Int, bitEndian) =>
        s"$io.ReadBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}($width)"
      case t: UserType =>
        val addParams = t.classSpec.get.params.lazyZip(t.args).map((p, a) => translator.translateAs(a, p.dataType)).mkString(", ")
        s"New${GoCompiler.types2class(t.classSpec.get.name)}($addParams)"
    }
  }

//  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean) = {
//    val expr1 = padRight match {
//      case Some(padByte) => s"$kstreamName.bytesStripRight($expr0, (byte) $padByte)"
//      case None => expr0
//    }
//    val expr2 = terminator match {
//      case Some(term) => s"$kstreamName.bytesTerminate($expr1, (byte) $term, $include)"
//      case None => expr1
//    }
//    expr2
//  }

  override def userTypeDebugRead(id: String, t: UserType, io: String): Unit = {
    val (parent, root) = if (t.isExternal(typeProvider.nowClass)) {
      ("nil", "nil")
    } else {
      val parent = t.forcedParent match {
        case Some(USER_TYPE_NO_PARENT) => "nil"
        case Some(fp) => expression(fp)
        case None => "this"
      }
      (parent, "this._root")
    }
    out.puts(s"err = $id.Read($io, $parent, $root)")
  }

  override def switchStart(id: Identifier, on: Ast.expr): Unit = {
    out.puts(s"switch (${expression(on)}) {")
  }

  override def switchCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"case ${expression(condition)}:")
    out.inc
  }

  override def switchCaseEnd(): Unit = {
    out.dec
  }

  override def switchElseStart(): Unit = {
    out.puts("default:")
    out.inc
  }

  override def switchEnd(): Unit =
    out.puts("}")

  override def switchShouldUseCompareFn(onType: DataType): (Option[String], () => Unit) = {
    onType match {
      case _: BytesType =>
        (Some("bytes.Equal"), () => importList.add("bytes"))
      case _ =>
        (None, () => {})
    }
  }

  override def switchCaseStartCompareFn(compareFn: String, switchOn: Ast.expr, condition: Ast.expr): Unit = {
    out.puts(s"case ${compareFn}(${expression(switchOn)}, ${expression(condition)}):")
    out.inc
  }

  override def instanceDeclaration(attrName: InstanceIdentifier, attrType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"${calculatedFlagForName(attrName)} bool")
    out.puts(s"${idToStr(attrName)} ${instanceNativeType(attrName, attrType)}")
    if (config.readWrite && ioValueInstances.exists(_ == attrName)) {
      out.puts(s"_wf_${idToStr(attrName)} bool")
      out.puts(s"_wv_${idToStr(attrName)} ${writerParamType(attrType)}")
    }
  }

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    val prefix = if (writingInstance.contains(instName)) "_writeGet" else ""
    val nativeType = if (writerValueInstance(instName)) writerParamType(dataType) else instanceNativeType(instName, dataType)
    out.puts(s"func (this *${types2class(className)}) $prefix${publicMemberName(instName)}() (v $nativeType, err error) {")
    out.inc
    translator.returnRes = Some((if (config.readWrite) dataType.asCombined else dataType) match {
      case _: NumericType => "0"
      case _: BooleanType => "false"
      case _: StrType => "\"\""
      case _ => "nil"
    })
  }

  override def instanceCalculate(instName: Identifier, dataType: DataType, value: Ast.expr): Unit = {
    val r = translator.translate(value)
    val converted = dataType match {
      case _: UserType => r
      case t if isIoType(t) => r
      case _ => s"${instanceNativeType(instName, dataType)}($r)"
    }
    out.puts(s"${instanceValueName(instName)} = $converted")
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    out.puts(s"if (this.${instanceCacheFlag(instName)}) {")
    out.inc
    instanceReturn(instName, dataType, false)
    universalFooter
    if (config.readWrite && !writing && parseInstance(typeProvider.nowClass, instName.humanReadable)) {
      // Re-reading an invalidated repeated instance must replace its old slice,
      // including the raw/size arrays maintained for processed child streams.
      val inst = typeProvider.nowClass.instances(instName).asInstanceOf[ParseInstanceSpec]
      val attrs = List(AttrSpec(List(), instName, dataType)) ++ ExtraAttrs.forAttr(inst, this)
      attrs.foreach { attr =>
        out.puts("{")
        out.inc
        out.puts(s"var zero ${kaitaiType2NativeType(attr.dataTypeComposite)}")
        out.puts(s"${privateMemberName(attr.id)} = zero")
        universalFooter
      }
    }
  }

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"return ${instanceValueName(instName)}, nil")
  }

  override def instanceSetCalculated(instName: InstanceIdentifier): Unit = {
    val cached = if (writerValueInstance(instName)) "this._activeWriter != nil" else "true"
    out.puts(s"this.${instanceCacheFlag(instName)} = $cached")
  }

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(BigInt, EnumValueSpec)]): Unit = {
    val fullEnumName: List[String] = curClass ++ List(enumName)
    val fullEnumNameStr = types2class(fullEnumName)

    out.puts
    val nativeType = if (config.readWrite && unsignedEnum(enumColl.map(_._1))) "uint64" else "int"
    out.puts(s"type $fullEnumNameStr $nativeType")
    out.puts("const (")
    out.inc

    enumColl.foreach { case (id, label) =>
      out.puts(s"${enumToStr(fullEnumName, label.name)} $fullEnumNameStr = $id")
    }

    out.dec
    out.puts(")")
    // Inspired by https://gist.github.com/bgadrian/cb8b9344d9c66571ef331a14eb7a2e80
    val mapEntriesStr = enumColl.map { case (id, _) => s"$id: {}" }.mkString(", ")
    out.puts(s"var values_$fullEnumNameStr = map[$fullEnumNameStr]struct{}{$mapEntriesStr}")
    out.puts(s"func (v $fullEnumNameStr) isDefined() bool {")
    out.inc
    out.puts(s"_, ok := values_$fullEnumNameStr[v]")
    out.puts("return ok")
    out.dec
    out.puts("}")
  }

  override def classToString(toStringExpr: Ast.expr): Unit = {
    out.puts
    out.puts(s"func (this ${types2class(typeProvider.nowClass.name)}) String() string {")
    out.inc
    out.puts(s"return ${translator.translate(toStringExpr)}")
    out.dec
    out.puts("}")
  }

  override def idToStr(id: Identifier): String =
    id match {
      case InnerSizeIdentifier(inner) if config.readWrite => s"${GoCompiler.idToStr(inner)}_InnerSize"
      case OuterSizeIdentifier(inner) if config.readWrite => s"${GoCompiler.idToStr(inner)}_OuterSize"
      case _ => if (isPrivateSequenceField(id)) s"_m_${GoCompiler.idToStr(id)}" else GoCompiler.idToStr(id)
    }

  override def publicMemberName(id: Identifier): String =
    id match {
      case IoIdentifier => "_IO"
      case RootIdentifier => "_Root"
      case ParentIdentifier => "_Parent"
      case InstanceIdentifier(name) => Utils.upperCamelCase(name)
      case _ => GoCompiler.idToStr(id)
    }

  override def privateMemberName(id: Identifier): String = s"this.${idToStr(id)}"

  override def localTemporaryName(id: Identifier): String = s"_t_${idToStr(id)}"

  override def paramName(id: Identifier): String = Utils.lowerCamelCase(id.humanReadable)

  def calculatedFlagForName(id: Identifier) = s"_f_${idToStr(id)}"

  override def ksErrorName(err: KSError): String = GoCompiler.ksErrorName(err)

  override def attrValidateExpr(
    attr: AttrLikeSpec,
    checkExpr: Ast.expr,
    err: KSError,
    useIo: Boolean,
    actual: Ast.expr,
    expected: Option[Ast.expr] = None
  ): Unit =
    attrValidate(attr, s"!(${translator.translate(checkExpr)})", err, useIo, actual, expected)

  override def attrValidateInEnum(
    attr: AttrLikeSpec,
    et: EnumType,
    valueExpr: Ast.expr,
    err: ValidationNotInEnumError,
    useIo: Boolean
  ): Unit =
    attrValidate(attr, s"!${translator.translate(valueExpr)}.isDefined()", err, useIo, valueExpr, None)

  private def attrValidate(
    attr: AttrLikeSpec,
    failCondExpr: String,
    err: KSError,
    useIo: Boolean,
    actual: Ast.expr,
    expected: Option[Ast.expr]
  ): Unit = {
    if (config.readWrite && !useIo) {
      val field = expression(Ast.expr.Str(attr.id.humanReadable))
      val expectedStr = expected.map(expression).getOrElse("true")
      val actualStr = expression(actual)
      val context = expression(Ast.expr.Str(attr.path.mkString("/", "/", "") + ": %w"))
      importList.add("fmt")
      out.puts(s"if $failCondExpr {")
      out.inc
      out.puts(s"return fmt.Errorf($context, kaitai.NewConsistencyError($field, $expectedStr, $actualStr))")
      universalFooter
      return
    }
    val errArgsStr = expected.map(expression) ++ List(
      expression(actual),
      if (useIo) expression(Ast.expr.InternalName(IoIdentifier)) else "nil",
      expression(Ast.expr.Str(attr.path.mkString("/", "/", "")))
    )
    out.puts(s"if $failCondExpr {")
    out.inc
    val errInst = s"kaitai.New${err.name}(${errArgsStr.mkString(", ")})"
    val noValueAndErr = translator.returnRes match {
      case None => errInst
      case Some(r) => s"$r, $errInst"
    }
    out.puts(s"return $noValueAndErr")
    out.dec
    out.puts("}")
  }

  def ioAccessor(): Unit = {
    out.puts
    out.puts(s"func (this ${types2class(typeProvider.nowClass.name)}) IO_() *$kstreamName {")
    out.inc
    out.puts(s"return this._io")
    out.dec
    out.puts("}")
    if (config.readWrite) {
      out.puts
      // Retain the writer after a child scope ends so output metadata remains
      // available to other objects. Never substitute the input Stream here.
      out.puts(s"func (this *${types2class(typeProvider.nowClass.name)}) Writer_() *kaitai.Writer {")
      out.inc
      out.puts("if this == nil { return nil }")
      out.puts("return this._writer")
      out.dec
      out.puts("}")
    }
  }

  def parentAccessor(): Unit = {
    out.puts
    // The trailing underscore cannot collide with KSY-derived public names.
    // Preserve the actual parent pointer and normalize nil concrete parents
    // before converting them to the optional generic capability's result.
    out.puts(s"func (this *${types2class(typeProvider.nowClass.name)}) Parent_() kaitai.Struct {")
    out.inc
    out.puts("if this == nil || this._parent == nil {")
    out.inc
    out.puts("return nil")
    out.dec
    out.puts("}")
    out.puts("return this._parent")
    out.dec
    out.puts("}")
  }
}

object GoCompiler extends LanguageCompilerStatic
  with UpperCamelCaseClasses
  with StreamStructNames
  with ExceptionNames {

  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new GoCompiler(tp, config)

  def idToStr(id: Identifier): String =
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => Utils.upperCamelCase(name)
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => Utils.lowerCamelCase(name)
      case RawIdentifier(innerId) => s"_raw_${idToStr(innerId)}"
      case IoStorageIdentifier(innerId) => s"_io_${idToStr(innerId)}"
    }

  def privateMemberName(id: Identifier): String = s"this.${idToStr(id)}"

  /**
    * Determine Go data type corresponding to a KS data type.
    *
    * @param attrType KS data type
    * @return Go data type
    */
  def kaitaiType2NativeType(attrType: DataType): String = {
    attrType match {
      case Int1Type(false) => "uint8"
      case IntMultiType(false, Width2, _) => "uint16"
      case IntMultiType(false, Width4, _) => "uint32"
      case IntMultiType(false, Width8, _) => "uint64"

      case Int1Type(true) => "int8"
      case IntMultiType(true, Width2, _) => "int16"
      case IntMultiType(true, Width4, _) => "int32"
      case IntMultiType(true, Width8, _) => "int64"

      case FloatMultiType(Width4, _) => "float32"
      case FloatMultiType(Width8, _) => "float64"

      case BitsType(_, _) => "uint64"

      case _: BooleanType => "bool"
      case CalcIntType => "int"
      case CalcFloatType => "float64"

      case _: StrType => "string"
      case _: BytesType => "[]byte"

      case AnyType => "interface{}"
      case KaitaiStructType | CalcKaitaiStructType(_) => kstructName
      case KaitaiStreamType | OwnedKaitaiStreamType => s"*$kstreamName"

      case t: UserType => "*" + types2class(t.classSpec match {
        case Some(cs) => cs.name
        case None => t.name
      })
      case t: EnumType => types2class(t.enumSpec.get.name)

      case at: ArrayType => s"[]${kaitaiType2NativeType(at.elType)}"

      case st: SwitchType => kaitaiType2NativeType(st.combinedType)
    }
  }

  def types2class(names: List[String]): String = names.map(x => type2class(x)).mkString("_")

  def enumToStr(enumTypeAbs: List[String]): String = {
    val enumName = enumTypeAbs.last
    val enumClass: List[String] = enumTypeAbs.dropRight(1)
    enumToStr(enumClass, enumName)
  }

  def enumToStr(typeName: List[String], enumName: String): String =
    types2class(typeName) + "__" + type2class(enumName)

  override def kstreamName: String = "kaitai.Stream"
  override def kstructName: String = "kaitai.Struct"
  override def ksErrorName(err: KSError): String = err match {
    case ConversionError => "strconv.NumError"
    case _ => s"kaitai.${err.name}"
  }
}
