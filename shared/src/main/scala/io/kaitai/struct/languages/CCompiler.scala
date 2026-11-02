package io.kaitai.struct.languages

import io.kaitai.struct._
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.CTranslator
import scala.collection.mutable.{ListBuffer, Set => MutableSet}

class CCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
    extends LanguageCompiler(typeProvider, config)
    with ObjectOrientedLanguage
    with SingleOutputFile
    with AllocateIOLocalVar
    with EveryReadIsExpression
    with UniversalDoc
    with SwitchIfOps {
  import CCompiler._

  // ##############################
  // Context / Accumulator Types
  // ##############################

  private final class MethodEmitContext(
    // Method head (for C89 compatibility).
    var head: StringLanguageOutputWriter,
    // Method body.
    var body: StringLanguageOutputWriter,
    // Raw field ids already declared as `_raw_*` to avoid duplicate declarations.
    var rawDecls: MutableSet[String],
    // Signals that `int64_t i;` must be emitted in method locals (repeat loops).
    var needsIndexVar: Boolean,
    // Signals that `internal` pointer must be declared/initialized for instance cache access.
    var needsInternalVar: Boolean,
    // Selects check/assert macro family and return style (data-returning vs void).
    var checkContext: CheckContext
  )

  private def newMethodEmitContext(indentLevel: Int = 0, checkContext: CheckContext = CheckContext.Data): MethodEmitContext = {
    val head = new StringLanguageOutputWriter(indent)
    val body = new StringLanguageOutputWriter(indent)
    head.indentLevel = indentLevel
    body.indentLevel = indentLevel
    new MethodEmitContext(head, body, MutableSet.empty, false, false, checkContext)
  }

  private final class ClassEmitContext(
    // Canonical generated class name (ksx_<className>).
    var className: String,
    // Header-side main struct body (`struct ksx_<className> { ... }`).
    var struct: StringLanguageOutputWriter,
    // Source-side internal struct body (`struct ksx_<className>_internal { ... }`).
    var internalStruct: StringLanguageOutputWriter,
    // Body of `ksx_fill_<className>_instances(...)`.
    var instancesFill: StringLanguageOutputWriter,
    // Body of `ksx_read_<className>_instances(...)`.
    var instancesRead: StringLanguageOutputWriter,
    // Deferred instance-read blocks emitted after direct reads to prevent recursive self-read issues.
    var pendingReads: ListBuffer[StringLanguageOutputWriter],
    //  If true, read-instances function needs to be generated.
    var needsInstanceReads: Boolean,
    //  If true, read-instances function needs to add local `internal` variable.
    var needsInstancesFillInternal: Boolean,
    // If true, read-instances function needs to add local `i` variable.
    var needsInstancesReadIndex: Boolean,
    // If true, read-instances function needs to add local `stream` variable.
    var needsInstancesReadStream: Boolean,
    // Accumulates `{"field", (void*)ksx_get_...,}` entries for the static getter table in the .c file.
    var gettersTable: StringLanguageOutputWriter
  )

  private final class SourceAccumulators(
    var header: StringLanguageOutputWriter,
    var defs: StringLanguageOutputWriter,
    var structs: StringLanguageOutputWriter,
    var main: StringLanguageOutputWriter,
    var instances: StringLanguageOutputWriter,
    var instancesFill: StringLanguageOutputWriter,
    var instancesGet: StringLanguageOutputWriter
  )

  private final class HeaderAccumulators(
    var header: StringLanguageOutputWriter,
    var defs: StringLanguageOutputWriter,
    var enums: StringLanguageOutputWriter,
    var arrays: StringLanguageOutputWriter,
    var structs: StringLanguageOutputWriter,
    var finish: StringLanguageOutputWriter
  )

  // ##############################
  // Mutable Emit State
  // ##############################

  private final class EmitState(
    var method: MethodEmitContext,
    val classes: ListBuffer[ClassEmitContext],
    var tryFinallyCounter: Int,
    var tryFinallyContexts: List[TryFinallyContext],
    var suppressChecksDepth: Int,
    var savedMethodForSwitch: Option[MethodEmitContext],
    val importSrc: CppImportList,
    val importHdr: CppImportList,
    val srcOut: SourceAccumulators,
    val hdrOut: HeaderAccumulators
  ) {
    def currentClass: ClassEmitContext = classes.last
  }

  private val emitState = new EmitState(
    method = newMethodEmitContext(),
    classes = ListBuffer(),
    tryFinallyCounter = 0,
    tryFinallyContexts = Nil,
    suppressChecksDepth = 0,
    savedMethodForSwitch = None,
    importSrc = new CppImportList,
    importHdr = new CppImportList,
    srcOut = new SourceAccumulators(
      new StringLanguageOutputWriter(indent),
      new StringLanguageOutputWriter(indent),
      new StringLanguageOutputWriter(indent),
      new StringLanguageOutputWriter(indent),
      new StringLanguageOutputWriter(indent),
      new StringLanguageOutputWriter(indent),
      new StringLanguageOutputWriter(indent)
    ),
    hdrOut = new HeaderAccumulators(
      new StringLanguageOutputWriter(indent),
      new StringLanguageOutputWriter(indent),
      new StringLanguageOutputWriter(indent),
      new StringLanguageOutputWriter(indent),
      new StringLanguageOutputWriter(indent),
      new StringLanguageOutputWriter(indent)
    )
  )
  lazy val translator = new CTranslator(typeProvider, emitState.importSrc, true)

  private sealed trait CheckContext
  private object CheckContext {
    case object Data extends CheckContext // functions returning ksx_*
    case object Void extends CheckContext // void functions
  }

  private def byCheckContext[A](ifData: A, ifVoid: A): A =
    emitState.method.checkContext match {
      case CheckContext.Data => ifData
      case CheckContext.Void => ifVoid
    }

  private def ksCheckCall(expr: String): String =
    byCheckContext(s"KS_CHECK_DATA($expr)", s"KS_CHECK_VOID($expr)")

  private def ksAssertCall(cond: String, msg: String, err: String): String =
    byCheckContext(s"KS_ASSERT_DATA($cond, $msg, $err)", s"KS_ASSERT_VOID($cond, $msg, $err)")

  private def returnStatement: String =
    byCheckContext("return data;", "return;")

  case class TryFinallyContext(failedVar: String, finallyLabel: String, returnStmt: String, var hasDefer: Boolean = false)

  // ##############################
  // Private Helpers
  // ##############################

  private def printdbg(s: String): Unit = emitState.method.body.puts("//" + s)

  private def appendAtCurrentIndent(dst: StringLanguageOutputWriter, src: StringLanguageOutputWriter): Unit = {
    val rebased = new StringLanguageOutputWriter(indent)
    rebased.indentLevel = 1
    src.result.split("\n", -1).dropRight(1).foreach { line =>
      if (line.isEmpty) {
        rebased.puts
      } else {
        rebased.puts(line)
      }
    }
    dst.add(rebased)
  }

  private def classNeedsInstanceReads(classSpec: ClassSpec): Boolean =
    classSpec.instances.nonEmpty ||
    classSpec.seq.exists(attr => dataTypeNeedsInstanceReads(attr.dataTypeComposite, classSpec))

  private def dataTypeNeedsInstanceReads(dataType: DataType, ownerClass: ClassSpec): Boolean = dataType match {
    case t: UserType =>
      !t.isOpaque && !t.isExternal(ownerClass) && t.classSpec.exists(classNeedsInstanceReads)
    case KaitaiStructType | AnyType => true
    case st: SwitchType => dataTypeNeedsInstanceReads(st.combinedType, ownerClass)
    case at: ArrayType  => dataTypeNeedsInstanceReads(at.elType, ownerClass)
    case _              => false
  }

  private def checked(expr: String): String =
    if (emitState.suppressChecksDepth > 0) {
      expr
    } else {
      emitState.tryFinallyContexts.headOption match {
        case Some(ctx) =>
          ctx.hasDefer = true
          s"KS_CHECK_DEFER($expr, ${ctx.failedVar}, ${ctx.finallyLabel})"
        case None      => ksCheckCall(expr)
      }
    }

  // ##############################
  // Namings
  // ##############################

  override def idToStr(id: Identifier): String = CCompiler.idToStr(id)

  override def publicMemberName(id: Identifier): String = idToStr(id)

  override def privateMemberName(id: Identifier): String = idToStr(id)

  override def localTemporaryName(id: Identifier): String = s"_t_${idToStr(id)}"

  override def paramName(id: Identifier): String = s"p_${idToStr(id)}"

  override def ksErrorName(err: KSError): String = CCompiler.ksErrorName(err)

  // ##############################
  // Helper
  // ##############################


  // ##############################
  // Overrides: Compiler Surface
  // ##############################

  override def results(topClass: ClassSpec): Map[String, String] = {
    val className = topClass.nameAsStr
    Map(
      outFileNameSource(className) -> (emitState.srcOut.header.result + emitState.importSrc.result + emitState.srcOut.defs.result + emitState.srcOut.structs.result + emitState.srcOut.main.result + emitState.srcOut.instances.result + emitState.srcOut.instancesGet.result + emitState.srcOut.instancesFill.result),
      outFileNameHeader(className) -> (emitState.hdrOut.header.result + emitState.importHdr.result + emitState.hdrOut.defs.result + emitState.hdrOut.enums.result + emitState.hdrOut.structs.result + emitState.hdrOut.arrays.result + emitState.hdrOut.finish.result)
    )
  }

  override def outFileName(topClassName: String): String = topClassName.toLowerCase()
  private def outFileNameSource(className: String): String = outFileName(className) + ".c"
  private def outFileNameHeader(className: String): String = outFileName(className) + ".h"

  override def indent: String = "    "

  // ##############################
  // External class handling
  // ##############################

  override def externalTypeDeclaration(extType: ExternalType): Unit = {
    val name = extType.name.head.toLowerCase()
    emitState.importHdr.addLocal(outFileNameHeader(name))
  }

  private def importFile(file: String): Unit = {
    val name = file.toLowerCase().split("/").last
    emitState.importHdr.addLocal(outFileNameHeader(name))
    emitState.hdrOut.defs.puts("/* Import */")
    emitState.hdrOut.defs.puts(s"#ifndef HAVE_DECL_$name")
    emitState.hdrOut.defs.puts(s"#define HAVE_DECL_$name")
    emitState.hdrOut.defs.puts(s"typedef struct ksx_${name} ksx_$name;")
    emitState.hdrOut.defs.puts(s"#endif")
    emitState.hdrOut.defs.puts
  }

  // ##############################
  // Class and constructor handling
  // ##############################

  override def fileHeader(topClassName: String): Unit = {
    emitState.srcOut.header.puts(s"/* $headerComment */")
    emitState.srcOut.header.puts

    emitState.hdrOut.header.puts(s"/* $headerComment */")
    emitState.hdrOut.header.puts
    emitState.hdrOut.header.puts(s"#ifndef KAITAI_${topClassName.toUpperCase()}_H")
    emitState.hdrOut.header.puts(s"#define KAITAI_${topClassName.toUpperCase()}_H")
    emitState.hdrOut.header.puts

    emitState.importSrc.addLocal(outFileNameHeader(topClassName))

    emitState.importHdr.addKaitai("kaitaistruct_internal.h")

    emitState.hdrOut.defs.puts
    emitState.hdrOut.defs.puts("/* Forward declarations */")

    emitState.hdrOut.arrays.puts
    emitState.hdrOut.arrays.puts("/* Array structures */")

    emitState.hdrOut.enums.puts
    emitState.hdrOut.enums.puts("/* Enums */")

    emitState.hdrOut.structs.puts
    emitState.hdrOut.structs.puts("/* Main structures */")

    emitState.srcOut.defs.puts
    emitState.hdrOut.defs.puts
    emitState.srcOut.instances.puts

    emitState.hdrOut.finish.puts
    emitState.hdrOut.finish.puts("#endif")
  }

  override def fileFooter(topClassName: String): Unit = {
    emitState.method.body.dec
    emitState.method.body.puts("}")
  }

  override def classHeader(name: List[String]): Unit = {
    typeProvider.nowClass.meta.imports.foreach(file => importFile(file))
    val className = makeName(name)
    val needsInstReads = classNeedsInstanceReads(typeProvider.nowClass)
    val gettersTable = new StringLanguageOutputWriter(indent)
    gettersTable.indentLevel = 1
    val currentClass = new ClassEmitContext(
      className,
      new StringLanguageOutputWriter(indent),
      new StringLanguageOutputWriter(indent),
      new StringLanguageOutputWriter(indent),
      new StringLanguageOutputWriter(indent),
      ListBuffer.empty,
      needsInstReads,
      false,
      false,
      false,
      gettersTable
    )
    emitState.classes.append(currentClass)
    if (emitState.classes.length == 1) {
      emitState.hdrOut.defs.puts(s"/* Main struct */")
      emitState.hdrOut.defs.puts(s"#ifndef HAVE_DECL_$className")
      emitState.hdrOut.defs.puts(s"#define HAVE_DECL_$className")
      emitState.hdrOut.defs.puts(s"typedef struct ksx_${className} ksx_${className};")
      emitState.hdrOut.defs.puts(s"#endif")
    } else {
      emitState.hdrOut.defs.puts(s"typedef struct ksx_${className} ksx_${className};")
    }

    emitState.hdrOut.arrays.puts
    emitState.hdrOut.arrays.puts(s"struct ksx_array_${className}")
    emitState.hdrOut.arrays.puts("{")
    emitState.hdrOut.arrays.inc
    emitState.hdrOut.arrays.puts("ks_usertype_generic kaitai_base;")
    emitState.hdrOut.arrays.puts("int64_t size;")
    emitState.hdrOut.arrays.puts(s"ksx_$className** data;")
    emitState.hdrOut.arrays.dec
    emitState.hdrOut.arrays.puts(s"};")
    emitState.hdrOut.defs.puts(s"typedef struct ksx_array_${className} ksx_array_${className};")

    emitState.currentClass.struct.puts
    emitState.currentClass.struct.puts(s"struct ksx_${className}")
    emitState.currentClass.struct.puts("{")
    emitState.currentClass.struct.inc
    emitState.currentClass.struct.puts("ks_usertype_generic kaitai_base;")
    if (needsInstReads) {
      emitState.hdrOut.defs.puts(s"typedef struct ksx_${className}_internal ksx_${className}_internal;")
      emitState.currentClass.internalStruct.puts(s"struct ksx_${className}_internal")
      emitState.currentClass.internalStruct.puts("{")
      emitState.currentClass.internalStruct.inc
      emitState.currentClass.internalStruct.puts("ks_bool _flag_instances_read;")
      emitState.srcOut.defs.puts(s"static void ksx_fill_${className}_instances(ksx_${className}* data);")
      emitState.srcOut.defs.puts(s"static void ksx_read_${className}_instances(ksx_${className}* data);")
    }
  }

  override def classFooter(name: List[String]): Unit = {
    emitState.currentClass.struct.dec
    emitState.currentClass.struct.puts(s"};")
    emitState.hdrOut.structs.add(emitState.currentClass.struct)
    if (emitState.currentClass.needsInstanceReads) {
      emitState.currentClass.internalStruct.dec
      emitState.currentClass.internalStruct.puts(s"};")
      emitState.currentClass.internalStruct.puts
      emitState.srcOut.structs.add(emitState.currentClass.internalStruct)
      if (emitState.currentClass.needsInstancesFillInternal) {
        val cn = emitState.currentClass.className
        emitState.srcOut.instancesFill.puts(s"static ks_getter_entry ksx_${cn}_getters[] =")
        emitState.srcOut.instancesFill.puts("{")
        emitState.srcOut.instancesFill.inc
        emitState.srcOut.instancesFill.add(emitState.currentClass.gettersTable)
        emitState.srcOut.instancesFill.puts("{0, 0}")
        emitState.srcOut.instancesFill.dec
        emitState.srcOut.instancesFill.puts("};")
        emitState.srcOut.instancesFill.puts
      }
      emitState.srcOut.instancesFill.puts(s"static void ksx_fill_${emitState.currentClass.className}_instances(ksx_${emitState.currentClass.className}* data)")
      emitState.srcOut.instancesFill.puts("{")
      emitState.srcOut.instancesFill.inc
      if (emitState.currentClass.needsInstancesFillInternal)
        emitState.srcOut.instancesFill.puts(s"ksx_${emitState.currentClass.className}_internal* internal = (ksx_${emitState.currentClass.className}_internal*)ks_usertype_get_internal_read(&data->kaitai_base);")
      emitState.srcOut.instancesFill.puts(s"ks_usertype_set_instances_fn(&data->kaitai_base, (ks_callback)ksx_read_${emitState.currentClass.className}_instances);")
      if (emitState.currentClass.needsInstancesFillInternal)
        emitState.srcOut.instancesFill.puts(s"ks_usertype_set_getters(&data->kaitai_base, ksx_${emitState.currentClass.className}_getters);")
      appendAtCurrentIndent(emitState.srcOut.instancesFill, emitState.currentClass.instancesFill)
      emitState.srcOut.instancesFill.dec
      emitState.srcOut.instancesFill.puts("}")
      emitState.srcOut.instancesFill.puts

      emitState.srcOut.instancesFill.puts(s"static void ksx_read_${emitState.currentClass.className}_instances(ksx_${emitState.currentClass.className}* data)")
      emitState.srcOut.instancesFill.puts("{")
      emitState.srcOut.instancesFill.inc
      if (emitState.currentClass.needsInstancesReadIndex)
        emitState.srcOut.instancesFill.puts("int64_t i;")
      if (emitState.currentClass.needsInstancesReadStream)
        emitState.srcOut.instancesFill.puts(s"ks_stream* stream = ks_usertype_get_stream(&data->kaitai_base);")
      emitState.srcOut.instancesFill.puts(s"ksx_${emitState.currentClass.className}_internal* internal = (ksx_${emitState.currentClass.className}_internal*)ks_usertype_get_internal_read(&data->kaitai_base);")
      emitState.srcOut.instancesFill.puts("if (internal->_flag_instances_read)")
      emitState.srcOut.instancesFill.puts("    return;")
      emitState.srcOut.instancesFill.puts("internal->_flag_instances_read = 1;")
      appendAtCurrentIndent(emitState.srcOut.instancesFill, emitState.currentClass.instancesRead)
      emitState.currentClass.pendingReads.foreach(block => appendAtCurrentIndent(emitState.srcOut.instancesFill, block))
      emitState.srcOut.instancesFill.dec
      emitState.srcOut.instancesFill.puts("}")
      emitState.srcOut.instancesFill.puts
    }
    emitState.classes.trimEnd(1)
  }

  override def classConstructorHeader(name: List[String], parentType: DataType, rootClassName: List[String], isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    val className = makeName(name)
    val classFromStreamName = makeFromStreamName(name)
    translator.setCurrentClass(className)
    val rootName = makeName(rootClassName)
    val paramsArg = Utils.join(params.map(p => s"${kaitaiType2NativeType(p.dataType)}${CCompiler.getPtrSuffix(p.dataType)} ${paramName(p.id)}"), ", ", ", ", "")
    val addParams = Utils.join(params.map(p => paramName(p.id)), ", ", ", ", "")
    val publicEndianArg = if (isHybrid) ", ks_bool is_le" else ""
    val publicEndianPass = if (isHybrid) ", is_le" else ""
    emitState.hdrOut.structs.puts
    emitState.hdrOut.structs.puts(s"ksx_${className}* ksx_read_${classFromStreamName}_from_stream(ks_stream* stream, ks_error* error$publicEndianArg$paramsArg);")
    emitState.srcOut.main.puts
    emitState.srcOut.main.puts(s"ksx_${className}* ksx_read_${classFromStreamName}_from_stream(ks_stream* stream, ks_error* error$publicEndianArg$paramsArg)")
    emitState.srcOut.main.puts("{")
    emitState.srcOut.main.inc
    emitState.srcOut.main.puts(s"ksx_${className}* data;")
    emitState.srcOut.main.puts(s"data = ksx_read_$className(0, stream$publicEndianPass$addParams);")
    emitState.srcOut.main.puts(s"if (error) *error = ks_stream_get_error(stream);")
    emitState.srcOut.main.puts(s"KS_CHECK_DATA();")
    if (classNeedsInstanceReads(typeProvider.nowClass)) {
      emitState.srcOut.main.puts(s"ksx_read_${className}_instances(data);")
      emitState.srcOut.main.puts(s"if (error) *error = ks_stream_get_error(stream);")
      emitState.srcOut.main.puts(s"KS_CHECK_DATA();")
    }
    emitState.srcOut.main.puts(s"return data;")
    emitState.srcOut.main.dec
    emitState.srcOut.main.puts("}")
    val endianess = if (isHybrid) ", ks_bool is_le" else ""
    emitState.srcOut.defs.puts(s"static ksx_${className}* ksx_read_${className}(ks_usertype_generic* parent_data, ks_stream* stream$endianess$paramsArg);")
    emitState.srcOut.main.puts
    emitState.srcOut.main.puts(s"static ksx_${className}* ksx_read_${className}(ks_usertype_generic* parent_data, ks_stream* stream$endianess$paramsArg)")
    emitState.srcOut.main.puts("{")
    emitState.srcOut.main.inc

    emitState.method = newMethodEmitContext(indentLevel = 1, checkContext = CheckContext.Data)
    val internalSize = if (emitState.currentClass.needsInstanceReads) s"sizeof(ksx_${className}_internal)" else "0"
    emitState.srcOut.main.puts(s"ksx_${className}* data = ks_alloc_obj(stream, sizeof(ksx_${className}), KS_TYPE_USERTYPE, sizeof(ksx_${className}), $internalSize, parent_data);")

    params.foreach(p =>
      emitState.method.body.puts(s"data->${idToStr(p.id)} = ${paramName(p.id)};")
    )
    if (emitState.currentClass.needsInstanceReads)
      emitState.method.body.puts(s"ksx_fill_${className}_instances(data);")

    typeProvider.nowClass.meta.endian match {
      case Some(_: CalcEndian) | Some(InheritedEndian) =>
        val name = privateMemberName(EndianIdentifier)
        emitState.currentClass.struct.puts(s"ks_bool $name;")
        if (isHybrid) {
          emitState.srcOut.main.puts(s"data->$name = is_le;")
        } else {
          emitState.srcOut.main.puts(s"data->$name = -1;")
        }
      case _ =>
    }
  }

  override def classConstructorFooter: Unit = {}

  // ##############################
  // Sequence reads
  // ##############################

  override def runRead(name: List[String]): Unit = {}

  override def runReadCalc(): Unit = {
    val errorStr = "\"Unspecified endianess!\""
    emitState.method.body.puts
    emitState.method.body.puts(s"KS_ASSERT_DATA(data->${privateMemberName(EndianIdentifier)} == -1, $errorStr, KS_ERROR_ENDIANESS_UNSPECIFIED);")
    emitState.method.body.puts(s"if (data->${privateMemberName(EndianIdentifier)} == 1) {")
    emitState.method.body.inc
    emitState.method.body.puts(s"${checked(s"ksx_read_${emitState.currentClass.className}_le(stream, data)")};")
    emitState.method.body.dec
    emitState.method.body.puts("} else {")
    emitState.method.body.inc
    emitState.method.body.puts(s"${checked(s"ksx_read_${emitState.currentClass.className}_be(stream, data)")};")
    emitState.method.body.dec
    emitState.method.body.puts("}")
  }

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = {
    val suffix = endian match {
      case Some(e) => e.toSuffix
      case None    => return
    }
    val currentClassName = emitState.currentClass.className
    emitState.srcOut.defs.puts(s"static void ksx_read_${currentClassName}_$suffix(ks_stream* stream, ksx_$currentClassName* data);")
    emitState.srcOut.main.puts(s"static void ksx_read_${currentClassName}_$suffix(ks_stream* stream, ksx_$currentClassName* data)");
    emitState.srcOut.main.puts("{")
    emitState.srcOut.main.inc
    emitState.method = newMethodEmitContext(indentLevel = 1, checkContext = CheckContext.Void)
  }

  private def makeFooter(instance: Boolean): Unit = {
    val className = emitState.currentClass.className
    val outSrc = if (instance) emitState.srcOut.instances else emitState.srcOut.main
    if (emitState.method.needsIndexVar) {
      val index = translator.doName(Identifier.INDEX)
      emitState.method.head.puts(s"int64_t $index;")
    }
    if (emitState.method.needsInternalVar) {
      emitState.method.head.puts(s"ksx_${className}_internal* internal;")
    }
    outSrc.add(emitState.method.head)
    if (emitState.method.head.result.nonEmpty) {
      outSrc.puts
    }
    if (!instance && emitState.method.checkContext == CheckContext.Data) {
      emitState.method.body.puts("return data;")
    }
    if (emitState.method.needsInternalVar) {
      outSrc.puts(s"internal = (ksx_${className}_internal*)ks_usertype_get_internal_read(&data->kaitai_base);")
    }
    outSrc.add(emitState.method.body)
    outSrc.dec
    outSrc.puts("}")
    emitState.method = newMethodEmitContext()
  }

  override def readFooter(): Unit = {
    makeFooter(false)
  }

  // ##############################
  // Instance handling
  // ##############################

  private def handleInstanceReadsCommon(outInstancesRead: StringLanguageOutputWriter, attrName: Identifier, isNullable: Boolean, isArray: Boolean, expr: String): Unit = {
    val name = idToStr(attrName)
    emitState.currentClass.needsInstancesReadStream = true
    if (isNullable) {
      outInstancesRead.puts(s"if (data->${nullFlagForName(attrName)})")
      outInstancesRead.puts("{")
      outInstancesRead.inc
    }
    if (isArray) {
      emitState.currentClass.needsInstancesReadIndex = true
      outInstancesRead.puts(s"for (i = 0; i < data->$name->size; i++)")
      outInstancesRead.puts("{")
      outInstancesRead.inc
    }
    outInstancesRead.puts(expr)
    if (isArray) {
      outInstancesRead.dec
      outInstancesRead.puts("}")
    }
    if (isNullable) {
      outInstancesRead.dec
      outInstancesRead.puts("}")
    }
  }

  // Self-typed instances (e.g. cast_to_top.header: cast_to_top) can recurse indefinitely
  // if we auto-read nested instances, so we detect and guard that case explicitly.
  private def isSelfUserType(t: UserType): Boolean =
    t.classSpec.exists(_.name == typeProvider.nowClass.name)

  private def handleInstanceReads(
    outInstancesRead: StringLanguageOutputWriter,
    attrType: DataType,
    attrName: Identifier,
    isNullable: Boolean,
    skipRecursiveSelfRead: Boolean = false,
    isArray: Boolean = false
  ): Unit = {
    val name = idToStr(attrName)
    attrType match {
      case t: UserType =>
        if (t.isOpaque || t.isExternal(typeProvider.nowClass)) return
        if (skipRecursiveSelfRead && isSelfUserType(t)) return
        if (!t.classSpec.exists(classNeedsInstanceReads)) return
        val typename = makeName(t.classSpec.get.name)
        val readExpr = if (isArray) {
          s"if (data->$name->data[i]) { KS_CHECK_VOID(ksx_read_${typename}_instances(data->$name->data[i])); }"
        } else {
          s"if (data->$name) { KS_CHECK_VOID(ksx_read_${typename}_instances(data->$name)); }"
        }
        handleInstanceReadsCommon(outInstancesRead, attrName, isNullable, isArray, readExpr)
      case AnyType if isArray =>
        handleInstanceReadsCommon(outInstancesRead, attrName, isNullable, true, s"KS_CHECK_VOID(ks_usertype_read_instances((ks_usertype_generic*)((void**)data->$name->data)[i]));")
      case KaitaiStructType | AnyType =>
        handleInstanceReadsCommon(outInstancesRead, attrName, isNullable, false, s"KS_CHECK_VOID(ks_usertype_read_instances(data->$name));")
      case st: SwitchType =>
        handleInstanceReads(outInstancesRead, st.combinedType, attrName, isNullable, skipRecursiveSelfRead, isArray)
      case at: ArrayType =>
        handleInstanceReads(outInstancesRead, at.elType, attrName, isNullable, skipRecursiveSelfRead, isArray = true)
      case _ =>
    }
  }

  private def flagForInstName(ksName: Identifier) = s"_flag_${idToStr(ksName)}"

  override def instanceHeader(classNameList: List[String], instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    val className = makeName(classNameList)
    val name = privateMemberName(instName)
    translator.setCurrentClass(className)

    emitState.method = newMethodEmitContext(checkContext = CheckContext.Void)
    emitState.method.head.puts(s"static void ksx_read_${className}_instance_${name}(ks_stream* stream, ksx_${className}* data)")
    emitState.method.head.puts("{")
    emitState.method.head.inc
    emitState.method.body.inc
    emitState.srcOut.instances.puts
    emitState.srcOut.instances.inc
  }

  override def instanceFooter: Unit = makeFooter(true)

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    emitState.method.needsInternalVar = true
    emitState.method.head.puts("int64_t _old_pos = ks_stream_get_pos(stream);")
    emitState.method.body.puts(s"if (internal->${flagForInstName(instName)})")
    emitState.method.body.inc
    emitState.method.body.puts("return;")
    emitState.method.body.dec
  }

  override def instanceDeclaration(attrName: InstanceIdentifier, attrType: DataType, isNullable: Boolean): Unit = {
    emitState.currentClass.internalStruct.puts(s"ks_bool ${flagForInstName(attrName)};")
    attributeDeclarationCommon(attrName, attrType, isNullable, true)
  }

  override def instanceSetCalculated(instName: InstanceIdentifier): Unit = {
    emitState.method.needsInternalVar = true
    emitState.method.body.puts(s"internal->${flagForInstName(instName)} = 1;")
  }

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType, isNullable: Boolean): Unit = {
    emitState.method.body.puts("ks_stream_seek(stream, _old_pos);")
  }

  override def instanceCalculate(instName: Identifier, dataType: DataType, value: expr): Unit = {
    handleAssignmentSimple(instName, expression(value))
  }

  override def instanceClear(instName: InstanceIdentifier): Unit = {}

  // ##############################
  // Attributes
  // ##############################

  private def attributeDeclarationCommon(attrName: Identifier, attrType: DataType, isNullable: Boolean, isInstance: Boolean): Unit = {
    val name = idToStr(attrName)
    if (name == "_parent" || name == "_root") {
      return
    }
    attrName match {
      case RawIdentifier(_) => return
      case _                =>
    }
    if (isNullable) {
      emitState.currentClass.struct.puts(s"ks_bool ${nullFlagForName(attrName)};")
    }
    val currentClassName = emitState.currentClass.className
    val typeStr = kaitaiType2NativeType(attrType) + CCompiler.getPtrSuffix(attrType)
    emitState.currentClass.struct.puts(s"$typeStr ${privateMemberName(attrName)};")

    if (isInstance) {
      emitState.srcOut.instancesGet.puts(s"static ${typeStr} ksx_get_${currentClassName}_${name}(ksx_${currentClassName}* data)")
      emitState.srcOut.instancesGet.puts("{")
      emitState.srcOut.instancesGet.inc
      emitState.srcOut.instancesGet.puts(s"ks_stream* stream = ks_usertype_get_stream(&data->kaitai_base);")
      emitState.srcOut.instancesGet.puts(s"KS_CHECK(ksx_read_${currentClassName}_instance_${name}(stream, data), data->$name);")
      attrType match {
        case t: UserType => emitState.srcOut.instancesGet.puts(s"return (${kaitaiType2NativeType(t)}*)data->${name};")
        case _           => emitState.srcOut.instancesGet.puts(s"return data->${name};")
      }
      emitState.srcOut.instancesGet.dec
      emitState.srcOut.instancesGet.puts("}")
      emitState.srcOut.instancesGet.puts

      val getFunc = s"_get_${idToStr(attrName)}"
      emitState.currentClass.internalStruct.puts(s"${typeStr} (*$getFunc)(ksx_${currentClassName}* data);")
      emitState.currentClass.needsInstancesFillInternal = true
      emitState.currentClass.instancesFill.puts(s"internal->$getFunc = ksx_get_${currentClassName}_${name};")
      emitState.currentClass.gettersTable.puts(s"""{"$name", (void*)ksx_get_${currentClassName}_${name}},""")

      val pendingBlock = new StringLanguageOutputWriter(indent)
      pendingBlock.puts(s"internal->$getFunc(data);")
      handleInstanceReads(pendingBlock, attrType, attrName, isNullable, true)
      emitState.currentClass.pendingReads.append(pendingBlock)
    } else {
      handleInstanceReads(emitState.currentClass.instancesRead, attrType, attrName, isNullable)
    }
  }

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    attributeDeclarationCommon(attrName, attrType, isNullable, false)
  }

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {}

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {
    emitState.method.body.puts(s"if (data->${privateMemberName(EndianIdentifier)} == 1) {")
    emitState.method.body.inc
    leProc()
    emitState.method.body.dec
    emitState.method.body.puts("} else {")
    emitState.method.body.inc
    beProc()
    emitState.method.body.dec
    emitState.method.body.puts("}")
  }

  private def getRawIdExpr(varName: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(varName)
    rep match {
      case NoRepeat => memberName
      case _        => s"$memberName->data[i]"
    }
  }

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, rep: RepeatSpec): String = {
    val srcExpr = "_raw_" + privateMemberName(varSrc)

    proc match {
      case ProcessXor(xorValue) =>
        val t = translator.detectType(xorValue)
        t match {
          case t1: BytesType =>
            s"ks_bytes_process_xor_bytes($srcExpr, ${expression(xorValue)})"
          case it: IntType =>
            val xorBytes = it match {
              case Int1Type(_)           => 1
              case IntMultiType(_, w, _) => w.width
              case BitsType(width, _)    => (width + 7) / 8
              case CalcIntType           => 1
            }
            s"ks_bytes_process_xor_int($srcExpr, ${expression(xorValue)}, $xorBytes)"
          case _ =>
            emitState.method.body.puts("Unknown xor type: " + t.toString())
            ""
        }
      case ProcessZlib =>
        s"ks_inflate(ks_usertype_get_config(&data->kaitai_base), $srcExpr)"
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        s"ks_bytes_process_rotate_left($srcExpr, $expr)"
      case ProcessCustom(typename, args) =>
        val name2 = idToStr(varSrc)
        val procClass = typename.last
        emitState.importHdr.addLocal(outFileNameHeader(procClass))
        emitState.method.head.puts(s"ks_custom_decoder _decoder_$name2;")
        emitState.method.body.puts(s"_decoder_$name2 = ${procClass}_create(${args.map(expression).mkString(", ")});")
        emitState.method.body.puts(s"$srcExpr = _decoder_$name2.decode(_decoder_$name2.userdata, $srcExpr);")
        emitState.method.body.puts(s"${procClass}_destroy(_decoder_$name2);")
        srcExpr
    }
  }

  // ##############################
  // Validate
  // ##############################

  private def emitValidationError(cond: String): Unit = {
    emitState.method.body.puts(s"if (!($cond))")
    emitState.method.body.puts("{")
    emitState.method.body.inc
    emitState.method.body.puts(ksAssertCall("1", "\"Validation error\"", "KS_ERROR_VALIDATION_FAILED") + ";")
    emitState.method.body.dec
    emitState.method.body.puts("}")
  }

  override def attrValidateExpr(
    attr: AttrLikeSpec,
    checkExpr: Ast.expr,
    err: KSError,
    useIo: Boolean,
    actual: Ast.expr,
    expected: Option[Ast.expr] = None
  ): Unit =
    emitValidationError(translator.translate(checkExpr))

  override def attrValidateInEnum(
    attr: AttrLikeSpec,
    et: EnumType,
    valueExpr: Ast.expr,
    err: ValidationNotInEnumError,
    useIo: Boolean
  ): Unit = {
    val value = translator.translate(valueExpr)
    val isInEnumCond = et.enumSpec.get.map.keys.toSeq
      .map(v => s"$value == ${translator.doIntLiteral(v)}")
      .mkString(" || ")
    emitValidationError(isInEnumCond)
  }

  // ##############################
  // Enum
  // ##############################

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, EnumValueSpec)]): Unit = {
    val enumClass = makeName(curClass :+ enumName)
    val enumClass2 = "ksx_" + enumClass

    emitState.hdrOut.enums.puts
    emitState.hdrOut.enums.puts(s"typedef enum ${enumClass2}_")
    emitState.hdrOut.enums.puts(s"{")
    emitState.hdrOut.enums.inc

    enumColl.foreach { case (id, label) =>
      val value = translator.doIntLiteral(id)
      emitState.hdrOut.enums.puts(s"${enumClass2.toUpperCase()}_${label.name.toUpperCase()} = $value,")
    }

    emitState.hdrOut.enums.dec
    emitState.hdrOut.enums.puts(s"} $enumClass2;")

    emitState.hdrOut.arrays.puts
    emitState.hdrOut.arrays.puts(s"struct ksx_array_${enumClass}")
    emitState.hdrOut.arrays.puts("{")
    emitState.hdrOut.arrays.inc
    emitState.hdrOut.arrays.puts("ks_usertype_generic kaitai_base;")
    emitState.hdrOut.arrays.puts("int64_t size;")
    emitState.hdrOut.arrays.puts(s"ksx_$enumClass* data;")
    emitState.hdrOut.arrays.dec
    emitState.hdrOut.arrays.puts(s"};")
    emitState.hdrOut.defs.puts(s"typedef struct ksx_array_${enumClass} ksx_array_$enumClass;")
  }

  // ##############################
  // IO
  // ##############################

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = {
    val privateVarName = privateMemberName(varName)

    val ioName = s"_io_$privateVarName"

    emitState.method.head.puts(s"ks_stream* $ioName;")
    emitState.method.body.puts(s"/* Subtype with substream */")
    emitState.method.body.puts(s"$ioName = ks_stream_create_from_bytes(_raw_$privateVarName);")
    ioName
  }

  override def useIO(ioEx: expr): String = {
    emitState.method.head.puts(s"$kstreamName* io;")
    emitState.method.body.puts(s"io = ${expression(ioEx)};")
    "io"
  }

  override def pushPos(io: String): Unit = {
    // emitState.method.body.puts(s"long _pos = $io.Pos;")
  }

  override def seek(io: String, pos: Ast.expr): Unit =
    emitState.method.body.puts(s"${checked(s"ks_stream_seek(${makeIO(io)}, ${expression(pos)})")};")

  override def popPos(io: String): Unit = {
    // emitState.method.body.puts(s"$io.Seek(_pos);")
  }

  // ##############################
  // Conditional read
  // ##############################

  override def condIfHeader(expr: expr): Unit = {
    emitState.method.body.puts(s"if (${expression(expr)}) {")
    emitState.method.body.inc
  }

  private def nullFlagForName(ksName: Identifier) =
    s"_is_valid_${idToStr(ksName)}"

  override def condIfSetNull(instName: Identifier): Unit =
    emitState.method.body.puts(s"data->${nullFlagForName(instName)} = 0;")

  override def condIfSetNonNull(instName: Identifier): Unit =
    emitState.method.body.puts(s"data->${nullFlagForName(instName)} = 1;")

  override def condIfFooter: Unit = {
    emitState.method.body.dec
    emitState.method.body.puts("}")
  }

  // ##############################
  // Assignment helper
  // ##############################

  override def condRepeatInitAttr(id: Identifier, dataType: DataType): Unit = {}

  private def isTypeGenericInternal(idType: DataType): Boolean = {
    idType match {
      case at: ArrayType => isTypeGenericInternal(at.elType)
      case t: SwitchType =>
        t.combinedType match {
          case KaitaiStructType | AnyType => true
          case _                          => false
        }
      case _ => false
    }
  }

  private def isTypeGeneric(id: Identifier): Boolean = {
    if (idToStr(id).startsWith("_")) {
      return false
    }
    val idType = typeProvider.determineType(id)
    isTypeGenericInternal(idType)
  }

  private def elementType(id: Identifier): DataType = typeProvider.determineType(id) match {
    case at: ArrayType => at.elType
    case t             => t
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    if (expr == "") {
      id match {
        case RawIdentifier(_) =>
        case _                =>
          val target = getRawIdExpr(id, NoRepeat)
          emitState.method.body.puts(s"data->$target = _raw_$target;")
      }
      return
    }
    handleAssignmentCommon(id, expr, false)
  }

  override def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit =
    emitState.method.body.puts(s"${kaitaiType2NativeType(dataType)}${CCompiler.getPtrSuffix(dataType)} $id = $expr;")

  // ##############################
  // Repeat Helpers
  // ##############################

  private def beginResizableRepeat(comment: String, name: String, dataType: DataType): Unit = {
    val dataTypeArray = ArrayTypeInStream(dataType)
    val arrayTypeSize = getKaitaiTypeEnumAndSize(dataType)
    emitState.method.needsIndexVar = true
    emitState.method.body.puts(comment)
    emitState.method.body.puts(s"data->$name = ks_alloc_obj(stream, sizeof(${kaitaiType2NativeType(dataTypeArray)}), $arrayTypeSize, 0, 0);")
    emitState.method.body.puts(s"data->$name->size = 0;")
    emitState.method.body.puts(s"data->$name->data = 0;")
    emitState.method.body.puts("{")
    emitState.method.body.inc
  }

  private def emitResizableRepeatAssignment(id: Identifier, expr: String): String = {
    val name = privateMemberName(id)
    val elType = elementType(id)
    val ptr = CCompiler.getPtrSuffix(elType)
    val sizeof = s"sizeof(${kaitaiType2NativeType(elType)}$ptr)"
    emitState.method.body.puts(s"data->$name->size++;")
    emitState.method.body.puts(s"data->$name->data = ks_realloc(ks_stream_get_config(stream), data->$name->data, $sizeof * data->$name->size);")
    emitState.method.body.puts(s"memset(data->$name->data + data->$name->size - 1, 0, $sizeof);")
    handleAssignmentCommon(id, expr, true)
    name
  }

  private def handleRawRepeatAssignment(id: Identifier, expr: String): Unit =
    handleAssignmentCommon(id, expr, false)

  // ##############################
  // Repeat Eos
  // ##############################

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType): Unit = {
    val name = privateMemberName(id)
    val pos = translator.doName(Identifier.INDEX)
    val io_new = makeIO(io)
    beginResizableRepeat("/* Array (repeat-eos) */", name, dataType)
    emitState.method.body.puts(s"while (!ks_stream_is_eof($io_new)) {")
    emitState.method.body.puts(s"$pos = data->$name->size;")
    emitState.method.body.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    id match {
      case RawIdentifier(_) => handleRawRepeatAssignment(id, expr)
      case _                => emitResizableRepeatAssignment(id, expr)
    }
  }

  override def condRepeatEosFooter: Unit = {
    emitState.method.body.dec
    emitState.method.body.puts("}")
    emitState.method.body.dec
    emitState.method.body.puts("}")
  }

  // ##############################
  // Repeat Expr
  // ##############################

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, repeatExpr: expr): Unit = {
    val pos = translator.doName(Identifier.INDEX)
    val len = expression(repeatExpr)
    val name = privateMemberName(id)
    val lenVar = s"l_$name"
    val dataTypeArray = ArrayTypeInStream(dataType)
    val arrayTypeSize = getKaitaiTypeEnumAndSize(dataType)
    val ptr = CCompiler.getPtrSuffix(dataType)
    emitState.method.needsIndexVar = true
    emitState.method.head.puts(s"int64_t $lenVar;")
    emitState.method.body.puts("/* Array (repeat-expr) */")
    emitState.method.body.puts(s"$lenVar = $len;")
    emitState.method.body.puts(s"data->$name = ks_alloc_obj(stream, sizeof(${kaitaiType2NativeType(dataTypeArray)}), $arrayTypeSize, 0, 0);")
    emitState.method.body.puts(s"data->$name->size = 0;")
    emitState.method.body.puts(s"data->$name->data = ks_alloc_data(ks_stream_get_config(stream), sizeof(${kaitaiType2NativeType(dataType)}$ptr) * $lenVar);")
    emitState.method.body.puts(s"for ($pos = 0; $pos < $lenVar; $pos++)")
    emitState.method.body.puts("{")
    emitState.method.body.inc
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit = {
    val name = privateMemberName(id)
    handleAssignmentCommon(id, expr, true)
    id match {
      case RawIdentifier(_) =>
      case _                =>
        emitState.method.body.puts(s"data->$name->size++;")
    }
  }

  override def condRepeatExprFooter: Unit = {
    fileFooter(null)
  }

  // ##############################
  // Repeat Until
  // ##############################

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, untilExpr: expr): Unit = {
    val pos = translator.doName(Identifier.INDEX)
    val name = privateMemberName(id)
    val ptr = CCompiler.getPtrSuffix(dataType)
    beginResizableRepeat("/* Array (repeat-until) */", name, dataType)
    emitState.method.body.puts(s"${kaitaiType2NativeType(dataType)}$ptr ${translator.doName("_")} = {0};")
    emitState.method.body.puts("i = 0;")
    emitState.method.body.puts(s"(void)${translator.doName("_")};")
    emitState.method.body.puts(s"while (1)")
    emitState.method.body.puts("{")
    emitState.method.body.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    if (isRaw) {
      handleRawRepeatAssignment(id, expr)
    } else {
      val name = emitResizableRepeatAssignment(id, expr)
      val nameTemp = translator.doName(Identifier.ITERATOR)
      val pos = translator.doName(Identifier.INDEX)
      emitState.method.body.puts(s"$nameTemp = data->$name->data[$pos];")
    }
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, untilExpr: expr): Unit = {
    val pos = translator.doName(Identifier.INDEX)
    typeProvider._currentIteratorType = Some(dataType)
    emitState.method.body.puts(s"$pos++;")
    emitState.method.body.puts(s"if (${expression(untilExpr)})")
    emitState.method.body.puts("    break;")
    emitState.method.body.dec
    emitState.method.body.puts("}")
    emitState.method.body.dec
    emitState.method.body.puts("}")
  }

  // ##############################
  // Expression handling
  // ##############################

  override def blockScopeHeader: Unit = {
    emitState.method.body.puts("{")
    emitState.method.body.inc
  }
  override def blockScopeFooter: Unit = {
    emitState.method.body.dec
    emitState.method.body.puts("}")
  }

  private def handleAssignmentCommon(id: Identifier, expr: String, isArray: Boolean): Unit = {
    val name = privateMemberName(id)
    val nameTarget = if (isArray) {
      val pos = translator.doName(Identifier.INDEX)
      s"$name->data[$pos]"
    } else {
      name
    }

    id match {
      case RawIdentifier(_) =>
        if (!emitState.method.rawDecls.contains(name)) {
          emitState.method.head.puts(s"ks_bytes* _raw_$name;")
          emitState.method.rawDecls.add(name)
        }
        emitState.method.body.puts(s"${checked(s"_raw_$name = $expr")};")
      case _ if isTypeGeneric(id) =>
        emitState.method.body.puts(s"${checked(s"data->$nameTarget = (ks_usertype_generic*)$expr")};")
      case _ =>
        emitState.method.body.puts(s"${checked(s"data->$nameTarget = $expr")};")
    }
  }

  override def parseExpr(dataType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    val io_new = makeIO(io)

    dataType match {
      case t: ReadableType =>
        s"ks_stream_read_${t.apiCall(defEndian)}($io_new)"
      case blt: BytesLimitType =>
        s"ks_stream_read_bytes($io_new, ${expression(blt.size)})"
      case _: BytesEosType =>
        s"ks_stream_read_bytes_full($io_new)"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        val include2 = if (include) 1 else 0
        val consume2 = if (consume) 1 else 0
        val eosError2 = if (eosError) 1 else 0
        if (terminator.length == 1) {
          val t = terminator.head & 0xff
          s"ks_stream_read_bytes_term($io_new, $t, $include2, $consume2, $eosError2)"
        } else {
          s"ks_stream_read_bytes_term_multi($io_new, ${translator.doByteArrayLiteral(terminator)}, $include2, $consume2, $eosError2)"
        }
      case BitsType1(bitEndian) =>
        s"ks_stream_read_bits_${bitEndian.toSuffix.toLowerCase()}($io_new, 1)"
      case BitsType(width: Int, bitEndian) =>
        s"ks_stream_read_bits_${bitEndian.toSuffix.toLowerCase()}($io_new, $width)"
      case t: UserType =>
        val parent = t.forcedParent match {
          case Some(USER_TYPE_NO_PARENT) => "0"
          case Some(fp)                  => translator.translate(fp)
          case None                      => "data"
        }
        val typeName = makeName(t.classSpec.get.name)
        val fromStreamTypeName = makeFromStreamName(t.classSpec.get.name)
        val isExternal = t.isExternal(typeProvider.nowClass)
        val formatArg = (arg: Ast.expr) => {
          val argExpr = translator.translate(arg)
          translator.detectType(arg) match {
            case _: UserType => s"(void*)$argExpr" // Possibly need cast to generic struct
            case _           => argExpr
          }
        }
        val addParams = Utils.join(t.args.map(formatArg), ", ", ", ", "")
        val addEndian = t.classSpec.get.meta.endian match {
          case Some(InheritedEndian) => s", data->${privateMemberName(EndianIdentifier)}"
          case _                     => ""
        }
        if (isExternal || (t.isOpaque && t.classSpec.get != typeProvider.topClass)) { // Our own top class is *not* opaque!
          s"ksx_read_${fromStreamTypeName}_from_stream($io_new, 0$addParams)"
        } else {
          s"ksx_read_$typeName((ks_usertype_generic*)${parent}, $io_new$addEndian$addParams)"
        }
      case _ =>
        emitState.method.body.puts("Missing expression type: " + dataType.toString())
        ""
    }
  }

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Seq[Byte]], include: Boolean) = {
    val expr1 = (padRight, terminator) match {
      // Keep the first terminator byte when include=true and pad-right uses the same byte.
      case (Some(padByte), Some(term)) if include && term.length == 1 && (term.head & 0xff) == padByte => expr0
      case (Some(padByte), _)                                                                          => s"ks_bytes_strip_right($expr0, $padByte)"
      case _                                                                                           => expr0
    }
    terminator match {
      case Some(term) =>
        val include_int = if (include) 1 else 0
        if (term.length == 1) {
          val t = term.head & 0xff
          s"ks_bytes_terminate($expr1, $t, $include_int)"
        } else {
          s"ks_bytes_terminate_multi($expr1, ${translator.doByteArrayLiteral(term)}, $include_int)"
        }
      case None => expr1
    }
  }

  // ##############################
  // Switch (normal)
  // ##############################

  override def switchRequiresIfs(onType: DataType): Boolean = onType match {
    case _: IntType | _: EnumType => false
    case _                        => true
  }

  private def switchOverrideStart(): Unit = {
    if (emitState.savedMethodForSwitch.isDefined)
      throw new RuntimeException("Nested switch context override is not supported")
    emitState.savedMethodForSwitch = Some(emitState.method)
    emitState.method = newMethodEmitContext(emitState.method.body.indentLevel, emitState.method.checkContext)
  }

  private def switchOverrideEnd(): Unit = {
    val savedContext = emitState.savedMethodForSwitch.getOrElse(
      throw new RuntimeException("switchOverrideEnd called without matching switchOverrideStart")
    )
    savedContext.body.add(emitState.method.head)
    if (emitState.method.head.result.nonEmpty)
      savedContext.body.puts
    savedContext.body.add(emitState.method.body)
    savedContext.needsIndexVar ||= emitState.method.needsIndexVar
    savedContext.needsInternalVar ||= emitState.method.needsInternalVar
    emitState.method = savedContext
    emitState.savedMethodForSwitch = None
  }

  private def openSwitchBranch(header: String): Unit = {
    emitState.method.body.puts(header)
    emitState.method.body.puts("{")
    emitState.method.body.inc
    switchOverrideStart()
  }

  private def closeSwitchBranch(emitBreak: Boolean): Unit = {
    switchOverrideEnd()
    if (emitBreak) {
      emitState.method.body.puts("break;")
    }
    emitState.method.body.dec
    emitState.method.body.puts("}")
  }

  override def switchStart(id: Identifier, on: Ast.expr): Unit =
    emitState.method.body.puts(s"switch (${expression(on)}) {")

  override def switchCaseFirstStart(condition: Ast.expr): Unit =
    switchCaseStart(condition)

  override def switchCaseStart(condition: Ast.expr): Unit =
    openSwitchBranch(s"case ${expression(condition)}:")

  override def switchCaseEnd(): Unit =
    closeSwitchBranch(emitBreak = true)

  override def switchElseStart(): Unit =
    openSwitchBranch("default:")

  override def switchEnd(): Unit =
    emitState.method.body.puts("}")

  // ##############################
  // Switch (if)
  // ##############################

  private val NAME_SWITCH_ON = Ast.expr.Name(Ast.identifier(Identifier.SWITCH_ON))

  override def switchIfStart(id: Identifier, on: Ast.expr, onType: DataType): Unit = {
    emitState.method.body.puts("{")
    emitState.method.body.inc
    emitState.method.body.puts(s"${kaitaiType2NativeType(onType)} ${expression(NAME_SWITCH_ON)} = ${expression(on)};")
  }

  private def switchCmpExpr(condition: Ast.expr): String =
    expression(
      Ast.expr.Compare(
        NAME_SWITCH_ON,
        Ast.cmpop.Eq,
        condition
      )
    )

  override def switchIfCaseFirstStart(condition: Ast.expr): Unit =
    openSwitchBranch(s"if (${switchCmpExpr(condition)})")

  override def switchIfCaseStart(condition: Ast.expr): Unit =
    openSwitchBranch(s"else if (${switchCmpExpr(condition)})")

  override def switchIfCaseEnd(): Unit =
    closeSwitchBranch(emitBreak = false)

  override def switchIfElseStart(): Unit =
    openSwitchBranch("else")

  override def switchIfEnd(): Unit = {
    emitState.method.body.dec
    emitState.method.body.puts("}")
  }

  // ##############################
  // Other
  // ##############################

  override def userTypeDebugRead(id: String, dataType: DataType, assignType: DataType): Unit = {
    dataType match {
      case t: UserType =>
        val target = if (id.startsWith("_t_")) id else s"data->$id"
        t.classSpec match {
          case Some(spec) if classNeedsInstanceReads(spec) =>
            val typeName = makeName(spec.name)
            emitState.method.body.puts(s"${checked(s"ksx_read_${typeName}_instances((ksx_${typeName}*)$target)")};")
          case _ if emitState.tryFinallyContexts.nonEmpty =>
            // In try/finally debug paths we still need checked(...) so KS_CHECK_DEFER
            // can detect pending stream error and set the failed flag.
            emitState.method.body.puts(s"${checked(s"(void)$target")};")
          case _ =>
        }
      case _ =>
    }
  }

  override def tryFinally(tryBlock: () => Unit, finallyBlock: () => Unit): Unit = {
    val returnStmt = returnStatement
    val failedVar = s"_ks_tf_failed_${emitState.tryFinallyCounter}"
    val finallyLabel = s"_ks_tf_finally_${emitState.tryFinallyCounter}"
    emitState.tryFinallyCounter += 1
    val ctx = TryFinallyContext(failedVar, finallyLabel, returnStmt)

    emitState.method.body.puts("{")
    emitState.method.body.inc
    emitState.method.body.puts(s"ks_bool $failedVar = 0;")
    emitState.tryFinallyContexts = ctx :: emitState.tryFinallyContexts
    tryBlock()
    emitState.tryFinallyContexts = emitState.tryFinallyContexts.tail
    if (ctx.hasDefer)
      emitState.method.body.puts(s"$finallyLabel: ;")
    emitState.suppressChecksDepth += 1
    finallyBlock()
    emitState.suppressChecksDepth -= 1
    emitState.method.body.puts(s"if ($failedVar)")
    emitState.method.body.puts("{")
    emitState.method.body.inc
    emitState.method.body.puts("ks_stream_set_error(stream, 0);")
    emitState.method.body.puts(returnStmt)
    emitState.method.body.dec
    emitState.method.body.puts("}")
    emitState.method.body.dec
    emitState.method.body.puts("}")
  }

  override def universalDoc(doc: DocSpec): Unit = {}

  override def type2class(className: String): String =
    className

}

object CCompiler extends LanguageCompilerStatic
    with StreamStructNames
    with UpperCamelCaseClasses
    with ExceptionNames {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new CCompiler(tp, config)

  def idToStr(id: Identifier): String = {
    id match {
      case SpecialIdentifier(name)  => name
      case NamedIdentifier(name)    => name.toLowerCase()
      case NumberedIdentifier(idx)  => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => name.toLowerCase()
      case RawIdentifier(innerId)   => idToStr(innerId)
    }
  }

  def kaitaiType2NativeTypeArray(attrType: DataType): String = {
    attrType match {
      case t: UserType                                => s"ksx_array_${makeName(t.classSpec.get.name)}"
      case t: EnumType                                => s"ksx_array_${makeName(t.enumSpec.get.name)}"
      case _: StrType                                 => s"ks_array_string"
      case _: BytesType                               => s"ks_array_bytes"
      case KaitaiStreamType | OwnedKaitaiStreamType   => "ks_array_stream"
      case KaitaiStructType | CalcKaitaiStructType(_) => s"ks_array_usertype_generic"
      case AnyType                                    => "ks_array_any"
      case st: SwitchType                             => kaitaiType2NativeTypeArray(st.combinedType)
      case _                                          => s"ks_array_${kaitaiType2NativeType(attrType)}"
    }
  }

  def kaitaiType2NativeType(attrType: DataType): String = {
    attrType match {
      case Int1Type(false)                => "uint8_t"
      case IntMultiType(false, Width2, _) => "uint16_t"
      case IntMultiType(false, Width4, _) => "uint32_t"
      case IntMultiType(false, Width8, _) => "uint64_t"

      case Int1Type(true)                => "int8_t"
      case IntMultiType(true, Width2, _) => "int16_t"
      case IntMultiType(true, Width4, _) => "int32_t"
      case IntMultiType(true, Width8, _) => "int64_t"

      case FloatMultiType(Width4, _) => "float"
      case FloatMultiType(Width8, _) => "double"

      case BitsType(_, _) => "uint64_t"

      case CalcIntType    => "int64_t"
      case CalcFloatType  => "double"
      case _: BooleanType => "ks_bool"

      case _: StrType   => "ks_string*"
      case _: BytesType => "ks_bytes*"

      case AnyType                                    => "void"
      case KaitaiStructType | CalcKaitaiStructType(_) => kstructName
      case KaitaiStreamType | OwnedKaitaiStreamType   => kstreamName

      case t: UserType => "ksx_" + makeName(t.classSpec.get.name)
      case t: EnumType => "ksx_" + makeName(t.enumSpec.get.name)

      case at: ArrayType => kaitaiType2NativeTypeArray(at.elType)

      case st: SwitchType => kaitaiType2NativeType(st.combinedType)
      case _              => "Error Type"
    }
  }

  def getPtrSuffix(dataType: DataType): String = {
    dataType match {
      case _: UserType | _: ArrayType                 => "*"
      case KaitaiStreamType | OwnedKaitaiStreamType   => "*"
      case KaitaiStructType | CalcKaitaiStructType(_) => "*"
      case AnyType                                    => "*"
      case sw: SwitchType                             => CCompiler.getPtrSuffix(sw.combinedType)
      case _                                          => ""
    }
  }

  def getKaitaiTypeEnumAndSize(attrType: DataType): String = {
    attrType match {
      case Int1Type(false)                => "KS_TYPE_ARRAY_UINT, 1"
      case IntMultiType(false, Width2, _) => "KS_TYPE_ARRAY_UINT, 2"
      case IntMultiType(false, Width4, _) => "KS_TYPE_ARRAY_UINT, 4"
      case IntMultiType(false, Width8, _) => "KS_TYPE_ARRAY_UINT, 8"

      case Int1Type(true)                => "KS_TYPE_ARRAY_INT, 1"
      case IntMultiType(true, Width2, _) => "KS_TYPE_ARRAY_INT, 2"
      case IntMultiType(true, Width4, _) => "KS_TYPE_ARRAY_INT, 4"
      case IntMultiType(true, Width8, _) => "KS_TYPE_ARRAY_INT, 8"

      case FloatMultiType(Width4, _) => "KS_TYPE_ARRAY_FLOAT, 4"
      case FloatMultiType(Width8, _) => "KS_TYPE_ARRAY_FLOAT, 8"

      case _: StrType => "KS_TYPE_ARRAY_STRING, sizeof(ks_string*)"

      case BitsType(_, _) => "KS_TYPE_ARRAY_UINT, 8"
      case _: BytesType   => s"KS_TYPE_ARRAY_BYTES, sizeof(ks_bytes*)"

      case t: UserType => s"KS_TYPE_ARRAY_USERTYPE, sizeof(ksx_${makeName(t.classSpec.get.name)}*)"
      case t: EnumType => getKaitaiTypeEnumAndSize(t.basedOn)

      case _ => "KS_TYPE_UNKNOWN, 0"
    }
  }

  def types2class(typeName: Ast.typeId): String =
    types2class(typeName.names)

  def types2class(names: Iterable[String]) = names.map(type2class).mkString(".").toLowerCase()

  def makeName(names: Iterable[String]) = {
    val arr = names.toList
    if (arr.length == 1)
      arr.mkString("_").toLowerCase()
    else
      arr.drop(1).mkString("_").toLowerCase()
  }

  def makeFromStreamName(names: Iterable[String]) = names.mkString("_").toLowerCase()

  def makeIO(io: String) = if (io == "_io") "stream" else io

  override def kstructName = "ks_usertype_generic"
  override def kstreamName = "ks_stream"
  override def ksErrorName(err: KSError): String = err match {
    case EndOfStreamError         => "KS_ERROR_END_OF_STREAM"
    case UndecidedEndiannessError => "KS_ERROR_ENDIANESS_UNSPECIFIED"
    case _: ValidationError       => "KS_ERROR_VALIDATION_FAILED"
    case _                        => "KS_ERROR_OTHER"
  }
}
