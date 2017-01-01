package io.kaitai.struct

import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format._
import io.kaitai.struct.languages._
import io.kaitai.struct.languages.components.{LanguageCompiler, LanguageCompilerStatic}

import scala.collection.mutable.ListBuffer

class ClassCompiler(val topClass: ClassSpec, val lang: LanguageCompiler) extends AbstractCompiler {
  val provider = new ClassTypeProvider(topClass)

  val topClassName = topClass.name

  override def compile {
    lang.open(topClassName.head, provider)

    lang.fileHeader(topClassName.head)
    compileOpaqueClasses(topClass)
    compileClass(topClass)
    lang.fileFooter(topClassName.head)
    lang.close
  }

  def compileOpaqueClasses(topClass: ClassSpec) = {
    TypeProcessor.getOpaqueClasses(topClass).foreach((classSpec) =>
      lang.opaqueClassDeclaration(classSpec)
    )
  }

  def compileClass(curClass: ClassSpec): Unit = {
    provider.nowClass = curClass

    curClass.doc.foreach((doc) => lang.classDoc(curClass.name, doc))
    lang.classHeader(curClass.name)

    val extraAttrs = ListBuffer[AttrSpec]()
    extraAttrs += AttrSpec(RootIdentifier, UserTypeInstream(topClassName))
    extraAttrs += AttrSpec(ParentIdentifier, UserTypeInstream(curClass.parentTypeName))

    // Forward declarations for recursive types
    curClass.types.foreach { case (typeName, _) => lang.classForwardDeclaration(List(typeName)) }

    if (lang.innerEnums)
      compileEnums(curClass)

    if (lang.debug)
      lang.debugClassSequence(curClass.seq)

    lang.classConstructorHeader(curClass.name, curClass.parentTypeName, topClassName)
    curClass.instances.foreach { case (instName, _) => lang.instanceClear(instName) }
    curClass.seq.foreach((attr) => lang.attrParse(attr, attr.id, extraAttrs))
    lang.classConstructorFooter

    lang.classDestructorHeader(curClass.name, curClass.parentTypeName, topClassName)
    curClass.seq.foreach((attr) => lang.attrDestructor(attr, attr.id))
    curClass.instances.foreach { case (id, instSpec) =>
      instSpec match {
        case pis: ParseInstanceSpec => lang.attrDestructor(pis, id)
        case _: ValueInstanceSpec => // ignore for now
      }
    }
    lang.classDestructorFooter

    // Recursive types
    if (lang.innerClasses) {
      compileSubclasses(curClass)

      provider.nowClass = curClass
    }

    curClass.instances.foreach { case (instName, instSpec) => compileInstance(curClass.name, instName, instSpec, extraAttrs) }

    // Attributes declarations and readers
    (curClass.seq ++ extraAttrs).foreach((attr) => lang.attributeDeclaration(attr.id, attr.dataTypeComposite, attr.cond))
    (curClass.seq ++ extraAttrs).foreach { (attr) =>
      attr.doc.foreach((doc) => lang.attributeDoc(attr.id, doc))
      lang.attributeReader(attr.id, attr.dataTypeComposite, attr.cond)
    }

    lang.classFooter(curClass.name)

    if (!lang.innerClasses)
      compileSubclasses(curClass)

    if (!lang.innerEnums)
      compileEnums(curClass)
  }

  def compileEnums(curClass: ClassSpec): Unit =
    curClass.enums.foreach { case(_, enumColl) => compileEnum(curClass, enumColl) }

  def compileSubclasses(curClass: ClassSpec): Unit =
    curClass.types.foreach { case (_, intClass) => compileClass(intClass) }

  def compileInstance(className: List[String], instName: InstanceIdentifier, instSpec: InstanceSpec, extraAttrs: ListBuffer[AttrSpec]): Unit = {
    // Determine datatype
    val dataType = TypeProcessor.getInstanceDataType(instSpec)

    // Declare caching variable
    val condSpec = instSpec match {
      case vis: ValueInstanceSpec => ConditionalSpec(vis.ifExpr, NoRepeat)
      case pis: ParseInstanceSpec => pis.cond
    }
    lang.instanceDeclaration(instName, dataType, condSpec)

    instSpec.doc.foreach((doc) => lang.attributeDoc(instName, doc))
    lang.instanceHeader(className, instName, dataType)
    lang.instanceCheckCacheAndReturn(instName)

    instSpec match {
      case vi: ValueInstanceSpec =>
        lang.attrParseIfHeader(instName, vi.ifExpr)
        lang.instanceCalculate(instName, dataType, vi.value)
        lang.attrParseIfFooter(vi.ifExpr)
      case i: ParseInstanceSpec =>
        lang.attrParse(i, instName, extraAttrs)
    }

    lang.instanceSetCalculated(instName)
    lang.instanceReturn(instName)
    lang.instanceFooter
  }

  def compileEnum(curClass: ClassSpec, enumColl: EnumSpec): Unit = {
    lang.enumDeclaration(curClass.name, enumColl.name.last, enumColl.sortedSeq)
  }
}

object ClassCompiler {
  def fromClassSpecToFile(topClass: ClassSpec, lang: LanguageCompilerStatic, outDir: String, conf: RuntimeConfig): AbstractCompiler = {
    val config = updateConfig(conf, topClass)
    val outPath = lang.outFilePath(config, outDir, topClass.name.head)
    if (config.verbose)
      Console.println(s"... => ${outPath}")
    lang match {
      case GraphvizClassCompiler =>
        val out = new FileLanguageOutputWriter(outPath, lang.indent)
        new GraphvizClassCompiler(topClass, out)
      case CppCompiler =>
        val outSrc = new FileLanguageOutputWriter(s"$outPath.cpp", lang.indent)
        val outHdr = new FileLanguageOutputWriter(s"$outPath.h", lang.indent)
        new ClassCompiler(topClass, new CppCompiler(config, outSrc, outHdr))
      case _ =>
        val out = new FileLanguageOutputWriter(outPath, lang.indent)
        new ClassCompiler(topClass, getCompiler(lang, config, out))
    }
  }

  def fromClassSpecToString(topClass: ClassSpec, lang: LanguageCompilerStatic, conf: RuntimeConfig):
    (StringLanguageOutputWriter, Option[StringLanguageOutputWriter], AbstractCompiler) = {
    val config = updateConfig(conf, topClass)
    lang match {
      case GraphvizClassCompiler =>
        val out = new StringLanguageOutputWriter(lang.indent)
        (out, None, new GraphvizClassCompiler(topClass, out))
      case CppCompiler =>
        val outSrc = new StringLanguageOutputWriter(lang.indent)
        val outHdr = new StringLanguageOutputWriter(lang.indent)
        val cc = new ClassCompiler(topClass, new CppCompiler(config, outSrc, outHdr))
        (outSrc, Some(outHdr), cc)
      case _ =>
        val out = new StringLanguageOutputWriter(lang.indent)
        val cc = new ClassCompiler(topClass, getCompiler(lang, config, out))
        (out, None, cc)
    }
  }

  private def getCompiler(lang: LanguageCompilerStatic, config: RuntimeConfig, out: LanguageOutputWriter) = lang match {
    case CSharpCompiler => new CSharpCompiler(config, out)
    case JavaCompiler => new JavaCompiler(config, out)
    case JavaScriptCompiler => new JavaScriptCompiler(config, out)
    case PerlCompiler => new PerlCompiler(config, out)
    case PHPCompiler => new PHPCompiler(config, out)
    case PythonCompiler => new PythonCompiler(config, out)
    case RubyCompiler => new RubyCompiler(config, out)
  }

  /**
    * Updates runtime configuration with "enforcement" options that came from a source file itself.
    * Currently only used to enforce debug when "ks-debug: true" is specified in top-level "meta" key.
    * @param config original runtime configuration
    * @param topClass top-level class spec
    * @return updated runtime configuration with applied enforcements
    */
  private def updateConfig(config: RuntimeConfig, topClass: ClassSpec): RuntimeConfig = {
    if (topClass.meta.get.forceDebug) {
      config.copy(debug = true)
    } else {
      config
    }
  }
}
