package io.kaitai.struct

import java.io.FileReader

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format._
import io.kaitai.struct.languages._
import io.kaitai.struct.languages.components.{LanguageCompiler, LanguageCompilerStatic}

import scala.collection.mutable.ListBuffer

class ClassCompiler(val topClass: ClassSpec, val lang: LanguageCompiler) extends AbstractCompiler {
  val provider = new ClassTypeProvider(topClass)

  val topClassName = List(topClass.meta.get.id)

  topClass.name = topClassName

  val userTypes: Map[String, ClassSpec] = gatherUserTypes(topClass) ++ Map(topClassName.last -> topClass)

  var nowClass: ClassSpec = topClass

  def gatherUserTypes(curClass: ClassSpec): Map[String, ClassSpec] = {
    val recValues: Map[String, ClassSpec] = curClass.types.map {
      case (typeName, intClass) => gatherUserTypes(intClass)
    }.flatten.toMap
    curClass.types ++ recValues
  }

  override def compile {
    lang.open(topClassName.head, provider)

    lang.fileHeader(topClassName.head)
    compileClass(topClass)
    lang.fileFooter(topClassName.head)
    lang.close
  }

  def compileClass(curClass: ClassSpec): Unit = {
    nowClass = curClass
    provider.nowClass = curClass

    lang.classHeader(nowClass.name)

    val extraAttrs = ListBuffer[AttrSpec]()
    extraAttrs += AttrSpec(RootIdentifier, UserTypeInstream(topClassName))
    extraAttrs += AttrSpec(ParentIdentifier, UserTypeInstream(curClass.parentTypeName))

    // Forward declarations for recursive types
    curClass.types.foreach { case (typeName, intClass) => lang.classForwardDeclaration(List(typeName)) }

    lang.classConstructorHeader(nowClass.name, curClass.parentTypeName, topClassName)
    curClass.instances.foreach { case (instName, instSpec) => lang.instanceClear(instName) }
    curClass.seq.foreach((attr) => lang.attrParse(attr, attr.id, extraAttrs, lang.normalIO))
    lang.classConstructorFooter

    lang.classDestructorHeader(nowClass.name, curClass.parentTypeName, topClassName)
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
      curClass.types.foreach { case (typeName, intClass) => compileClass(intClass) }

      nowClass = curClass
      provider.nowClass = curClass
    }

    curClass.instances.foreach { case (instName, instSpec) => compileInstance(nowClass.name, instName, instSpec, extraAttrs) }

    // Attributes declarations and readers
    (curClass.seq ++ extraAttrs).foreach((attr) => lang.attributeDeclaration(attr.id, attr.dataTypeComposite, attr.cond))
    (curClass.seq ++ extraAttrs).foreach((attr) => lang.attributeReader(attr.id, attr.dataTypeComposite))

    curClass.enums.foreach { case(enumName, enumColl) => compileEnum(enumName, enumColl) }

    lang.classFooter(nowClass.name)

    if (!lang.innerClasses) {
      curClass.types.foreach { case (typeName, intClass) => compileClass(intClass) }
    }
  }

  def compileInstance(className: List[String], instName: InstanceIdentifier, instSpec: InstanceSpec, extraAttrs: ListBuffer[AttrSpec]): Unit = {
    // Determine datatype
    val dataType = TypeProcessor.getInstanceDataType(instSpec)

    // Declare caching variable
    lang.instanceDeclaration(instName, dataType, ConditionalSpec(None, NoRepeat))

    lang.instanceHeader(className, instName, dataType)
    lang.instanceCheckCacheAndReturn(instName)

    instSpec match {
      case ValueInstanceSpec(value, _) =>
        lang.instanceCalculate(instName, dataType, value)
      case i: ParseInstanceSpec =>
        val io = i.io match {
          case None => lang.normalIO
          case Some(ex) => lang.useIO(ex)
        }
        i.pos.foreach { pos =>
          lang.pushPos(io)
          lang.seek(io, pos)
        }
        lang.attrParse(i, instName, extraAttrs, io)
        i.pos.foreach((pos) => lang.popPos(io))
    }

    lang.instanceSetCalculated(instName)
    lang.instanceReturn(instName)
    lang.instanceFooter
  }

  def compileEnum(enumName: String, enumColl: Map[Long, String]): Unit = {
    lang.enumDeclaration(nowClass.name, enumName, enumColl)
  }
}

object ClassCompiler {
  def localFileToSpec(yamlFilename: String): ClassSpec = {
    val reader = new FileReader(yamlFilename)
    val mapper = new ObjectMapper(new YAMLFactory())
    val spec = mapper.readValue(reader, classOf[ClassSpec])
    TypeProcessor.processTypes(spec)
    spec
  }

  def fromLocalFileToFile(yamlFilename: String, lang: LanguageCompilerStatic, outDir: String, config: RuntimeConfig): AbstractCompiler =
    fromClassSpecToFile(localFileToSpec(yamlFilename), lang, outDir, config)

  def fromClassSpecToFile(topClass: ClassSpec, lang: LanguageCompilerStatic, outDir: String, config: RuntimeConfig): AbstractCompiler = {
    val outPath = lang.outFilePath(config, outDir, topClass.meta.get.id)
    if (config.verbose)
      Console.println(s"... => ${outPath}")
    lang match {
      case GraphvizClassCompiler =>
        val out = new FileLanguageOutputWriter(outPath, lang.indent)
        new GraphvizClassCompiler(topClass, out)
      case CppCompiler =>
        val outSrc = new FileLanguageOutputWriter(s"$outPath.cpp", lang.indent)
        val outHdr = new FileLanguageOutputWriter(s"$outPath.h", lang.indent)
        new ClassCompiler(topClass, new CppCompiler(config.verbose, outSrc, outHdr))
      case _ =>
        val out = new FileLanguageOutputWriter(outPath, lang.indent)
        new ClassCompiler(topClass, getCompiler(lang, config, out))
    }
  }

  def fromStringToString(src: String, lang: LanguageCompilerStatic, config: RuntimeConfig):
    (StringLanguageOutputWriter, Option[StringLanguageOutputWriter], ClassCompiler) = {
    val mapper = new ObjectMapper(new YAMLFactory())
    val topClass: ClassSpec = mapper.readValue(src, classOf[ClassSpec])

    fromClassSpecToString(topClass, lang, config)
  }

  def fromClassSpecToString(topClass: ClassSpec, lang: LanguageCompilerStatic, config: RuntimeConfig):
    (StringLanguageOutputWriter, Option[StringLanguageOutputWriter], ClassCompiler) = {
    lang match {
      case CppCompiler =>
        val outSrc = new StringLanguageOutputWriter(lang.indent)
        val outHdr = new StringLanguageOutputWriter(lang.indent)
        val cc = new ClassCompiler(topClass, new CppCompiler(config.verbose, outSrc, outHdr))
        (outSrc, Some(outHdr), cc)
      case _ =>
        val out = new StringLanguageOutputWriter(lang.indent)
        val cc = new ClassCompiler(topClass, getCompiler(lang, config, out))
        (out, None, cc)
    }
  }

  private def getCompiler(lang: LanguageCompilerStatic, config: RuntimeConfig, out: LanguageOutputWriter) = lang match {
    case CSharpCompiler => new CSharpCompiler(config.verbose, out, config.dotNetNamespace)
    case JavaCompiler => new JavaCompiler(config.verbose, out, config.javaPackage)
    case JavaScriptCompiler => new JavaScriptCompiler(config.verbose, out)
    case PerlCompiler => new PerlCompiler(config.verbose, out)
    case PHPCompiler => new PHPCompiler(config.verbose, out, config.phpNamespace)
    case PythonCompiler => new PythonCompiler(config.verbose, out)
    case RubyCompiler => new RubyCompiler(config.verbose, config.debug, out)
  }
}
