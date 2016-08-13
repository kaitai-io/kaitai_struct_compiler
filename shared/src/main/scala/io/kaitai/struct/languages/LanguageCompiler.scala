package io.kaitai.struct.languages

import io.kaitai.struct.{LanguageOutputWriter, RuntimeConfig}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType.{BaseType, UserType}
import io.kaitai.struct.format._
import io.kaitai.struct.translators.{BaseTranslator, JavaTranslator, TypeProvider}

import scala.collection.mutable.ListBuffer

abstract class LanguageCompiler(verbose: Boolean, out: LanguageOutputWriter) {
  protected var _translator: Option[BaseTranslator] = None

  def getStatic: LanguageCompilerStatic
  def open(topClassName: String, tp: TypeProvider): Unit = {
    _translator = Some(getStatic.getTranslator(tp))
  }
  def close = out.close
  def translator: BaseTranslator = _translator.get

  def headerComment = "This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild"
  def fileHeader(topClassName: String): Unit
  def fileFooter(topClassName: String): Unit = {}

  def classHeader(name: List[String]): Unit
  def classFooter(name: List[String]): Unit
  def classForwardDeclaration(name: List[String]): Unit = {}

  def classConstructorHeader(name: List[String], parentClassName: List[String], rootClassName: List[String]): Unit
  def classConstructorFooter: Unit

  def classDestructorHeader(name: List[String], parentTypeName: List[String], topClassName: List[String]): Unit = {}
  def classDestructorFooter: Unit = {}

  def attributeDeclaration(attrName: String, attrType: BaseType, condSpec: ConditionalSpec): Unit
  def attributeReader(attrName: String, attrType: BaseType): Unit

  def attrParse(attr: AttrLikeSpec, id: String, extraAttrs: ListBuffer[AttrSpec], io: String): Unit
  def attrDestructor(attr: AttrLikeSpec, id: String): Unit = {}

  def attrFixedContentsParse(attrName: String, contents: Array[Byte]): Unit

  def condIfHeader(expr: Ast.expr): Unit
  def condIfFooter(expr: Ast.expr): Unit

  def condRepeatEosHeader(id: String, io: String, dataType: BaseType, needRaw: Boolean): Unit
  def condRepeatEosFooter: Unit

  def condRepeatExprHeader(id: String, io: String, dataType: BaseType, needRaw: Boolean, repeatExpr: expr): Unit
  def condRepeatExprFooter: Unit

  def attrProcess(proc: ProcessExpr, varSrc: String, varDest: String): Unit

  def normalIO: String
  def allocateIO(varName: String, rep: RepeatSpec): String
  def useIO(ioEx: Ast.expr): String
  def pushPos(io: String): Unit
  def seek(io: String, pos: Ast.expr): Unit
  def popPos(io: String): Unit

  def instanceClear(instName: String): Unit = {}
  def instanceSetCalculated(instName: String): Unit = {}
  def instanceDeclaration(attrName: String, attrType: BaseType, condSpec: ConditionalSpec) = attributeDeclaration(attrName, attrType, condSpec)
  def instanceHeader(className: List[String], instName: String, dataType: BaseType): Unit
  def instanceAttrName(instName: String): String
  def instanceFooter: Unit
  def instanceCheckCacheAndReturn(instName: String): Unit
  def instanceReturn(instName: String): Unit
  def instanceCalculate(instName: String, dataType: BaseType, value: expr)

  def enumDeclaration(curClass: List[String], enumName: String, enumColl: Map[Long, String]): Unit

  def expression(e: Ast.expr): String = translator.translate(e)

  def privateMemberName(ksName: String): String
}

trait LanguageCompilerStatic {
  def indent: String
  def outFileName(topClassName: String): String
  def outFilePath(config: RuntimeConfig, outDir: String, topClassName: String) = s"$outDir/${outFileName(topClassName)}"
  def getTranslator(tp: TypeProvider): BaseTranslator
}

object LanguageCompilerStatic {
  val NAME_TO_CLASS = Map(
    "cpp_stl" -> CppCompiler,
    "csharp" -> CSharpCompiler,
    "java" -> JavaCompiler,
    "javascript" -> JavaScriptCompiler,
    "php" -> PHPCompiler,
    "python" -> PythonCompiler,
    "ruby" -> RubyCompiler
  )

  def byString(langName: String): LanguageCompilerStatic = NAME_TO_CLASS(langName)
}
