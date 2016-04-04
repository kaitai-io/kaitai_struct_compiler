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

  def open(topClassName: String, tp: TypeProvider): Unit = {
    _translator = Some(getTranslator(tp))
  }
  def close = out.close
  def getTranslator(tp: TypeProvider): BaseTranslator
  def translator: BaseTranslator = _translator.get

  def headerComment = "This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild"
  def fileHeader(topClassName: String): Unit
  def fileFooter(topClassName: String): Unit = {}

  def classHeader(name: List[String]): Unit
  def classFooter(name: List[String]): Unit

  def classConstructorHeader(name: List[String], parentClassName: List[String], rootClassName: List[String]): Unit
  def classConstructorFooter: Unit

  def attributeDeclaration(attrName: String, attrType: BaseType): Unit
  def attributeReader(attrName: String, attrType: BaseType): Unit

  def attrParse(attr: AttrLikeSpec, id: String, extraAttrs: ListBuffer[AttrSpec], io: String): Unit

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
  def instanceDeclaration(attrName: String, attrType: BaseType) = attributeDeclaration(attrName, attrType)
  def instanceHeader(className: List[String], instName: String, dataType: BaseType): Unit
  def instanceAttrName(instName: String): String
  def instanceFooter: Unit
  def instanceCheckCacheAndReturn(instName: String): Unit
  def instanceReturn(instName: String): Unit
  def instanceCalculate(instName: String, value: Ast.expr)

  def enumDeclaration(curClass: List[String], enumName: String, enumColl: Map[Long, String]): Unit

  def expression(e: Ast.expr): String = translator.translate(e)
}

trait LanguageCompilerStatic {
  def indent: String
  def outFileName(topClassName: String): String
  def outFilePath(config: RuntimeConfig, outDir: String, topClassName: String) = s"$outDir/${outFileName(topClassName)}"
}

object LanguageCompilerStatic {
  def byString(langName: String): LanguageCompilerStatic = langName match {
    case "cpp_stl" => CppCompiler
    case "java" => JavaCompiler
    case "javascript" => JavaScriptCompiler
    case "python" => PythonCompiler
    case "ruby" => RubyCompiler
  }
}
