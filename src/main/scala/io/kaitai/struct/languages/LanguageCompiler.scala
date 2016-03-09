package io.kaitai.struct.languages

import io.kaitai.struct.LanguageOutputWriter
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType.{UserType, BaseType}
import io.kaitai.struct.format._
import io.kaitai.struct.translators.{JavaTranslator, BaseTranslator, TypeProvider}

import scala.collection.mutable.ListBuffer

abstract class LanguageCompiler(verbose: Boolean, outDir: String) {
  protected var _translator: Option[BaseTranslator] = None
  protected var out: LanguageOutputWriter = null

  def open(topClassName: String, tp: TypeProvider): Unit = {
    val fn = s"$outDir/${outFileName(topClassName)}"
    if (verbose)
      Console.println(s"... => ${fn}")
    out = new LanguageOutputWriter(fn, indent)
    _translator = Some(getTranslator(tp))
  }
  def close = out.close
  def getTranslator(tp: TypeProvider): BaseTranslator
  def translator: BaseTranslator = _translator.get

  def outFileName(topClassName: String): String
  def indent: String

  def fileHeader(sourceFileName: String, topClassName: String): Unit
  def fileFooter(topClassName: String): Unit = {}

  def classHeader(name: String): Unit
  def classFooter(name: String): Unit

  def classConstructorHeader(name: String, parentClassName: String, rootClassName: String): Unit
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
  def seek(io: String, pos: Ast.expr): Unit

  def instanceDeclaration(attrName: String, attrType: BaseType) = attributeDeclaration(attrName, attrType)
  def instanceHeader(className: String, instName: String, dataType: BaseType): Unit
  def instanceAttrName(instName: String): String
  def instanceFooter: Unit
  def instanceCheckCacheAndReturn(instName: String): Unit
  def instanceReturn(instName: String): Unit
  def instanceCalculate(instName: String, value: Ast.expr)

  def enumDeclaration(curClass: String, enumName: String, enumColl: Map[Long, String]): Unit

  def expression(e: Ast.expr): String = translator.translate(e)
}
