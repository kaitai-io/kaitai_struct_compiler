package io.kaitai.struct.languages

import io.kaitai.struct.LanguageOutputWriter
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{ProcessExpr, AttrSpec}
import io.kaitai.struct.translators.{JavaTranslator, BaseTranslator, TypeProvider}

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

  def classConstructorHeader(name: String): Unit
  def classConstructorFooter: Unit

  def attributeDeclaration(attrName: String, attrType: String, isArray: Boolean): Unit
  def attributeReader(attrName: String, attrType: String, isArray: Boolean): Unit

  def attrFixedContentsParse(attrName: String, contents: Array[Byte]): Unit
  def attrNoTypeWithSize(id: String, attr: AttrSpec): Unit
  def attrNoTypeWithSizeEos(id: String, attr: AttrSpec): Unit
  def attrStdTypeParse(id: String, attr: AttrSpec, endian: Option[String]): Unit
  def attrUserTypeParse(id: String, attr: AttrSpec, io: String): Unit

  def attrProcess(proc: ProcessExpr, varSrc: String, varDest: String): Unit

  def normalIO: String
  def allocateIO(varName: String): String
  def seek(io: String, pos: Ast.expr): Unit

  def instanceDeclaration(attrName: String, attrType: String, isArray: Boolean) = attributeDeclaration(attrName, attrType, isArray)
  def instanceHeader(className: String, instName: String, dataType: String, isArray: Boolean): Unit
  def instanceAttrName(instName: String): String
  def instanceFooter: Unit
  def instanceCheckCacheAndReturn(instName: String): Unit
  def instanceReturn(instName: String): Unit
  def instanceCalculate(instName: String, value: Ast.expr)

  def expression(e: Ast.expr): String = translator.translate(e)
}
