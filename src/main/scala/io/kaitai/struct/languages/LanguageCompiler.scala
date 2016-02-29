package io.kaitai.struct.languages

import io.kaitai.struct.LanguageOutputWriter
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{ProcessExpr, AttrSpec}

abstract class LanguageCompiler(verbose: Boolean, outDir: String) {
  protected var out: LanguageOutputWriter = null

  def open(topClassName: String): Unit = {
    val fn = s"$outDir/${outFileName(topClassName)}"
    if (verbose)
      Console.println(s"... => ${fn}")
    out = new LanguageOutputWriter(fn, indent)
  }
  def close = out.close

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
  def attrNoTypeWithSize(varName: String, size: Ast.expr): Unit
  def attrNoTypeWithSizeEos(varName: String): Unit
  def attrStdTypeParse(attr: AttrSpec, endian: Option[String]): Unit
  def attrUserTypeParse(id: String, attr: AttrSpec, io: String): Unit

  def attrProcess(proc: ProcessExpr, varSrc: String, varDest: String): Unit

  def normalIO: String
  def allocateIO(varName: String): String
  def seek(io: String, pos: Ast.expr): Unit

  def instanceHeader(className: String, instName: String, dataType: String, isArray: Boolean): Unit
  def instanceAttrName(instName: String): String
  def instanceFooter: Unit
  def instanceCheckCacheAndReturn(instName: String): Unit
  def instanceReturn(instName: String): Unit
  def instanceCalculate(instName: String, value: Ast.expr)

  def expression(e: Ast.expr): String
}
