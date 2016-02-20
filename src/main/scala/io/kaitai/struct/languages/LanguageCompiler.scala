package io.kaitai.struct.languages

import io.kaitai.struct.format.AttrSpec

trait LanguageCompiler {
  def fileHeader(sourceFileName: String, topClassName: String): Unit

  def classHeader(name: String): Unit
  def classFooter: Unit

  def classConstructorHeader(name: String): Unit
  def classConstructorFooter: Unit

  def attributeDeclaration(attrName: String, attrType: String, isArray: Boolean): Unit
  def attributeReader(attrName: String, attrType: String, isArray: Boolean): Unit

  def attrFixedContentsParse(attrName: String, contents: Array[Byte]): Unit
  def attrNoTypeWithSize(varName: String, size: String): Unit
  def attrNoTypeWithSizeEos(varName: String): Unit
  def attrStdTypeParse(attr: AttrSpec, endian: Option[String]): Unit
  def attrUserTypeParse(attr: AttrSpec, io: String): Unit

  def normalIO: String
  def allocateIO(varName: String): String

  def instanceHeader(instName: String, dataType: String, isArray: Boolean): Unit
  def instanceAttrName(instName: String): String
  def instanceFooter: Unit
  def instanceCheckCacheAndReturn(instName: String): Unit
  def instanceReturn(instName: String): Unit
}
