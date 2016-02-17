package io.kaitai.structures.languages

import io.kaitai.structures.format.AttrSpec

trait LanguageCompiler {
  def fileHeader(sourceFileName: String, topClassName: String): Unit

  def classHeader(name: String): Unit
  def classFooter: Unit

  def classConstructorHeader(name: String): Unit
  def classConstructorFooter: Unit

  def attributeDeclaration(attrName: String, attrType: String): Unit
  def attributeReader(attrName: String, attrType: String): Unit

  def attrFixedContentsParse(attrName: String, contents: Array[Byte]): Unit
  def attrNoTypeWithSize(varName: String, size: String): Unit
  def attrNoTypeWithSizeEos(varName: String): Unit
  def attrStdTypeParse(attr: AttrSpec, endian: Option[String]): Unit
  def attrUserTypeParse(attr: AttrSpec, io: String): Unit

  def normalIO: String
  def allocateIO(varName: String): String

  def instanceHeader(instName: String, dataType: String): Unit
  def instanceFooter: Unit
}
