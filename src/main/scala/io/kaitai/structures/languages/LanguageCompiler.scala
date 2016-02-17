package io.kaitai.structures.languages

import io.kaitai.structures.format.AttrSpec

trait LanguageCompiler {
  def fileHeader(sourceFileName: String, topClassName: String)

  def classHeader(name: String)
  def classFooter

  def classConstructorHeader(name: String)
  def classConstructorFooter

  def attributeDeclaration(attrName: String, attrType: String)
  def attributeReader(attrName: String, attrType: String)

  def attrFixedContentsParse(attrName: String, contents: Array[Byte])
  def attrStdTypeParse(attr: AttrSpec, endian: Option[String])
}
