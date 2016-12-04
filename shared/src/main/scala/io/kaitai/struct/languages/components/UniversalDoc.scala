package io.kaitai.struct.languages.components

import io.kaitai.struct.format.Identifier

/**
  * Docstrings for attributes and classes are the same for this language.
  */
trait UniversalDoc extends LanguageCompiler {
  override def classDoc(name: List[String], doc: String) = universalDoc(doc)
  override def attributeDoc(id: Identifier, doc: String) = universalDoc(doc)

  def universalDoc(doc: String)
}
