package io.kaitai.struct.languages.components

import io.kaitai.struct.format.{DocSpec, Identifier}

/**
  * Docstrings for attributes and classes are the same for this language.
  */
trait UniversalDoc extends LanguageCompiler {
  override def classDoc(name: List[String], doc: DocSpec) = universalDoc(doc)
  override def attributeDoc(id: Identifier, doc: DocSpec) = universalDoc(doc)

  def universalDoc(doc: DocSpec): Unit
}
