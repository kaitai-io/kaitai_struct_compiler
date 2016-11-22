package io.kaitai.struct.languages.components

import io.kaitai.struct.format.Identifier

/**
  * Allows uniform implementation of attrFixedContentsParse by enforcing usage
  * of doByteArrayLiteral in relevant language's translator.
  */
trait FixedContentsUsingArrayByteLiteral extends LanguageCompiler {
  def attrFixedContentsParse(attrName: Identifier, contents: Array[Byte]) =
    attrFixedContentsParse(attrName, translator.doByteArrayLiteral(contents))
  def attrFixedContentsParse(attrName: Identifier, contents: String): Unit
}
