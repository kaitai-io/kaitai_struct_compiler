package io.kaitai.struct.languages.components

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.Identifier

/**
  * Allows uniform implementation of attrFixedContentsParse by enforcing usage
  * of doByteArrayLiteral in relevant language's translator.
  */
trait FixedContentsUsingArrayByteLiteral extends LanguageCompiler {
  def attrFixedContentsParse(attrName: Identifier, contents: Array[Byte]) =
    attrFixedContentsParse(
      attrName,
      translator.translate(
        Ast.expr.List(
          contents.map(x => Ast.expr.IntNum(BigInt(x & 0xff)))
        )
      )
    )
  def attrFixedContentsParse(attrName: Identifier, contents: String): Unit
}
