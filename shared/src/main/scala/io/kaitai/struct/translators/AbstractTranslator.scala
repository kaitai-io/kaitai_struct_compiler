package io.kaitai.struct.translators

import io.kaitai.struct.exprlang.Ast

/**
  * Translators are per-target language classes which implement translation
  * of Kaitai Struct expression language into expression in target language.
  *
  * This simplest, most abstract form of translator provides only a single
  * `translate` method, which takes KS expression and returns string in
  * target language.
  */
trait AbstractTranslator {
  /**
    * Translates KS expression into an expression in some target language.
    * @param v KS expression to translate
    * @return expression in target language as string
    */
  def translate(v: Ast.expr): String
}
