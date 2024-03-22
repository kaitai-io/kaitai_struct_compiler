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
    * @param extPrec precedence of external context of this expression:
    *                it it's higher than inner expression we translate, we
    *                will generate extra parenthesis to keep inner expression
    *                safe
    * @return expression in target language as string
    */
  def translate(v: Ast.expr, extPrec: Int): String

  def translate(v: Ast.expr): String = translate(v, 0)
}
