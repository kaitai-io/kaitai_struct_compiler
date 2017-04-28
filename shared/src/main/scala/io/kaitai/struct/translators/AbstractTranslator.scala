package io.kaitai.struct.translators

import io.kaitai.struct.exprlang.Ast

trait AbstractTranslator {
  def translate(v: Ast.expr): String
}
