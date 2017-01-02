package io.kaitai.struct.translators.components

import io.kaitai.struct.exprlang.Ast

trait CTernaryOperator {
  import CTernaryOperator.IF_EXP_PRIO

  def translate(v: Ast.expr, outerPriority: Int): String

  def doIfExp(condition: Ast.expr, ifTrue: Ast.expr, ifFalse: Ast.expr): (String, Int) = (
    s"${translate(condition, IF_EXP_PRIO)} ? ${translate(ifTrue, IF_EXP_PRIO)} : ${translate(ifFalse, IF_EXP_PRIO)}",
    IF_EXP_PRIO
  )
}

object CTernaryOperator {
  val IF_EXP_PRIO = 200
}
