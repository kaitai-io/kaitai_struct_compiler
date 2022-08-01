package io.kaitai.struct.format

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.problems.KSYParseError

sealed trait RepeatSpec
case class RepeatExpr(expr: Ast.expr) extends RepeatSpec
case class RepeatUntil(expr: Ast.expr) extends RepeatSpec
case object RepeatEos extends RepeatSpec
case object NoRepeat extends RepeatSpec

object RepeatSpec {
  def fromYaml(
    srcMap: Map[String, Any],
    path: List[String]
  ): (RepeatSpec, Set[String]) = {
    val repeat = ParseUtils.getOptValueStr(srcMap, "repeat", path)
    val repeatExpr = ParseUtils.getOptValueExpression(srcMap, "repeat-expr", path)
    val repeatUntil = ParseUtils.getOptValueExpression(srcMap, "repeat-until", path)

    repeat match {
      case None =>
        (NoRepeat, Set())
      case Some("until") =>
        val spec = repeatUntil match {
          case Some(expr) => RepeatUntil(expr)
          case None =>
            throw KSYParseError.withText(
              "`repeat: until` requires a `repeat-until` expression",
              path ++ List("repeat")
            )
        }
        (spec, Set("repeat-until"))
      case Some("expr") =>
        val spec = repeatExpr match {
          case Some(expr) => RepeatExpr(expr)
          case None =>
            throw KSYParseError.withText(
              "`repeat: expr` requires a `repeat-expr` expression",
              path ++ List("repeat")
            )
        }
        (spec, Set("repeat-expr"))
      case Some("eos") =>
        (RepeatEos, Set())
      case Some(other) =>
        throw KSYParseError.badDictValue(
          Set("until", "expr", "eos"), other, path ++ List("repeat")
        )
    }
  }
}
