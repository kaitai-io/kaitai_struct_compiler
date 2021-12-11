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

    (repeat, repeatExpr, repeatUntil) match {
      // Only `repeat-x: ...` or nothing
      case (None, None,        None) => (NoRepeat, Set())
      case (None, Some(count), None) => (RepeatExpr(count),  Set("repeat-expr"))
      case (None, None, Some(until)) => (RepeatUntil(until), Set("repeat-until"))

      // Both `repeat: x` and `repeat-x: ...`
      case (Some("eos"),   None,        None) => (RepeatEos, Set())
      case (Some("expr"),  Some(count), None) => (RepeatExpr(count),  Set("repeat-expr"))
      case (Some("until"), None, Some(until)) => (RepeatUntil(until), Set("repeat-until"))

      // Only `repeat: x`
      case (Some("eos"),   Some(_), Some(_)) |
           (Some("expr"),  Some(_), Some(_)) |
           (Some("until"), Some(_), Some(_)) => throw KSYParseError.withText(
        "either `repeat: eos`, or `repeat-expr`, or `repeat-until` must be specified",
        path :+ "repeat"
      )
      case (Some("expr"),  None, None) => throw KSYParseError.withText(
        "`repeat: expr` requires a `repeat-expr` key",
        path :+ "repeat"
      )
      case (Some("until"), None, None) => throw KSYParseError.withText(
        "`repeat: until` requires a `repeat-until` key",
        path :+ "repeat"
      )

      // Incompatible combinations
      case (Some(_), Some(_), _) => throw KSYParseError.withText(
        "`repeat-expr` requires either a `repeat: expr` or absence of a `repeat` key",
        path :+ "repeat"
      )
      case (Some(_), _, Some(_)) => throw KSYParseError.withText(
        "`repeat-until` requires either a `repeat: until` or absence of a `repeat` key",
        path :+ "repeat"
      )
      case (_, Some(_), Some(_)) => throw KSYParseError.withText(
        "either `repeat-expr` or `repeat-until` must be specified",
        path :+ "repeat-expr"
      )

      case (Some(other), _, _) => throw KSYParseError.badDictValue(
        Set("until", "expr", "eos"), other,
        path :+ "repeat"
      )
    }
  }
}
