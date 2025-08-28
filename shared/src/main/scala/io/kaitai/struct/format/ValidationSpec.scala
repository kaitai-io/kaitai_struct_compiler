package io.kaitai.struct.format

import io.kaitai.struct.exprlang.{Ast, Expressions}
import io.kaitai.struct.problems.KSYParseError

sealed trait ValidationSpec

case class ValidationEq(value: Ast.expr) extends ValidationSpec
case class ValidationMin(min: Ast.expr) extends ValidationSpec
case class ValidationMax(max: Ast.expr) extends ValidationSpec
case class ValidationRange(min: Ast.expr, max: Ast.expr) extends ValidationSpec
case class ValidationAnyOf(values: List[Ast.expr]) extends ValidationSpec
case class ValidationInEnum() extends ValidationSpec
case class ValidationExpr(checkExpr: Ast.expr) extends ValidationSpec

object ValidationEq {
  val LEGAL_KEYS = Set("eq")

  def fromMap(src: Map[String, Any], path: List[String]): Option[ValidationEq] =
    ParseUtils.getOptValueExpression(src, "eq", path).map { case eqExpr =>
      ParseUtils.ensureLegalKeys(src, LEGAL_KEYS, path)
      ValidationEq(eqExpr)
    }
}

object ValidationRange {
  val LEGAL_KEYS = Set("min", "max")

  def fromMap(src: Map[String, Any], path: List[String]): Option[ValidationSpec] = {
    val minExprOpt = ParseUtils.getOptValueExpression(src, "min", path)
    val maxExprOpt = ParseUtils.getOptValueExpression(src, "max", path)

    (minExprOpt, maxExprOpt) match {
      case (None, None) => None
      case (Some(minExpr), None) =>
        ParseUtils.ensureLegalKeys(src, LEGAL_KEYS, path)
        Some(ValidationMin(minExpr))
      case (None, Some(maxExpr)) =>
        ParseUtils.ensureLegalKeys(src, LEGAL_KEYS, path)
        Some(ValidationMax(maxExpr))
      case (Some(minExpr), Some(maxExpr)) =>
        ParseUtils.ensureLegalKeys(src, LEGAL_KEYS, path)
        Some(ValidationRange(minExpr, maxExpr))
    }
  }
}

object ValidationAnyOf {
  val LEGAL_KEYS = Set("any-of")

  def fromMap(src: Map[String, Any], path: List[String]): Option[ValidationAnyOf] = {
    if (src.get("any-of").nonEmpty) {
      ParseUtils.ensureLegalKeys(src, LEGAL_KEYS, path)
      Some(ValidationAnyOf(
        ParseUtils.getList(src, "any-of", ParseUtils.asExpression, path)
      ))
    } else {
      None
    }
  }
}

object ValidationInEnum {
  val LEGAL_KEYS = Set("in-enum")

  def fromMap(src: Map[String, Any], path: List[String]): Option[ValidationInEnum] =
    ParseUtils.getOptValueBool(src, "in-enum", path).map { case boolVal =>
      ParseUtils.ensureLegalKeys(src, LEGAL_KEYS, path)
      if (!boolVal) {
        throw KSYParseError.withText(
          "only `true` is supported as value, got `false`" +
          " (if you don't want any validation, omit the `valid` key)",
          path ++ List("in-enum")
        )
      }
      ValidationInEnum()
    }
}

object ValidationExpr {
  val LEGAL_KEYS = Set("expr")

  def fromMap(src: Map[String, Any], path: List[String]): Option[ValidationExpr] =
    ParseUtils.getOptValueExpression(src, "expr", path).map { case expr =>
      ParseUtils.ensureLegalKeys(src, LEGAL_KEYS, path)
      ValidationExpr(expr)
    }
}

object ValidationSpec {
  val LEGAL_KEYS =
    ValidationEq.LEGAL_KEYS ++
    ValidationRange.LEGAL_KEYS ++
    ValidationAnyOf.LEGAL_KEYS ++
    ValidationInEnum.LEGAL_KEYS ++
    ValidationExpr.LEGAL_KEYS

  def fromYaml(src: Any, path: List[String]): ValidationSpec = {
    src match {
      case value: String =>
        fromString(value, path)
      case x: Boolean =>
        fromString(x.toString, path)
      case x: Int =>
        fromString(x.toString, path)
      case x: Long =>
        fromString(x.toString, path)
      case srcMap: Map[Any, Any] =>
        fromMap(ParseUtils.anyMapToStrMap(srcMap, path), path)
      case _ =>
        throw KSYParseError.badType("string or map", src, path)
    }
  }

  def fromString(value: String, path: List[String]): ValidationSpec =
    ValidationEq(Expressions.parse(value))

  def fromMap(src: Map[String, Any], path: List[String]): ValidationSpec = {
    val opt1 = ValidationEq.fromMap(src, path)
    if (opt1.nonEmpty)
      return opt1.get
    val opt2 = ValidationRange.fromMap(src, path)
    if (opt2.nonEmpty)
      return opt2.get
    val opt3 = ValidationAnyOf.fromMap(src, path)
    if (opt3.nonEmpty)
      return opt3.get
    val opt4 = ValidationInEnum.fromMap(src, path)
    if (opt4.nonEmpty)
      return opt4.get
    val opt5 = ValidationExpr.fromMap(src, path)
    if (opt5.nonEmpty)
      return opt5.get
    // No validation templates matched, check for any bogus keys
    ParseUtils.ensureLegalKeys(src, LEGAL_KEYS, path)

    // No bogus keys found, likely an empty validation
    throw KSYParseError.noKeys(path, LEGAL_KEYS)
  }
}
