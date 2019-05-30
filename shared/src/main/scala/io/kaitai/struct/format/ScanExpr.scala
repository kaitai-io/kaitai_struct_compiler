package io.kaitai.struct.format

import io.kaitai.struct.exprlang.{Ast, Expressions}

sealed trait ScanExpr

case class ScanCustom(name: List[String], args: Seq[Ast.expr]) extends ScanExpr

object ScanExpr {
  private val ReCustom = "^([a-z][a-z0-9_.]*)\\(\\s*(.*?)\\s*\\)$".r
  private val ReCustomNoArg = "^([a-z][a-z0-9_.]*)$".r

  // This method is called in fromYalm() to parse the content.
  def fromStr(s: Option[String], path: List[String]): Option[ScanExpr] = {
    println(s, path)
    s match {
      case None =>
        None
      case Some(op) =>
        try {
          Some(op match {
            case ReCustom(name, args) =>
              ScanCustom(name.split('.').toList, Expressions.parseList(args))
            case ReCustomNoArg(name) =>
              ScanCustom(name.split('.').toList, Seq())
            case _ =>
              throw YAMLParseException.badProcess(op, path)
          })
        } catch {
          case epe: Expressions.ParseException =>
            throw YAMLParseException.expression(epe, path)
        }
    }
  }
}
