package io.kaitai.struct.format

import io.kaitai.struct.exprlang.{Ast, Expressions}
import io.kaitai.struct.problems.KSYParseError

sealed trait ProcessExpr

case object ProcessZlib extends ProcessExpr
case class ProcessXor(key: Ast.expr) extends ProcessExpr
case class ProcessRotate(left: Boolean, key: Ast.expr) extends ProcessExpr
case class ProcessCustom(name: List[String], args: Seq[Ast.expr]) extends ProcessExpr

object ProcessExpr {
  private val ReXor = "^xor\\(\\s*(.*?)\\s*\\)$".r
  private val ReRotate = "^ro(l|r)\\(\\s*(.*?)\\s*\\)$".r
  private val ReCustom = "^([a-z][a-z0-9_.]*)\\(\\s*(.*?)\\s*\\)$".r
  private val ReCustomNoArg = "^([a-z][a-z0-9_.]*)$".r

  def fromStr(s: Option[String], path: List[String]): Option[ProcessExpr] = {
    s match {
      case None =>
        None
      case Some(op) =>
        try {
          Some(op match {
            case "zlib" =>
              ProcessZlib
            case ReXor(arg) =>
              ProcessXor(Expressions.parse(arg))
            case ReRotate(dir, arg) =>
              ProcessRotate(dir == "l", Expressions.parse(arg))
            case ReCustom(name, args) =>
              ProcessCustom(name.split('.').toList, Expressions.parseList(args))
            case ReCustomNoArg(name) =>
              ProcessCustom(name.split('.').toList, Seq())
            case _ =>
              throw KSYParseError.badProcess(op, path)
          })
        } catch {
          case epe: Expressions.ParseException =>
            throw KSYParseError.expression(epe, path)
        }
    }
  }
}
