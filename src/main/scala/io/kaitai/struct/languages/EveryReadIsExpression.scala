package io.kaitai.struct.languages

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType.{BaseType, UserTypeByteLimit, BytesLimitType}
import io.kaitai.struct.format._

/**
  * Helper trait for languages where single parsing of every standard or user data type is done as expression, i.e. an
  * rvalue. In these languages, "attrStdTypeParse" is replaced with higher-level API: "stdTypeParseExpr" and
  * "handleAssignment".
  */
trait EveryReadIsExpression extends LanguageCompiler {
  override def attrStdTypeParse(id: String, attr: AttrLikeSpec): Unit = {
    handleAssignment(id, attr, stdTypeParseExpr(attr.dataType), normalIO)
  }

  override def attrNoTypeWithSize(id: String, attr: AttrLikeSpec): Unit = {
    attr.dataType match {
      case BytesLimitType(size, _) =>
        handleAssignment(id, attr, noTypeWithSizeExpr(size), normalIO)
      case UserTypeByteLimit(_, size) =>
        handleAssignment(id, attr, noTypeWithSizeExpr(size), normalIO)
    }
  }

  override def attrNoTypeWithSizeEos(id: String, attr: AttrLikeSpec): Unit = {
    handleAssignment(id, attr, noTypeWithSizeEosExpr, normalIO)
  }

  def stdTypeParseExpr(dataType: BaseType): String
  def noTypeWithSizeExpr(size: Ast.expr): String
  def noTypeWithSizeEosExpr: String

  def handleAssignment(id: String, attr: AttrLikeSpec, expr: String, io: String) = {
    attr.cond.ifExpr match {
      case Some(e) => condIfHeader(e)
      case None => // ignore
    }

    attr.cond.repeat match {
      case RepeatEos =>
        condRepeatEosHeader(id, io, attr.dataType)
        handleAssignmentRepeatEos(id, expr)
        //out.puts(s"${id}.add(${expr});")
        condRepeatEosFooter
      case RepeatExpr(repeatExpr: Ast.expr) =>
        condRepeatExprHeader(id, io, attr.dataType, repeatExpr)
        handleAssignmentRepeatExpr(id, expr)
        condRepeatExprFooter
      case NoRepeat =>
        handleAssignmentSimple(id, expr)
    }

    attr.cond.ifExpr match {
      case Some(e) => condIfFooter(e)
      case None => // ignore
    }
  }

  def condIfHeader(expr: Ast.expr): Unit
  def condIfFooter(expr: Ast.expr): Unit

  def condRepeatEosHeader(id: String, io: String, dataType: BaseType): Unit
  def handleAssignmentRepeatEos(id: String, expr: String): Unit
  def condRepeatEosFooter: Unit

  def condRepeatExprHeader(id: String, io: String, dataType: BaseType, repeatExpr: expr): Unit
  def handleAssignmentRepeatExpr(id: String, expr: String): Unit
  def condRepeatExprFooter: Unit

  def handleAssignmentSimple(id: String, expr: String): Unit
}
