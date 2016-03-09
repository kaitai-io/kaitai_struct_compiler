package io.kaitai.struct.languages

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format._

import scala.collection.mutable.ListBuffer

/**
  * Helper trait for languages where single parsing of every standard or user data type is done as expression, i.e. an
  * rvalue. In these languages, "attrStdTypeParse" is replaced with higher-level API: "stdTypeParseExpr" and
  * "handleAssignment".
  */
trait EveryReadIsExpression extends LanguageCompiler {
  override def attrParse(attr: AttrLikeSpec, id: String, extraAttrs: ListBuffer[AttrSpec], io: String): Unit = {
    attr.cond.ifExpr match {
      case Some(e) => condIfHeader(e)
      case None => // ignore
    }

    attr.cond.repeat match {
      case RepeatEos =>
        condRepeatEosHeader(id, io, attr.dataType, needRaw(attr.dataType))
        attrParse2(id, attr.dataType, io, extraAttrs, attr.cond.repeat)
        condRepeatEosFooter
      case RepeatExpr(repeatExpr: Ast.expr) =>
        condRepeatExprHeader(id, io, attr.dataType, needRaw(attr.dataType), repeatExpr)
        attrParse2(id, attr.dataType, io, extraAttrs, attr.cond.repeat)
        condRepeatExprFooter
      case NoRepeat =>
        attrParse2(id, attr.dataType, io, extraAttrs, attr.cond.repeat)
    }

    attr.cond.ifExpr match {
      case Some(e) => condIfFooter(e)
      case None => // ignore
    }
  }

  def attrParse2(id: String, dataType: BaseType, io: String, extraAttrs: ListBuffer[AttrSpec], rep: RepeatSpec): Unit = {
    dataType match {
      case FixedBytesType(c, _) =>
        attrFixedContentsParse(id, c)
      case t: UserType =>
        val newIO = if (needRaw(t)) {
          // we have a fixed buffer, thus we shall create separate IO for it
          val rawId = s"_raw_$id"
          val byteType = t match {
            case UserTypeByteLimit(_, size) => BytesLimitType(size, None)
            case UserTypeEos(_) => BytesEosType(None)
          }

          attrParse2(rawId, byteType, io, extraAttrs, rep)

          val extraType = rep match {
            case NoRepeat => byteType
            case _ => ArrayType(byteType)
          }

          extraAttrs += AttrSpec(rawId, extraType)

          allocateIO(rawId, rep)
        } else {
          // no fixed buffer, just use regular IO
          normalIO
        }
        val expr = parseExpr(dataType, newIO)
        handleAssignment(id, expr, rep)

      case t: BytesType =>
        // use intermediate variable name, if we'll be doing post-processing
        val rawId = t.process match {
          case None => id
          case Some(_) =>
            extraAttrs += AttrSpec(s"_raw_$id", t)
            s"_raw_$id"
        }

        val expr = parseExpr(dataType, io)
        handleAssignment(rawId, expr, rep)

        // apply post-processing
        t.process.foreach((proc) => attrProcess(proc, rawId, id))
      case _ =>
        val expr = parseExpr(dataType, io)
        handleAssignment(id, expr, rep)
    }
  }

  def needRaw(dataType: BaseType): Boolean = {
    dataType match {
      case UserTypeByteLimit(_, _) | UserTypeEos(_) => true
      case _ => false
    }
  }

  def handleAssignment(id: String, expr: String, rep: RepeatSpec): Unit = {
    rep match {
      case RepeatExpr(_) => handleAssignmentRepeatExpr(id, expr)
      case RepeatEos => handleAssignmentRepeatEos(id, expr)
      case NoRepeat => handleAssignmentSimple(id, expr)
    }
  }

  def handleAssignmentRepeatEos(id: String, expr: String): Unit
  def handleAssignmentRepeatExpr(id: String, expr: String): Unit
  def handleAssignmentSimple(id: String, expr: String): Unit

  def parseExpr(dataType: BaseType, io: String): String
}
