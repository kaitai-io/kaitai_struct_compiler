package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.format._
import io.kaitai.struct.translators.GoTranslator

import scala.collection.mutable.ListBuffer

trait GoReads extends CommonReads with SwitchOps {
  val translator: GoTranslator

  override def attrParse2(
    id: Identifier,
    dataType: DataType,
    io: String,
    extraAttrs: ListBuffer[AttrSpec],
    rep: RepeatSpec,
    isRaw: Boolean
  ): Unit = {
    dataType match {
      case FixedBytesType(c, _) =>
        attrFixedContentsParse(id, c)
//      case t: UserType =>
//        attrUserTypeParse(id, t, io, extraAttrs, rep)
//      case t: BytesType =>
//        attrBytesTypeParse(id, t, io, extraAttrs, rep, isRaw)
//      case SwitchType(on, cases) =>
//        attrSwitchTypeParse(id, on, cases, io, extraAttrs, rep)
//      case t: StrFromBytesType =>
//        val expr = translator.bytesToStr(parseExprBytes(t.bytes, io), Ast.expr.Str(t.encoding))
//        handleAssignment(id, expr, rep, isRaw)
//      case t: EnumType =>
//        val expr = translator.doEnumById(t.enumSpec.get.name, parseExpr(t.basedOn, io))
//        handleAssignment(id, expr, rep, isRaw)
      case _ =>
        val expr = parseExpr(dataType, io)
        handleAssignment(id, expr, rep, isRaw)
    }
  }

  def handleAssignment(id: Identifier, expr: String, rep: RepeatSpec, isRaw: Boolean): Unit = {
    rep match {
      case RepeatEos => handleAssignmentRepeatEos(id, expr)
      case RepeatExpr(_) => handleAssignmentRepeatExpr(id, expr)
      case RepeatUntil(_) => handleAssignmentRepeatUntil(id, expr, isRaw)
      case NoRepeat => handleAssignmentSimple(id, expr)
    }
  }

  def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit
  def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit
  def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit
  def handleAssignmentSimple(id: Identifier, expr: String): Unit

  def parseExpr(dataType: DataType, io: String): String
}
