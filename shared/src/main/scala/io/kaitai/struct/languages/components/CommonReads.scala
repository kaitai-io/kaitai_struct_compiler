package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype._
import io.kaitai.struct.datatype.DataType.{SwitchType, UserTypeFromBytes}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._

trait CommonReads extends LanguageCompiler {
  override def attrParse(attr: AttrLikeSpec, id: Identifier, defEndian: Option[Endianness]): Unit = {
    attrParseIfHeader(id, attr.cond.ifExpr)

    // Manage IO & seeking for ParseInstances
    val io = attr match {
      case pis: ParseInstanceSpec =>
        val io = pis.io match {
          case None => normalIO
          case Some(ex) => useIO(ex)
        }
        pis.pos.foreach { pos =>
          pushPos(io)
          seek(io, pos)
        }
        io
      case _ =>
        // no seeking required for sequence attributes
        normalIO
    }

    if (config.readStoresPos)
      attrDebugStart(id, attr.dataType, Some(io), NoRepeat)

    defEndian match {
      case Some(_: CalcEndian) | Some(InheritedEndian) =>
        attrParseHybrid(
          () => attrParse0(id, attr, io, Some(LittleEndian)),
          () => attrParse0(id, attr, io, Some(BigEndian))
        )
      case None =>
        attrParse0(id, attr, io, None)
      case Some(fe: FixedEndian) =>
        attrParse0(id, attr, io, Some(fe))
    }

    if (config.readStoresPos)
      attrDebugEnd(id, attr.dataType, io, NoRepeat)

    // More position management after parsing for ParseInstanceSpecs
    attr match {
      case pis: ParseInstanceSpec =>
        if (pis.pos.isDefined)
          popPos(io)
      case _ => // no seeking required for sequence attributes
    }

    attrParseIfFooter(attr.cond.ifExpr)
  }

  def attrParse0(id: Identifier, attr: AttrLikeSpec, io: String, defEndian: Option[FixedEndian]): Unit = {
    attr.cond.repeat match {
      case RepeatEos =>
        condRepeatEosHeader(id, io, attr.dataType, needRaw(attr.dataType))
        attrParse2(id, attr.dataType, io, attr.cond.repeat, false, defEndian)
        condRepeatEosFooter
      case RepeatExpr(repeatExpr: Ast.expr) =>
        condRepeatExprHeader(id, io, attr.dataType, needRaw(attr.dataType), repeatExpr)
        attrParse2(id, attr.dataType, io, attr.cond.repeat, false, defEndian)
        condRepeatExprFooter
      case RepeatUntil(untilExpr: Ast.expr) =>
        condRepeatUntilHeader(id, io, attr.dataType, needRaw(attr.dataType), untilExpr)
        attrParse2(id, attr.dataType, io, attr.cond.repeat, false, defEndian)
        condRepeatUntilFooter(id, io, attr.dataType, needRaw(attr.dataType), untilExpr)
      case NoRepeat =>
        attrParse2(id, attr.dataType, io, attr.cond.repeat, false, defEndian)
    }
  }

  def attrParse2(id: Identifier, dataType: DataType, io: String, rep: RepeatSpec, isRaw: Boolean, defEndian: Option[FixedEndian], assignType: Option[DataType] = None): Unit

  def needRaw(dataType: DataType): Boolean = {
    dataType match {
      case _: UserTypeFromBytes => true
      case st: SwitchType => st.hasSize
      case _ => false
    }
  }

  def attrDebugStart(attrName: Identifier, attrType: DataType, io: Option[String], repeat: RepeatSpec): Unit = {}
  def attrDebugEnd(attrName: Identifier, attrType: DataType, io: String, repeat: RepeatSpec): Unit = {}
}
