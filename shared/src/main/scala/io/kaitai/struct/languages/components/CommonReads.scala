package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype._
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

    val needsDebug = attrDebugNeeded(id)

    if (needsDebug) {
      attrDebugStart(id, attr.dataType, Some(io), NoRepeat)
      if (attr.cond.repeat != NoRepeat)
        attrDebugArrInit(id, attr.dataType)
    }

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

    if (needsDebug)
      attrDebugEnd(id, attr.dataType, io, NoRepeat)

    // More position management + set calculated flag after parsing for ParseInstanceSpecs
    attr match {
      case pis: ParseInstanceSpec =>
        // Restore position, if applicable
        if (pis.pos.isDefined)
          popPos(io)

        // Mark parse instance as calculated
        instanceSetCalculated(pis.id)
      case _ => // no seeking required for sequence attributes
    }

    attrParseIfFooter(attr.cond.ifExpr)
  }

  def attrParse0(id: Identifier, attr: AttrLikeSpec, io: String, defEndian: Option[FixedEndian]): Unit = {
    if (attr.cond.repeat != NoRepeat)
      (ExtraAttrs.forAttr(attr, this) ++ List(attr)).foreach(a => condRepeatInitAttr(a.id, a.dataType))
    attr.cond.repeat match {
      case RepeatEos =>
        condRepeatEosHeader(id, io, attr.dataType)
      case RepeatExpr(repeatExpr: Ast.expr) =>
        condRepeatExprHeader(id, io, attr.dataType, repeatExpr)
      case RepeatUntil(untilExpr: Ast.expr) =>
        condRepeatUntilHeader(id, io, attr.dataType, untilExpr)
      case NoRepeat =>
    }
    attrParse2(id, attr.dataType, io, attr.cond.repeat, false, defEndian)
    attrValidateAll(attr)
    attr.cond.repeat match {
      case RepeatEos =>
        condRepeatEosFooter
      case _: RepeatExpr =>
        condRepeatExprFooter
      case RepeatUntil(untilExpr: Ast.expr) =>
        condRepeatUntilFooter(id, io, attr.dataType, untilExpr)
      case NoRepeat =>
    }
  }

  def attrParse2(id: Identifier, dataType: DataType, io: String, rep: RepeatSpec, isRaw: Boolean, defEndian: Option[FixedEndian], assignType: Option[DataType] = None): Unit

  def attrDebugStart(attrId: Identifier, attrType: DataType, io: Option[String], repeat: RepeatSpec): Unit = {}
  def attrDebugArrInit(attrId: Identifier, attrType: DataType): Unit = {}
  def attrDebugEnd(attrName: Identifier, attrType: DataType, io: String, repeat: RepeatSpec): Unit = {}

  def attrDebugNeeded(attrId: Identifier): Boolean = false

  /**
    * Runs all validation procedures requested for an attribute.
    * @param attr attribute to run validations for
    */
  def attrValidateAll(attr: AttrLikeSpec) =
    attr.valid.foreach(valid => attrValidate(attr.id, attr, valid))
}
