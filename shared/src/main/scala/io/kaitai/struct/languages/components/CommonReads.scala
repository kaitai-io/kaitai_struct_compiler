package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype._
import io.kaitai.struct.datatype.DataType.{SwitchType, UserTypeFromBytes, BytesType}
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

    // Run validations (still inside "if", if applicable)
    attrValidateAll(attr)

    attrParseIfFooter(attr.cond.ifExpr)
  }

  def attrParse0(id: Identifier, attr: AttrLikeSpec, io: String, defEndian: Option[FixedEndian]): Unit = {
    if (attr.cond.repeat != NoRepeat)
      condRepeatCommonInit(id, attr.dataType, needRaw(attr.dataType))
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

  def needRaw(dataType: DataType): NeedRaw = {
    val rawIo = dataType match {
      case _: UserTypeFromBytes => true
      case st: SwitchType => st.hasSize
      case _ => false
    }
    val rawProcess = dataType match {
      case bt: BytesType => bt.process.nonEmpty
      case utfb: UserTypeFromBytes => utfb.bytes.process.nonEmpty
      case _ => false
    }
    (rawIo, rawProcess) match {
      case (true, false) => RawIo
      case (false, true) => RawProcess
      case (true, true) => RawIoProcess
      case _ => NotRaw
    }
  }

  def attrDebugStart(attrName: Identifier, attrType: DataType, io: Option[String], repeat: RepeatSpec): Unit = {}
  def attrDebugEnd(attrName: Identifier, attrType: DataType, io: String, repeat: RepeatSpec): Unit = {}

  /**
    * Runs all validation procedures requested for an attribute.
    * @param attr attribute to run validations for
    */
  def attrValidateAll(attr: AttrLikeSpec) =
    attr.valid.foreach(valid => attrValidate(attr.id, attr, valid))
}
