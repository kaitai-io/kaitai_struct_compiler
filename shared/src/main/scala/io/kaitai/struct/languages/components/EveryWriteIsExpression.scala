package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype.FixedEndian
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._

import scala.collection.mutable.ListBuffer
import io.kaitai.struct.datatype._

trait EveryWriteIsExpression
  extends LanguageCompiler
    with ObjectOrientedLanguage
    with EveryReadIsExpression
    with GenericChecks {
  override def attrWrite(attr: AttrLikeSpec, id: Identifier, defEndian: Option[Endianness]): Unit = {
    val checksShouldDependOnIo: Option[Boolean] =
      if (userExprDependsOnIo(attr.cond.ifExpr)) {
        None
      } else {
        Some(true)
      }

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

    defEndian match {
      case Some(_: CalcEndian) | Some(InheritedEndian) =>
        // FIXME: rename to indicate that it can be used for both parsing/writing
        attrParseHybrid(
          () => attrWrite0(id, attr, io, Some(LittleEndian), checksShouldDependOnIo),
          () => attrWrite0(id, attr, io, Some(BigEndian), checksShouldDependOnIo)
        )
      case None =>
        attrWrite0(id, attr, io, None, checksShouldDependOnIo)
      case Some(fe: FixedEndian) =>
        attrWrite0(id, attr, io, Some(fe), checksShouldDependOnIo)
    }

    attr match {
      case pis: ParseInstanceSpec =>
        // Restore position, if applicable
        if (pis.pos.isDefined)
          popPos(io)
      case _ => // no seeking required for sequence attributes
    }

    attrParseIfFooter(attr.cond.ifExpr)
  }

  def attrWrite0(
    id: Identifier,
    attr: AttrLikeSpec,
    io: String,
    defEndian: Option[FixedEndian],
    checksShouldDependOnIo: Option[Boolean]
  ): Unit = {
    if (attr.cond.repeat != NoRepeat)
      condRepeatCommonWriteInit(id, attr.dataType, needRaw(attr.dataType))
    attr.cond.repeat match {
      case RepeatEos =>
      case RepeatExpr(repeatExpr: Ast.expr) =>
        attrRepeatExprCheck(id, repeatExpr, checksShouldDependOnIo)
      case RepeatUntil(untilExpr: Ast.expr) =>
        if (checksShouldDependOnIo.map(shouldDepend => shouldDepend == false).getOrElse(true))
          attrAssertUntilNotEmpty(id)
      case NoRepeat =>
    }
    if (attr.cond.repeat != NoRepeat) {
      condRepeatCommonHeader(id, io, attr.dataType)
    }
    attr.cond.repeat match {
      case RepeatEos =>
        attrIsEofCheck(id, false, io)
      case _ =>
    }
    attrWrite2(id, attr.dataType, io, attr.cond.repeat, false, defEndian, checksShouldDependOnIo)
    attr.cond.repeat match {
      case repUntil: RepeatUntil =>
        attrAssertUntilCond(id, attr.dataType, repUntil, false, checksShouldDependOnIo)
      case _ =>
    }
    if (attr.cond.repeat != NoRepeat) {
      condRepeatCommonFooter
    }
    attr.cond.repeat match {
      case RepeatEos =>
        attrIsEofCheck(id, true, io)
      case _ =>
    }
  }

  def attrWrite2(
    id: Identifier,
    dataType: DataType,
    io: String,
    rep: RepeatSpec,
    isRaw: Boolean,
    defEndian: Option[FixedEndian],
    checksShouldDependOnIo: Option[Boolean],
    exprTypeOpt: Option[DataType] = None
  ): Unit = {
    dataType match {
      case t: UserType =>
        attrUserTypeWrite(id, t, io, rep, isRaw, defEndian, checksShouldDependOnIo, exprTypeOpt)
      case t: BytesType =>
        attrBytesTypeWrite(id, t, io, rep, isRaw, checksShouldDependOnIo, exprTypeOpt)
      case st: SwitchType =>
        val isNullable = if (switchBytesOnlyAsRaw) {
          st.isNullableSwitchRaw
        } else {
          st.isNullable
        }

        attrSwitchTypeWrite(id, st.on, st.cases, io, rep, defEndian, checksShouldDependOnIo, isNullable, st.combinedType)
      case t: StrFromBytesType =>
        attrStrTypeWrite(id, t, io, rep, isRaw, checksShouldDependOnIo, exprTypeOpt)
      case t: EnumType =>
        val expr = itemExpr(id, rep, isRaw)
        val exprType = internalEnumIntType(t.basedOn)
        attrPrimitiveWrite(io, Ast.expr.Attribute(expr, Ast.identifier("to_i")), t.basedOn, defEndian, Some(exprType))
      case _ =>
        val expr = itemExpr(id, rep, isRaw)
        attrPrimitiveWrite(io, expr, dataType, defEndian, exprTypeOpt)
    }
  }

  def attrBytesTypeWrite(
    id: Identifier,
    t: BytesType,
    io: String,
    rep: RepeatSpec,
    isRaw: Boolean,
    checksShouldDependOnIo: Option[Boolean],
    exprTypeOpt: Option[DataType]
  ): Unit = {
    val idToWrite = t.process match {
      case Some(proc) =>
        val rawId = RawIdentifier(id)
        attrUnprocess(proc, id, rawId, rep, t, exprTypeOpt)
        rawId
      case None =>
        id
    }
    val expr = if (idToWrite.isInstanceOf[RawIdentifier] && rep != NoRepeat) {
      // NOTE: This special handling isn't normally needed and one can just use
      // `itemExpr(idToWrite, rep, isRaw)` as usual. The `itemExpr` method assumes that the
      // expression it's supposed to generate will be used in a loop where the iteration
      // variable `Identifier.INDEX` is available (usually called just `i`) and uses it. This
      // is a good default, but it doesn't work if the expression is used between
      // `subIOWriteBackHeader` and `subIOWriteBackFooter` (see `attrUserTypeWrite` below),
      // because in Java the loop control variable `i` is not "final" or "effectively final".
      //
      // The workaround is to change the expression so that it doesn't depend on the `i`
      // variable. We can do that here, because the `RawIdentifier(...)` array starts empty
      // before the loop and each element is added by `attrUnprocess` in each loop iteration -
      // so the current item is just the last entry in the `RawIdentifier(...)` array.
      //
      // See test ProcessRepeatUsertype that requires this.
      val astId = Ast.expr.InternalName(idToWrite)
      Ast.expr.Subscript(
        astId,
        Ast.expr.BinOp(
          Ast.expr.Attribute(
            astId,
            Ast.identifier("size")
          ),
          Ast.operator.Sub,
          Ast.expr.IntNum(1)
        )
      )
    } else {
      itemExpr(idToWrite, rep, isRaw)
    }
    attrBytesTypeWrite2(id, io, expr, t, checksShouldDependOnIo, exprTypeOpt)
  }

  def attrStrTypeWrite(
    id: Identifier,
    t: StrFromBytesType,
    io: String,
    rep: RepeatSpec,
    isRaw: Boolean,
    checksShouldDependOnIo: Option[Boolean],
    exprTypeOpt: Option[DataType]
  ): Unit = {
    val expr = exprStrToBytes(itemExpr(id, rep, isRaw), t.encoding)
    attrBytesTypeWrite2(id, io, expr, t.bytes, checksShouldDependOnIo, exprTypeOpt)
  }

  def attrBytesTypeWrite2(
    id: Identifier,
    io: String,
    expr: Ast.expr,
    t: BytesType,
    checksShouldDependOnIo: Option[Boolean],
    exprTypeOpt: Option[DataType]
  ): Unit = {
    attrBytesCheck(id, expr, t, checksShouldDependOnIo)
    t match {
      case bt: BytesEosType =>
        attrBytesLimitWrite2(io, expr, bt, exprIORemainingSize(io), bt.padRight, bt.terminator, bt.include, exprTypeOpt)
        attrIsEofCheck(id, true, io)
      case bt: BytesLimitType =>
        attrBytesLimitWrite2(io, expr, bt, expression(bt.size), bt.padRight, bt.terminator, bt.include, exprTypeOpt)
      case t: BytesTerminatedType =>
        attrPrimitiveWrite(io, expr, t, None, exprTypeOpt)
        if (!t.include) {
          if (!t.consume) {
            blockScopeHeader
            pushPos(io)
          }
          // FIXME: does not take `eos-error: false` into account (assumes `eos-error: true`)
          attrPrimitiveWrite(io, Ast.expr.IntNum(t.terminator), Int1Type(false), None, None)
          if (!t.consume) {
            popPos(io)
            blockScopeFooter
          }
        }
    }
  }

  def attrBytesLimitWrite2(
    io: String,
    expr: Ast.expr,
    bt: BytesType,
    sizeExpr: String,
    padRight: Option[Int],
    terminator: Option[Int],
    include: Boolean,
    exprTypeOpt: Option[DataType]
  ): Unit = {
    val (termArg, padRightArg) = (terminator, padRight, include) match {
      case (None, None, false) =>
        // no terminator, no padding => just a regular output
        // validation should check that expression's length matches size
        attrPrimitiveWrite(io, expr, bt, None, exprTypeOpt)
        return
      case (_, None, true) =>
        // terminator included, no padding => pad with zeroes
        (0, 0)
      case (_, Some(p), true) =>
        // terminator included, padding specified
        (p, p)
      case (Some(t), None, false) =>
        // only terminator given, don't care about what's gonna go after that
        // we'll just pad with zeroes
        (t, 0)
      case (None, Some(p), false) =>
        // only padding given, just add terminator equal to padding
        (p, p)
      case (Some(t), Some(p), false) =>
        // both terminator and padding specified
        (t, p)
    }
    attrBytesLimitWrite(io, expr, sizeExpr, termArg, padRightArg)
  }

  def attrUserTypeWrite(
    id: Identifier,
    t: UserType,
    io: String,
    rep: RepeatSpec,
    isRaw: Boolean,
    defEndian: Option[FixedEndian],
    checksShouldDependOnIo: Option[Boolean],
    exprTypeOpt: Option[DataType] = None
  ) = {
    val exprType = exprTypeOpt.getOrElse(t)
    val expr = itemExpr(id, rep, isRaw)

    t match {
      case _: UserTypeInstream =>
        attrUserTypeInstreamWrite(io, expr, t, exprType)
      case utb: UserTypeFromBytes =>
        val rawId = RawIdentifier(id)
        val byteType = utb.bytes

        /** @note Must be kept in sync with [[ExtraAttrs.writeNeedsOuterSize]] */
        val outerSize = byteType match {
          case blt: BytesLimitType =>
            translator.translate(blt.size)
          case _: BytesEosType =>
            exprIORemainingSize(io)
          case _: BytesTerminatedType =>
            translator.translate(itemExpr(OuterSizeIdentifier(id), rep, isRaw))
        }

        /** @note Must be kept in sync with [[ExtraAttrs.writeNeedsInnerSize]] */
        val innerSize = if (writeNeedsInnerSize(utb)) {
          translator.translate(itemExpr(InnerSizeIdentifier(id), rep, isRaw))
        } else {
          outerSize
        }

        this match {
          //      case thisStore: AllocateAndStoreIO =>
          //        val ourIO = thisStore.allocateIO(rawId, rep)
          //        Utils.addUniqueAttr(extraAttrs, AttrSpec(List(), ourIO, KaitaiStreamType))
          //        privateMemberName(ourIO)
          case thisLocal: AllocateIOLocalVar =>
            val ioFixed = thisLocal.allocateIOFixed(rawId, innerSize)
            addChildIO(io, ioFixed)

            blockScopeHeader

            pushPosForSubIOWriteBackHandler(io)
            seekRelative(io, outerSize)
            byteType match {
              case t: BytesTerminatedType =>
                // FIXME: does not take `eos-error: false` into account (assumes `eos-error: true`)
                if (!t.include && t.consume) {
                  // terminator can only be 1 byte long at the moment
                  seekRelative(io, expression(Ast.expr.IntNum(1)))
                }
              case _ => // do nothing
            }

            byteType.process.foreach { (process) =>
              attrUnprocessPrepareBeforeSubIOHandler(process, rawId)
            }

            {
              val parentIO = subIOWriteBackHeader(ioFixed)
              handleAssignment(rawId, exprStreamToByteArray(ioFixed), rep, true)
              attrBytesTypeWrite(rawId, byteType, parentIO, rep, isRaw, checksShouldDependOnIo, exprTypeOpt)
              subIOWriteBackFooter
            }

            blockScopeFooter

            attrUserTypeInstreamWrite(ioFixed, expr, t, exprType)
        }
    }
  }
  def attrSwitchTypeWrite(
    id: Identifier,
    on: Ast.expr,
    cases: Map[Ast.expr, DataType],
    io: String,
    rep: RepeatSpec,
    defEndian: Option[FixedEndian],
    checksShouldDependOnIo: Option[Boolean],
    isNullable: Boolean,
    assignType: DataType
  ): Unit = {
    if (isNullable)
      condIfSetNull(id)

    switchCases[DataType](id, on, cases,
      (dataType) => {
        if (isNullable)
          condIfSetNonNull(id)
        attrWrite2(id, dataType, io, rep, false, defEndian, checksShouldDependOnIo, Some(assignType))
      },
      (dataType) => if (switchBytesOnlyAsRaw) {
        dataType match {
          case t: BytesType =>
            attrWrite2(RawIdentifier(id), dataType, io, rep, false, defEndian, checksShouldDependOnIo, Some(assignType))
          case _ =>
            attrWrite2(id, dataType, io, rep, false, defEndian, checksShouldDependOnIo, Some(assignType))
        }
      } else {
        attrWrite2(id, dataType, io, rep, false, defEndian, checksShouldDependOnIo, Some(assignType))
      }
    )
  }

  def internalEnumIntType(basedOn: IntType): DataType

  def attrPrimitiveWrite(io: String, expr: Ast.expr, dt: DataType, defEndian: Option[FixedEndian], exprTypeOpt: Option[DataType]): Unit
  def attrBytesLimitWrite(io: String, expr: Ast.expr, size: String, term: Int, padRight: Int): Unit
  def attrUserTypeInstreamWrite(io: String, expr: Ast.expr, t: DataType, exprType: DataType): Unit
  def attrWriteStreamToStream(srcIo: String, dstIo: String): Unit
  def exprStreamToByteArray(ioFixed: String): String

  def attrUnprocess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec, dt: BytesType, exprTypeOpt: Option[DataType]): Unit
  def attrUnprocessPrepareBeforeSubIOHandler(proc: ProcessExpr, varSrc: Identifier): Unit
}
