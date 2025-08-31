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
      ExtraAttrs.forAttr(attr, this)
        .filter(a => a.id.isInstanceOf[RawIdentifier])
        .foreach(a => condRepeatInitAttr(a.id, a.dataType))
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
        attrAssertUntilCond(id, attr.dataType, repUntil, checksShouldDependOnIo)
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

        attrSwitchTypeWrite(id, st.on, st.cases, io, rep, defEndian, checksShouldDependOnIo, st.combinedType)
      case t: StrFromBytesType =>
        attrStrTypeWrite(id, t, io, rep, isRaw, checksShouldDependOnIo, exprTypeOpt)
      case t: EnumType =>
        val expr = Identifier.itemExpr(id, rep)
        val exprType = internalEnumIntType(t.basedOn)
        attrPrimitiveWrite(io, Ast.expr.Attribute(expr, Ast.identifier("to_i")), t.basedOn, defEndian, Some(exprType))
      case _ =>
        val expr = Identifier.itemExpr(id, rep)
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
    val item = Identifier.itemExpr(idToWrite, rep)
    val itemBytes =
      if (exprTypeOpt.map(exprType => !exprType.isInstanceOf[BytesType]).getOrElse(false))
        Ast.expr.CastToType(item, Ast.typeId(false, Seq("bytes")))
      else
        item
    attrBytesTypeWrite2(id, io, itemBytes, t, checksShouldDependOnIo, exprTypeOpt)
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
    val item = Identifier.itemExpr(id, rep)
    val itemStr =
      if (exprTypeOpt.map(exprType => !exprType.isInstanceOf[StrType]).getOrElse(false))
        Ast.expr.CastToType(item, Ast.typeId(false, Seq("str")))
      else
        item
    val bytes = exprStrToBytes(itemStr, t.encoding)
    attrBytesTypeWrite2(id, io, bytes, t.bytes, checksShouldDependOnIo, exprTypeOpt)
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
        val term =
          if (t.terminator.length == 1) {
            t.terminator.head & 0xff
          } else {
            throw new NotImplementedError("multibyte terminators cannot be serialized yet")
          }
        if (t.include) {
          val actualIndexOfTerm = exprByteArrayIndexOf(expr, term)
          if (!t.eosError) {
            condIfHeader(Ast.expr.Compare(actualIndexOfTerm, Ast.cmpop.Eq, Ast.expr.IntNum(-1)))
            attrIsEofCheck(id, true, io)
            condIfFooter
          }
        } else {
          if (!t.eosError)
            condIfIsEofHeader(io, false)

          if (!t.consume) {
            if (t.eosError) {
              blockScopeHeader
            }
            pushPos(io)
          }
          attrPrimitiveWrite(io, Ast.expr.IntNum(term), Int1Type(false), None, None)
          if (!t.consume) {
            popPos(io)
            if (t.eosError) {
              blockScopeFooter
            }
          }
          if (!t.eosError)
            condIfIsEofFooter
        }
    }
  }

  def attrBytesLimitWrite2(
    io: String,
    expr: Ast.expr,
    bt: BytesType,
    sizeExpr: String,
    padRight: Option[Int],
    terminator: Option[Seq[Byte]],
    include: Boolean,
    exprTypeOpt: Option[DataType]
  ): Unit = {
    val term =
      terminator.map { (terminator) =>
        if (terminator.length == 1) {
          terminator.head & 0xff
        } else {
          throw new NotImplementedError("multibyte terminators cannot be serialized yet")
        }
      }
    val (termArg, padRightArg) = (term, padRight, include) match {
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
    val expr = Identifier.itemExpr(id, rep)

    {
      val itemUserType =
        if (exprTypeOpt.map(exprType => !exprType.isInstanceOf[UserType]).getOrElse(false))
          Ast.expr.CastToType(expr, Ast.typeId(true, t.classSpec.get.name))
        else
          expr
      // check non-`io` params
      attrUserTypeCheck(id, itemUserType, t, checksShouldDependOnIo)
      // set `io` params
      (t.classSpec.get.params, t.args).zipped.foreach { (paramDef, argExpr) =>
        val paramItemType = getArrayItemType(paramDef.dataType)
        val paramBasedOnIo = (paramItemType == KaitaiStreamType || paramItemType == OwnedKaitaiStreamType)
        if (paramBasedOnIo)
          attrSetProperty(itemUserType, paramDef.id, expression(argExpr))
      }
    }

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
            translator.translate(Identifier.itemExpr(OuterSizeIdentifier(id), rep))
        }

        /** @note Must be kept in sync with [[ExtraAttrs.writeNeedsInnerSize]] */
        val innerSize = if (writeNeedsInnerSize(utb.bytes)) {
          translator.translate(Identifier.itemExpr(InnerSizeIdentifier(id), rep))
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
                if (!t.include && t.consume) {
                  if (!t.eosError)
                    condIfIsEofHeader(io, false)
                  // terminator can only be 1 byte long at the moment
                  seekRelative(io, expression(Ast.expr.IntNum(1)))
                  if (!t.eosError)
                    condIfIsEofFooter
                }
              case _ => // do nothing
            }

            byteType.process.foreach { (process) =>
              attrUnprocessPrepareBeforeSubIOHandler(process, rawId)
            }

            {
              val parentIO = subIOWriteBackHeader(ioFixed, rep, byteType.process)
              handleAssignment(rawId, exprStreamToByteArray(ioFixed), rep, true)
              attrBytesTypeWrite(rawId, byteType, parentIO, rep, isRaw, None, exprTypeOpt)
              subIOWriteBackFooter(ioFixed)
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
    checksShouldDependOnIoOrig: Option[Boolean],
    assignType: DataType
  ): Unit = {
    val checksShouldDependOnIo =
      if (userExprDependsOnIo(on)) {
        None
      } else {
        checksShouldDependOnIoOrig
      }

    switchCases[DataType](id, on, cases,
      (dataType) => {
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
  def exprStreamToByteArray(ioFixed: String): String

  def attrUnprocess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec, dt: BytesType, exprTypeOpt: Option[DataType]): Unit
  def attrUnprocessPrepareBeforeSubIOHandler(proc: ProcessExpr, varSrc: Identifier): Unit

  def condIfIsEofHeader(io: String, wantedIsEof: Boolean): Unit
  def condIfIsEofFooter: Unit

  def attrSetProperty(base: Ast.expr, propName: Identifier, value: String): Unit
}
