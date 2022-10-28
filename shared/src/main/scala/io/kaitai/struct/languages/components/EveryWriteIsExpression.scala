package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype.FixedEndian
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._

import scala.collection.mutable.ListBuffer
import io.kaitai.struct.datatype._

trait EveryWriteIsExpression extends LanguageCompiler with ObjectOrientedLanguage with EveryReadIsExpression {
  override def attrWrite(attr: AttrLikeSpec, id: Identifier, defEndian: Option[Endianness]): Unit = {
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
          () => attrWrite0(id, attr, io, Some(LittleEndian)),
          () => attrWrite0(id, attr, io, Some(BigEndian))
        )
      case None =>
        attrWrite0(id, attr, io, None)
      case Some(fe: FixedEndian) =>
        attrWrite0(id, attr, io, Some(fe))
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

  def attrWrite0(id: Identifier, attr: AttrLikeSpec, io: String, defEndian: Option[FixedEndian]): Unit = {
    attr.cond.repeat match {
      case RepeatEos =>
        // we could use condRepeatEosHeader instead (we only deal with fixed-size streams when
        // writing), but don't have to (there is no difference, because the `repeat: eos` repetition
        // doesn't involve user expressions, unlike `repeat: expr` and `repeat: until`)
        condRepeatCommonHeader(id, io, attr.dataType)
      case RepeatExpr(repeatExpr: Ast.expr) =>
        condRepeatExprHeader(id, io, attr.dataType, repeatExpr)
      case RepeatUntil(untilExpr: Ast.expr) =>
        condRepeatUntilHeader(id, io, attr.dataType, untilExpr)
        val expr = writeExprAsExpr(id, attr.cond.repeat, false)
        handleAssignmentRepeatUntilIterator(translator.translate(expr))
      case NoRepeat =>
    }
    attrWrite2(id, attr.dataType, io, attr.cond.repeat, false, defEndian)
    attr.cond.repeat match {
      case RepeatEos =>
        condRepeatCommonFooter
      case _: RepeatExpr =>
        condRepeatExprFooter
      case RepeatUntil(untilExpr: Ast.expr) =>
        condRepeatUntilFooter(id, io, attr.dataType, untilExpr)
      case NoRepeat =>
    }
  }

  def attrWrite2(
    id: Identifier,
    dataType: DataType,
    io: String,
    rep: RepeatSpec,
    isRaw: Boolean,
    defEndian: Option[FixedEndian],
    exprTypeOpt: Option[DataType] = None
  ): Unit = {
    dataType match {
      case t: UserType =>
        attrUserTypeWrite(id, t, io, rep, isRaw, defEndian, exprTypeOpt)
      case t: BytesType =>
        attrBytesTypeWrite(id, t, io, rep, isRaw, exprTypeOpt)
      case st: SwitchType =>
        val isNullable = if (switchBytesOnlyAsRaw) {
          st.isNullableSwitchRaw
        } else {
          st.isNullable
        }

        attrSwitchTypeWrite(id, st.on, st.cases, io, rep, defEndian, isNullable, st.combinedType)
      case t: StrFromBytesType =>
        attrStrTypeWrite(id, t, io, rep, isRaw, exprTypeOpt)
      case t: EnumType =>
        val expr = writeExprAsExpr(id, rep, isRaw)
        val exprType = internalEnumIntType(t.basedOn)
        attrPrimitiveWrite(io, Ast.expr.Attribute(expr, Ast.identifier("to_i")), t.basedOn, defEndian, Some(exprType))
      case _ =>
        val expr = writeExprAsExpr(id, rep, isRaw)
        attrPrimitiveWrite(io, expr, dataType, defEndian, exprTypeOpt)
    }
  }

  def writeExprAsExpr(id: Identifier, rep: RepeatSpec, isRaw: Boolean): Ast.expr = {
    val astId = Ast.expr.InternalName(id)
    rep match {
      case NoRepeat =>
        astId
      case _ =>
        Ast.expr.Subscript(
          astId,
          Ast.expr.Name(Ast.identifier(Identifier.INDEX))
        )
    }
  }

  def attrBytesTypeWrite(id: Identifier, t: BytesType, io: String, rep: RepeatSpec, isRaw: Boolean, exprTypeOpt: Option[DataType]): Unit = {
    val idToWrite = t.process match {
      case Some(proc) =>
        val rawId = RawIdentifier(id)
        attrUnprocess(proc, id, rawId, rep, t, exprTypeOpt)
        rawId
      case None =>
        id
    }
    val expr = writeExprAsExpr(idToWrite, rep, isRaw)
    attrBytesTypeWrite2(io, expr, t, exprTypeOpt)
  }

  def attrStrTypeWrite(id: Identifier, t: StrFromBytesType, io: String, rep: RepeatSpec, isRaw: Boolean, exprTypeOpt: Option[DataType]): Unit = {
    val expr = exprStrToBytes(writeExprAsExpr(id, rep, isRaw), t.encoding)
    attrBytesTypeWrite2(io, expr, t.bytes, exprTypeOpt)
  }

  def attrBytesTypeWrite2(io: String, expr: Ast.expr, t: BytesType, exprTypeOpt: Option[DataType]): Unit =
    t match {
      case t: BytesEosType =>
        attrPrimitiveWrite(io, expr, t, None, exprTypeOpt)
        t.terminator.foreach { (term) =>
          // FIXME: does not take `eos-error: false` into account (assumes `eos-error: true`)
          if (!t.include)
            attrPrimitiveWrite(io, Ast.expr.IntNum(term), Int1Type(false), None, None)
        }
      case blt: BytesLimitType =>
        attrBytesLimitWrite2(io, expr, blt, exprTypeOpt)
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

  def attrBytesLimitWrite2(io: String, expr: Ast.expr, blt: BytesLimitType, exprTypeOpt: Option[DataType]): Unit = {
    val (term, padRight) = (blt.terminator, blt.padRight, blt.include) match {
      case (None, None, false) =>
        // no terminator, no padding => just a regular output
        // validation should check that expression's length matches size
        attrPrimitiveWrite(io, expr, blt, None, exprTypeOpt)
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
    attrBytesLimitWrite(io, expr, translator.translate(blt.size), term, padRight)
  }

  def attrUserTypeWrite(
    id: Identifier,
    t: UserType,
    io: String,
    rep: RepeatSpec,
    isRaw: Boolean,
    defEndian: Option[FixedEndian],
    exprTypeOpt: Option[DataType] = None
  ) = {
    val exprType = exprTypeOpt.getOrElse(t)
    val expr = writeExprAsExpr(id, rep, isRaw)

    t match {
      case _: UserTypeInstream =>
        attrUserTypeInstreamWrite(io, expr, t, exprType)
      case knownSizeType: UserTypeFromBytes =>
        val rawId = RawIdentifier(id)
        val byteType = knownSizeType.bytes
        byteType.process match {
          case None =>
            byteType match {
              case blt: BytesLimitType =>
                this match {
                  //      case thisStore: AllocateAndStoreIO =>
                  //        val ourIO = thisStore.allocateIO(rawId, rep)
                  //        Utils.addUniqueAttr(extraAttrs, AttrSpec(List(), ourIO, KaitaiStreamType))
                  //        privateMemberName(ourIO)
                  case thisLocal: AllocateIOLocalVar =>
                    val ioFixed = thisLocal.allocateIOFixed(rawId, translator.translate(blt.size))
                    attrUserTypeInstreamWrite(ioFixed, expr, t, exprType)
                    attrWriteStreamToStream(ioFixed, io)
                }
              case _ =>
                attrUserTypeInstreamWrite(io, expr, t, exprType)
            }
          case Some(process) =>
            byteType match {
              case blt: BytesLimitType =>
                this match {
                  case thisLocal: AllocateIOLocalVar =>
                    val ioFixed = thisLocal.allocateIOFixed(rawId, translator.translate(blt.size))
                    attrUserTypeInstreamWrite(ioFixed, expr, t, exprType)
                    handleAssignment(rawId, exprStreamToByteArray(ioFixed), rep, isRaw)
                    attrBytesTypeWrite(rawId, byteType, io, rep, isRaw, exprTypeOpt)
                }
            }
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
    isNullable: Boolean,
    assignType: DataType
  ): Unit = {
    if (isNullable)
      condIfSetNull(id)

    switchCases[DataType](id, on, cases,
      (dataType) => {
        if (isNullable)
          condIfSetNonNull(id)
        attrWrite2(id, dataType, io, rep, false, defEndian, Some(assignType))
      },
      (dataType) => if (switchBytesOnlyAsRaw) {
        dataType match {
          case t: BytesType =>
            attrWrite2(RawIdentifier(id), dataType, io, rep, false, defEndian, Some(assignType))
          case _ =>
            attrWrite2(id, dataType, io, rep, false, defEndian, Some(assignType))
        }
      } else {
        attrWrite2(id, dataType, io, rep, false, defEndian, Some(assignType))
      }
    )
  }

  def exprStrToBytes(name: Ast.expr, encoding: String) =
    Ast.expr.Call(
      Ast.expr.Attribute(
        name,
        Ast.identifier("to_b")
      ),
      Seq(Ast.expr.Str(encoding))
    )

  def internalEnumIntType(basedOn: IntType): DataType

  def handleAssignmentRepeatUntilIterator(expr: String): Unit

  def attrPrimitiveWrite(io: String, expr: Ast.expr, dt: DataType, defEndian: Option[FixedEndian], exprTypeOpt: Option[DataType]): Unit
  def attrBytesLimitWrite(io: String, expr: Ast.expr, size: String, term: Int, padRight: Int): Unit
  def attrUserTypeInstreamWrite(io: String, expr: Ast.expr, t: DataType, exprType: DataType): Unit
  def attrWriteStreamToStream(srcIo: String, dstIo: String): Unit
  def exprStreamToByteArray(ioFixed: String): String

  def attrUnprocess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec, dt: BytesType, exprTypeOpt: Option[DataType]): Unit
}
