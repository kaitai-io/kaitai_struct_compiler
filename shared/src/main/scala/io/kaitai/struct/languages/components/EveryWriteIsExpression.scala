package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype.FixedEndian
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._

import scala.collection.mutable.ListBuffer

trait EveryWriteIsExpression extends LanguageCompiler with ObjectOrientedLanguage with EveryReadIsExpression {
  override def attrWrite(attr: AttrLikeSpec, id: Identifier, defEndian: Option[FixedEndian]): Unit = {
    attrParseIfHeader(id, attr.cond.ifExpr)

    val io = normalIO

    attr.cond.repeat match {
      case RepeatEos =>
        condRepeatCommonHeader(id, io, attr.dataType)
        attrWrite2(id, attr.dataType, io, attr.cond.repeat, false, defEndian)
        condRepeatCommonFooter
      case RepeatExpr(repeatExpr: Ast.expr) =>
        condRepeatCommonHeader(id, io, attr.dataType)
        attrWrite2(id, attr.dataType, io, attr.cond.repeat, false, defEndian)
        condRepeatCommonFooter
      case RepeatUntil(untilExpr: Ast.expr) =>
        condRepeatCommonHeader(id, io, attr.dataType)
        attrWrite2(id, attr.dataType, io, attr.cond.repeat, false, defEndian)
        condRepeatCommonFooter
      case NoRepeat =>
        attrWrite2(id, attr.dataType, io, attr.cond.repeat, false, defEndian)
    }

    attrParseIfFooter(attr.cond.ifExpr)
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
        attrBytesTypeWrite(id, t, io, rep, isRaw)
      case st: SwitchType =>
        val isNullable = if (switchBytesOnlyAsRaw) {
          st.isNullableSwitchRaw
        } else {
          st.isNullable
        }

        attrSwitchTypeWrite(id, st.on, st.cases, io, rep, defEndian, isNullable, st.combinedType)
      case t: StrFromBytesType =>
        attrStrTypeWrite(id, t, io, rep, isRaw)
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

  def attrBytesTypeWrite(id: Identifier, t: BytesType, io: String, rep: RepeatSpec, isRaw: Boolean): Unit = {
    val idToWrite = t.process match {
      case Some(proc) =>
        val rawId = RawIdentifier(id)
        attrUnprocess(proc, id, rawId, rep)
        rawId
      case None =>
        id
    }
    val expr = writeExprAsExpr(idToWrite, rep, isRaw)
    attrBytesTypeWrite2(io, expr, t)
  }

  def attrStrTypeWrite(id: Identifier, t: StrFromBytesType, io: String, rep: RepeatSpec, isRaw: Boolean): Unit = {
    val expr = exprStrToBytes(writeExprAsExpr(id, rep, isRaw), t.encoding)
    attrBytesTypeWrite2(io, expr, t.bytes)
  }

  def attrBytesTypeWrite2(io: String, expr: Ast.expr, t: BytesType): Unit =
    t match {
      case t: BytesEosType =>
        attrPrimitiveWrite(io, expr, t, None)
        t.terminator.foreach { (term) =>
          // FIXME: does not take `eos-error: false` into account (assumes `eos-error: true`)
          if (!t.include)
            attrPrimitiveWrite(io, Ast.expr.IntNum(term), Int1Type(false), None)
        }
      case blt: BytesLimitType =>
        attrBytesLimitWrite2(io, expr, blt)
      case t: BytesTerminatedType =>
        attrPrimitiveWrite(io, expr, t, None)
        if (!t.include) {
          if (!t.consume) {
            blockScopeHeader
            pushPos(io)
          }
          // FIXME: does not take `eos-error: false` into account (assumes `eos-error: true`)
          attrPrimitiveWrite(io, Ast.expr.IntNum(t.terminator), Int1Type(false), None)
          if (!t.consume) {
            popPos(io)
            blockScopeFooter
          }
        }
    }

  def attrBytesLimitWrite2(io: String, expr: Ast.expr, blt: BytesLimitType): Unit = {
    val (term, padRight) = (blt.terminator, blt.padRight, blt.include) match {
      case (None, None, false) =>
        // no terminator, no padding => just a regular output
        // validation should check that expression's length matches size
        attrPrimitiveWrite(io, expr, blt, None)
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
                    attrBytesTypeWrite(rawId, byteType, io, rep, isRaw)
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

  def attrPrimitiveWrite(io: String, expr: Ast.expr, dt: DataType, defEndian: Option[FixedEndian], exprTypeOpt: Option[DataType] = None): Unit
  def attrBytesLimitWrite(io: String, expr: Ast.expr, size: String, term: Int, padRight: Int): Unit
  def attrUserTypeInstreamWrite(io: String, expr: Ast.expr, t: DataType, exprType: DataType): Unit
  def attrWriteStreamToStream(srcIo: String, dstIo: String): Unit
  def exprStreamToByteArray(ioFixed: String): String

  def attrUnprocess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec): Unit
}
