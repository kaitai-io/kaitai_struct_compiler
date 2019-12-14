package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype.FixedEndian
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._

import scala.collection.mutable.ListBuffer

trait EveryWriteIsExpression extends LanguageCompiler with ObjectOrientedLanguage with EveryReadIsExpression {
  override def attrWrite(attr: AttrLikeSpec, id: Identifier, defEndian: Option[FixedEndian]): Unit = {
    attrParseIfHeader(id, attr.cond.ifExpr)

    val io = normalIO

    attr.cond.repeat match {
      case RepeatEos =>
        condRepeatCommonHeader(id, io, attr.dataType, needRaww(attr.dataType))
        attrWrite2(id, attr.dataType, io, attr.cond.repeat, false, defEndian)
        condRepeatCommonFooter
      case RepeatExpr(repeatExpr: Ast.expr) =>
        condRepeatCommonHeader(id, io, attr.dataType, needRaww(attr.dataType))
        attrWrite2(id, attr.dataType, io, attr.cond.repeat, false, defEndian)
        condRepeatCommonFooter
      case RepeatUntil(untilExpr: Ast.expr) =>
        condRepeatCommonHeader(id, io, attr.dataType, needRaww(attr.dataType))
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
        val expr = translator.enumToInt(Ast.expr.Name(Ast.identifier(idToStr(id))), t)
        val exprType = internalEnumIntType(t.basedOn)
        attrPrimitiveWrite(io, expr, t.basedOn, defEndian, Some(exprType))
      case _ =>
        val expr = writeExprAsString(id, rep, isRaw)
        attrPrimitiveWrite(io, expr, dataType, defEndian, exprTypeOpt)
    }
  }

  // TODO: unite these methods
  def writeExprAsString(id: Identifier, rep: RepeatSpec, isRaw: Boolean): String = {
    rep match {
      case NoRepeat =>
        privateMemberName(id)
      case _ =>
        translator.arraySubscript(
          Ast.expr.Name(Ast.identifier(idToStr(id))),
          Ast.expr.Name(Ast.identifier(Identifier.INDEX))
        )
    }
  }

  def writeExprAsExpr(id: Identifier, rep: RepeatSpec, isRaw: Boolean): Ast.expr = {
    val astId = Ast.expr.Name(Ast.identifier(idToStr(id)))
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

  def attrBytesTypeWrite(id: Identifier, t: BytesType, io: String, rep: RepeatSpec, isRaw: Boolean) = {
    val idToWrite = t.process match {
      case Some(proc) =>
        val rawId = RawIdentifier(id)
        attrUnprocess(proc, id, rawId)
        rawId
      case None =>
        id
    }
    t match {
      case FixedBytesType(contents, process) =>
        attrPrimitiveWrite(io, translator.doByteArrayLiteral(contents), t, None)
      case t: BytesEosType =>
        val expr = writeExprAsString(idToWrite, rep, isRaw)
        attrPrimitiveWrite(io, expr, t, None)
        if (t.terminator.isDefined && !t.include)
          attrPrimitiveWrite(io, t.terminator.toString, Int1Type(false), None)
      case blt: BytesLimitType =>
        val expr = writeExprAsString(idToWrite, rep, isRaw)
        attrBytesLimitWrite2(io, expr, blt)
      case t: BytesTerminatedType =>
        val expr = writeExprAsString(idToWrite, rep, isRaw)
        attrPrimitiveWrite(io, expr, t, None)
        if (t.consume && !t.include)
          attrPrimitiveWrite(io, t.terminator.toString, Int1Type(false), None)
    }
  }

  def attrStrTypeWrite(id: Identifier, t: StrFromBytesType, io: String, rep: RepeatSpec, isRaw: Boolean) = {
    val expr = translator.strToBytes(writeExprAsExpr(id, rep, isRaw), Ast.expr.Str(t.encoding))
    attrPrimitiveWrite(io, expr, t.bytes, None)

    t.bytes match {
      case t: BytesEosType =>
        if (t.terminator.isDefined && !t.include)
          attrPrimitiveWrite(io, t.terminator.toString, Int1Type(false), None)
      case t: BytesLimitType =>
        // FIXME: implement padding and terminator byte
        t.terminator.foreach((terminator) =>
          if (!t.include)
            attrPrimitiveWrite(io, terminator.toString, Int1Type(false), None)
        )
      case t: BytesTerminatedType =>
        if (t.consume && !t.include)
          attrPrimitiveWrite(io, t.terminator.toString, Int1Type(false), None)
    }
  }

  def attrBytesLimitWrite2(io: String, expr: String, blt: BytesLimitType): Unit = {
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
    val expr = writeExprAsString(id, rep, isRaw)

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
    on: expr,
    cases: Map[expr, DataType],
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

  def internalEnumIntType(basedOn: IntType): DataType

  def attrPrimitiveWrite(io: String, expr: String, dt: DataType, defEndian: Option[FixedEndian], exprTypeOpt: Option[DataType] = None): Unit
  def attrBytesLimitWrite(io: String, expr: String, size: String, term: Int, padRight: Int): Unit
  def attrUserTypeInstreamWrite(io: String, expr: String, t: DataType, exprType: DataType): Unit
  def attrWriteStreamToStream(srcIo: String, dstIo: String): Unit
  def exprStreamToByteArray(ioFixed: String): String

  def attrUnprocess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit

  // FIXME: unify with EveryReadIsExpression
  def needRaww(dataType: DataType): Boolean = {
    dataType match {
      case t: UserTypeFromBytes => true
      case _ => false
    }
  }
}
