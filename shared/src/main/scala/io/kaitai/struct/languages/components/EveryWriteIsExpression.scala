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
        condRepeatEosHeader2(id, io, attr.dataType, needRaww(attr.dataType))
        attrWrite2(id, attr.dataType, io, defEndian, attr.cond.repeat, false)
        condRepeatEosFooter
      case RepeatExpr(repeatExpr: Ast.expr) =>
        condRepeatExprHeader2(id, io, attr.dataType, needRaww(attr.dataType), repeatExpr)
        attrWrite2(id, attr.dataType, io, defEndian, attr.cond.repeat, false)
        condRepeatExprFooter
      case RepeatUntil(untilExpr: Ast.expr) =>
        condRepeatUntilHeader(id, io, attr.dataType, needRaww(attr.dataType), untilExpr)
        attrWrite2(id, attr.dataType, io, defEndian, attr.cond.repeat, false)
        condRepeatUntilFooter(id, io, attr.dataType, needRaww(attr.dataType), untilExpr)
      case NoRepeat =>
        attrWrite2(id, attr.dataType, io, defEndian, attr.cond.repeat, false)
    }

    attrParseIfFooter(attr.cond.ifExpr)
  }

  def attrWrite2(
    id: Identifier,
    dataType: DataType,
    io: String,
    defEndian: Option[FixedEndian],
    rep: RepeatSpec,
    isRaw: Boolean
  ): Unit = {
    dataType match {
      case t: UserType =>
        attrUserTypeWrite(id, t, io, rep, isRaw, defEndian)
      case t: BytesType =>
        attrBytesTypeWrite(id, t, io, rep, isRaw)
      case SwitchType(on, cases, _) =>
        attrSwitchTypeWrite(id, on, cases, io, rep, defEndian)
      case t: StrFromBytesType =>
        attrStrTypeWrite(id, t, io, rep, isRaw)
      case t: EnumType =>
        val expr = translator.enumToInt(Ast.expr.Name(Ast.identifier(idToStr(id))), t)
        attrPrimitiveWrite(io, expr, t.basedOn, defEndian)
      case _ =>
        val expr = writeExpr(id, rep, isRaw)
        attrPrimitiveWrite(io, translator.translate(expr), dataType, defEndian)
    }
  }

  def writeExpr(id: Identifier, rep: RepeatSpec, isRaw: Boolean): Ast.expr = {
    val astId = Ast.expr.Name(Ast.identifier(idToStr(id)))
    rep match {
      case _: RepeatExpr | RepeatEos =>
        Ast.expr.Subscript(
          astId,
          Ast.expr.Name(Ast.identifier(Identifier.INDEX))
        )
      case NoRepeat =>
        astId
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
        val expr = writeExpr(idToWrite, rep, isRaw)
        attrPrimitiveWrite(io, translator.translate(expr), t, None)
        if (t.terminator.isDefined && !t.include)
          attrPrimitiveWrite(io, t.terminator.toString, Int1Type(false), None)
      case blt: BytesLimitType =>
        val expr = writeExpr(idToWrite, rep, isRaw)
        attrBytesLimitWrite2(io, translator.translate(expr), blt)
      case t: BytesTerminatedType =>
        val expr = writeExpr(idToWrite, rep, isRaw)
        attrPrimitiveWrite(io, translator.translate(expr), t, None)
        if (t.consume && !t.include)
          attrPrimitiveWrite(io, t.terminator.toString, Int1Type(false), None)
    }
  }

  def attrStrTypeWrite(id: Identifier, t: StrFromBytesType, io: String, rep: RepeatSpec, isRaw: Boolean) = {
    val expr = translator.strToBytes(writeExpr(id, rep, isRaw), Ast.expr.Str(t.encoding))
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
    defEndian: Option[FixedEndian]
  ) = {
    val expr = writeExpr(id, rep, isRaw)

    t match {
      case _: UserTypeInstream =>
        attrUserTypeInstreamWrite(io, translator.translate(expr))
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
                    attrUserTypeInstreamWrite(ioFixed, translator.translate(expr))
                    attrWriteStreamToStream(ioFixed, io)
                }
              case _ =>
                attrUserTypeInstreamWrite(io, translator.translate(expr))
            }
          case Some(process) =>
            byteType match {
              case blt: BytesLimitType =>
                this match {
                  case thisLocal: AllocateIOLocalVar =>
                    val ioFixed = thisLocal.allocateIOFixed(rawId, translator.translate(blt.size))
                    attrUserTypeInstreamWrite(ioFixed, translator.translate(expr))
                    handleAssignment(rawId, exprStreamToByteArray(ioFixed), rep, isRaw)
                    attrBytesTypeWrite(rawId, byteType, io, rep, isRaw)
                }
            }
        }
    }
  }

  def attrSwitchTypeWrite(id: Identifier, on: expr, cases: Map[expr, DataType], io: String, rep: RepeatSpec, defEndian: Option[FixedEndian]) = {
    switchStart(id, on)

    // Pass 1: only normal case clauses
    var first = true

    cases.foreach { case (condition, dataType) =>
      condition match {
        case SwitchType.ELSE_CONST =>
        // skip for now
        case _ =>
          if (first) {
            switchCaseFirstStart(condition)
            first = false
          } else {
            switchCaseStart(condition)
          }
          attrWrite2(id, dataType, io, defEndian, rep, false)
          switchCaseEnd()
      }
    }

    // Pass 2: else clause, if it is there
    cases.foreach { case (condition, dataType) =>
      condition match {
        case SwitchType.ELSE_CONST =>
          switchElseStart()
          if (switchBytesOnlyAsRaw) {
            dataType match {
              case t: BytesType =>
                attrWrite2(RawIdentifier(id), dataType, io, defEndian, rep, false)
              case _ =>
                attrWrite2(id, dataType, io, defEndian, rep, false)
            }
          } else {
            attrWrite2(id, dataType, io, defEndian, rep, false)
          }
          switchElseEnd()
        case _ =>
        // ignore normal case clauses
      }
    }

    switchEnd()
  }

  def attrPrimitiveWrite(io: String, expr: String, dt: DataType, defEndian: Option[FixedEndian]): Unit
  def attrBytesLimitWrite(io: String, expr: String, size: String, term: Int, padRight: Int): Unit
  def attrUserTypeInstreamWrite(io: String, expr: String): Unit
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

  def condRepeatEosHeader2(id: Identifier, io: String, dataType: DataType, needRaw: Boolean): Unit
  def condRepeatExprHeader2(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: Ast.expr): Unit
}
