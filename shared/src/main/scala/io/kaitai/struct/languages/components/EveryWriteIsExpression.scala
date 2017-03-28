package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._

import scala.collection.mutable.ListBuffer

trait EveryWriteIsExpression extends LanguageCompiler with ObjectOrientedLanguage with EveryReadIsExpression {
  override def attrWrite(attr: AttrLikeSpec, id: Identifier, extraAttrs: ListBuffer[AttrSpec]): Unit = {
    attrParseIfHeader(id, attr.cond.ifExpr)

    val io = normalIO

    attr.cond.repeat match {
      case RepeatEos =>
        condRepeatEosHeader(id, io, attr.dataType, needRaww(attr.dataType))
        attrWrite2(id, attr.dataType, io, extraAttrs, attr.cond.repeat, false)
        condRepeatEosFooter
      case RepeatExpr(repeatExpr: Ast.expr) =>
        condRepeatExprHeader2(id, io, attr.dataType, needRaww(attr.dataType), repeatExpr)
        attrWrite2(id, attr.dataType, io, extraAttrs, attr.cond.repeat, false)
        condRepeatExprFooter
      case RepeatUntil(untilExpr: Ast.expr) =>
        condRepeatUntilHeader(id, io, attr.dataType, needRaww(attr.dataType), untilExpr)
        attrWrite2(id, attr.dataType, io, extraAttrs, attr.cond.repeat, false)
        condRepeatUntilFooter(id, io, attr.dataType, needRaww(attr.dataType), untilExpr)
      case NoRepeat =>
        attrWrite2(id, attr.dataType, io, extraAttrs, attr.cond.repeat, false)
    }

    attrParseIfFooter(attr.cond.ifExpr)
  }

  def attrWrite2(
                  id: Identifier,
                  dataType: DataType,
                  io: String,
                  extraAttrs: ListBuffer[AttrSpec],
                  rep: RepeatSpec,
                  isRaw: Boolean
                ): Unit = {
    dataType match {
      case t: UserType =>
        attrUserTypeWrite(id, t, io, extraAttrs, rep, isRaw)
      case t: BytesType =>
        attrBytesTypeWrite(id, t, io, extraAttrs, rep, isRaw)
//      case SwitchType(on, cases) =>
//        attrSwitchTypeWrite(id, on, cases, io, extraAttrs, rep)
      case t: StrFromBytesType =>
        attrStrTypeWrite(id, t, io, extraAttrs, rep, isRaw)
      case t: EnumType =>
        val expr = translator.enumToInt(Ast.expr.Name(Ast.identifier(idToStr(id))), t)
        attrPrimitiveWrite(io, expr, t.basedOn)
      case _ =>
        val expr = writeExpr(id, rep, isRaw)
        attrPrimitiveWrite(io, expr, dataType)
    }
  }

  def writeExpr(id: Identifier, rep: RepeatSpec, isRaw: Boolean): String = {
    rep match {
      case _: RepeatExpr =>
        translator.translate(
          Ast.expr.Subscript(
            Ast.expr.Name(Ast.identifier(idToStr(id))),
            Ast.expr.Name(Ast.identifier(Identifier.ITERATOR_I))
          )
        )
      case NoRepeat =>
        privateMemberName(id)
    }
  }

  def attrBytesTypeWrite(id: Identifier, t: BytesType, io: String, extraAttrs: ListBuffer[AttrSpec], rep: RepeatSpec, isRaw: Boolean) = {
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
        attrPrimitiveWrite(io, translator.doByteArrayLiteral(contents), t)
      case t: BytesEosType =>
        val expr = writeExpr(idToWrite, rep, isRaw)
        attrPrimitiveWrite(io, expr, t)
        if (t.terminator.isDefined && !t.include)
          attrPrimitiveWrite(io, t.terminator.toString, Int1Type(false))
      case blt: BytesLimitType =>
        val expr = writeExpr(idToWrite, rep, isRaw)
        attrBytesLimitWrite2(io, expr, blt)
      case t: BytesTerminatedType =>
        val expr = writeExpr(idToWrite, rep, isRaw)
        attrPrimitiveWrite(io, expr, t)
        if (t.consume && !t.include)
          attrPrimitiveWrite(io, t.terminator.toString, Int1Type(false))
    }
  }

  def attrStrTypeWrite(id: Identifier, t: StrFromBytesType, io: String, extraAttrs: ListBuffer[AttrSpec], rep: RepeatSpec, isRaw: Boolean) = {
    val expr = translator.strToBytes(writeExpr(id, rep, isRaw), Ast.expr.Str(t.encoding))
    attrPrimitiveWrite(io, expr, t.bytes)

    t.bytes match {
      case t: BytesEosType =>
        if (t.terminator.isDefined && !t.include)
          attrPrimitiveWrite(io, t.terminator.toString, Int1Type(false))
      case t: BytesLimitType =>
        // FIXME: implement padding and terminator byte
        t.terminator.foreach((terminator) =>
          if (!t.include)
            attrPrimitiveWrite(io, terminator.toString, Int1Type(false))
        )
      case t: BytesTerminatedType =>
        if (t.consume && !t.include)
          attrPrimitiveWrite(io, t.terminator.toString, Int1Type(false))
    }
  }

  def attrBytesLimitWrite2(io: String, expr: String, blt: BytesLimitType): Unit = {
    val (term, padRight) = (blt.terminator, blt.padRight, blt.include) match {
      case (None, None, false) =>
        // no terminator, no padding => just a regular output
        // validation should check that expression's length matches size
        attrPrimitiveWrite(io, expr, blt)
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
    extraAttrs: ListBuffer[AttrSpec],
    rep: RepeatSpec,
    isRaw: Boolean
  ) = {
    val expr = writeExpr(id, rep, isRaw)

    t match {
      case _: UserTypeInstream =>
        attrUserTypeInstreamWrite(io, expr)
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
                    attrUserTypeInstreamWrite(ioFixed, expr)
                    attrWriteStreamToStream(ioFixed, io)
                }
              case _ =>
                attrUserTypeInstreamWrite(io, expr)
            }
          case Some(process) =>
            byteType match {
              case blt: BytesLimitType =>
                this match {
                  case thisLocal: AllocateIOLocalVar =>
                    val ioFixed = thisLocal.allocateIOFixed(rawId, translator.translate(blt.size))
                    attrUserTypeInstreamWrite(ioFixed, expr)
                    handleAssignment(rawId, exprStreamToByteArray(ioFixed), rep, isRaw)
                    attrBytesTypeWrite(rawId, byteType, io, extraAttrs, rep, isRaw)
                }
            }
        }
    }
  }

  def attrPrimitiveWrite(io: String, expr: String, dt: DataType): Unit
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

  def condRepeatExprHeader2(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: Ast.expr): Unit = ???
}
