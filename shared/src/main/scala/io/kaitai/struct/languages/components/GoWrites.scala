package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype.{DataType, FixedEndian}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{Identifier, InnerSizeIdentifier, OuterSizeIdentifier, ProcessExpr, RawIdentifier, RepeatSpec}

trait GoWrites extends LanguageCompiler with CommonWrites with GoReads {
  override def attrSwitchTypeWrite(id: Identifier, on: Ast.expr, cases: Map[Ast.expr, DataType], io: String, rep: RepeatSpec, defEndian: Option[FixedEndian], checksShouldDependOnIoOrig: Option[Boolean], assignType: DataType): Unit = {
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

  def attrUserTypeWrite(id: Identifier, t: UserType, io: String, rep: RepeatSpec, isRaw: Boolean, defEndian: Option[FixedEndian], checksShouldDependOnIo: Option[Boolean], exprTypeOpt: Option[DataType] = None): Unit = {
    val exprType = exprTypeOpt.getOrElse(t)
    val expr = itemExpr(id, rep)

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
            translator.translate(itemExpr(OuterSizeIdentifier(id), rep))
        }

        /** @note Must be kept in sync with [[ExtraAttrs.writeNeedsInnerSize]] */
        val innerSize = if (writeNeedsInnerSize(utb.bytes)) {
          translator.translate(itemExpr(InnerSizeIdentifier(id), rep))
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
              val parentIO = subIOWriteBackHeader(ioFixed, byteType.process)
              handleAssignment(rawId, exprStreamToByteArray(ioFixed), rep, true)
              attrBytesTypeWrite(rawId, byteType, parentIO, rep, isRaw, None, exprTypeOpt)
              subIOWriteBackFooter(ioFixed)
            }

            attrUserTypeInstreamWrite(ioFixed, expr, t, exprType)
        }
    }
  }

  def attrUnprocessPrepareBeforeSubIOHandler(proc: ProcessExpr, varSrc: Identifier): Unit

  def attrUserTypeInstreamWrite(io: String, expr: Ast.expr, t: DataType, exprType: DataType): Unit

  def exprStreamToByteArray(ioFixed: String): String
}
