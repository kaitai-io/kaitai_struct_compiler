package io.kaitai.struct.languages.components

import io.kaitai.struct.Utils
import io.kaitai.struct.datatype.{BigEndian, DataType, FixedEndian}
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.translators.{GoTranslator, TranslatorResult}

import scala.collection.mutable.ListBuffer

trait GoReads extends CommonReads with ObjectOrientedLanguage with SwitchOps {
  val translator: GoTranslator

  override def attrParse2(
    id: Identifier,
    dataType: DataType,
    io: String,
    extraAttrs: ListBuffer[AttrSpec],
    rep: RepeatSpec,
    isRaw: Boolean,
    defEndian: Option[FixedEndian],
    assignType: Option[DataType] = None
  ): Unit = {
    dataType match {
      case FixedBytesType(c, _) =>
        attrFixedContentsParse(id, c)
      case t: UserType =>
        attrUserTypeParse(id, t, io, extraAttrs, rep, defEndian)
//      case t: BytesType =>
//        attrBytesTypeParse(id, t, io, extraAttrs, rep, isRaw)
//      case SwitchType(on, cases) =>
//        attrSwitchTypeParse(id, on, cases, io, extraAttrs, rep)
      case t: StrFromBytesType =>
        val r1 = translator.outVarCheckRes(parseExprBytes(t.bytes, io))
        val expr = translator.bytesToStr(translator.resToStr(r1), Ast.expr.Str(t.encoding))
        handleAssignment(id, expr, rep, isRaw)
//      case t: EnumType =>
//        val expr = translator.doEnumById(t.enumSpec.get.name, parseExpr(t.basedOn, io))
//        handleAssignment(id, expr, rep, isRaw)
      case _ =>
        val expr = parseExpr(dataType, io, defEndian)
        val r = translator.outVarCheckRes(expr)
        handleAssignment(id, r, rep, isRaw)
    }
  }

  def parseExprBytes(dataType: BytesType, io: String): String = {
    val expr = parseExpr(dataType, io, None) // FIXME
/*
    // apply pad stripping and termination
    dataType match {
      case BytesEosType(terminator, include, padRight, _) =>
        bytesPadTermExpr(expr, padRight, terminator, include)
      case BytesLimitType(_, terminator, include, padRight, _) =>
        bytesPadTermExpr(expr, padRight, terminator, include)
      case _ =>
        expr
    }*/
    expr
  }

  def attrUserTypeParse(id: Identifier, dataType: UserType, io: String, extraAttrs: ListBuffer[AttrSpec], rep: RepeatSpec, defEndian: Option[FixedEndian]): Unit = {
    val newIO = dataType match {
      case knownSizeType: UserTypeFromBytes =>
        // we have a fixed buffer, thus we shall create separate IO for it
        val rawId = RawIdentifier(id)
        val byteType = knownSizeType.bytes

        attrParse2(rawId, byteType, io, extraAttrs, rep, true, defEndian)

        val extraType = rep match {
          case NoRepeat => byteType
          case _ => ArrayType(byteType)
        }

        Utils.addUniqueAttr(extraAttrs, AttrSpec(List(), rawId, extraType))

        this match {
          case thisStore: AllocateAndStoreIO =>
            thisStore.allocateIO(rawId, rep, extraAttrs)
          case thisLocal: AllocateIOLocalVar =>
            thisLocal.allocateIO(rawId, rep)
        }
      case _: UserTypeInstream =>
        // no fixed buffer, just use regular IO
        io
    }

    val expr = translator.userType(dataType, newIO)
    handleAssignment(id, expr, rep, false)
  }

  def handleAssignment(id: Identifier, expr: TranslatorResult, rep: RepeatSpec, isRaw: Boolean): Unit = {
    rep match {
      case RepeatEos => handleAssignmentRepeatEos(id, expr)
      case RepeatExpr(_) => handleAssignmentRepeatExpr(id, expr)
      case RepeatUntil(_) => handleAssignmentRepeatUntil(id, expr, isRaw)
      case NoRepeat => handleAssignmentSimple(id, expr)
    }
  }

  def handleAssignmentRepeatEos(id: Identifier, expr: TranslatorResult): Unit
  def handleAssignmentRepeatExpr(id: Identifier, expr: TranslatorResult): Unit
  def handleAssignmentRepeatUntil(id: Identifier, expr: TranslatorResult, isRaw: Boolean): Unit
  def handleAssignmentSimple(id: Identifier, expr: TranslatorResult): Unit

  def parseExpr(dataType: DataType, io: String, defEndian: Option[FixedEndian]): String
}
