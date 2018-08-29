package io.kaitai.struct.languages.components

import io.kaitai.struct.Utils
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype.{DataType, FixedEndian}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.translators.BaseTranslator

import scala.collection.mutable.ListBuffer

/**
  * Helper trait for languages where single parsing of every standard or user data type is done as expression, i.e. an
  * rvalue. In these languages, "attrStdTypeParse" is replaced with higher-level API: "stdTypeParseExpr" and
  * "handleAssignment".
  */
trait EveryReadIsExpression
  extends LanguageCompiler
  with ObjectOrientedLanguage
  with CommonReads
  with SwitchOps {
  val translator: BaseTranslator

  override def attrParse2(
    id: Identifier,
    dataType: DataType,
    io: String,
    extraAttrs: ListBuffer[AttrSpec],
    rep: RepeatSpec,
    isRaw: Boolean,
    defEndian: Option[FixedEndian],
    assignTypeOpt: Option[DataType] = None
  ): Unit = {
    val assignType = assignTypeOpt.getOrElse(dataType)

    if (debug && rep != NoRepeat)
      attrDebugStart(id, dataType, Some(io), rep)

    dataType match {
      case FixedBytesType(c, _) =>
        attrFixedContentsParse(id, c)
      case t: UserType =>
        attrUserTypeParse(id, t, io, extraAttrs, rep, defEndian)
      case t: BytesType =>
        attrBytesTypeParse(id, t, io, extraAttrs, rep, isRaw)
      case st: SwitchType =>
        val isNullable = if (switchBytesOnlyAsRaw) {
          st.isNullableSwitchRaw
        } else {
          st.isNullable
        }

        attrSwitchTypeParse(id, st.on, st.cases, io, extraAttrs, rep, defEndian, isNullable, st.combinedType)
      case t: StrFromBytesType =>
        val expr = translator.bytesToStr(parseExprBytes(t.bytes, io), Ast.expr.Str(t.encoding))
        handleAssignment(id, expr, rep, isRaw)
      case t: EnumType =>
        val expr = translator.doEnumById(t.enumSpec.get.name, parseExpr(t.basedOn, t.basedOn, io, defEndian))
        handleAssignment(id, expr, rep, isRaw)
      case _ =>
        val expr = parseExpr(dataType, assignType, io, defEndian)
        handleAssignment(id, expr, rep, isRaw)
    }

    if (debug && rep != NoRepeat)
      attrDebugEnd(id, dataType, io, rep)
  }

  def attrBytesTypeParse(
    id: Identifier,
    dataType: BytesType,
    io: String,
    extraAttrs: ListBuffer[AttrSpec],
    rep: RepeatSpec,
    isRaw: Boolean
  ): Unit = {
    // use intermediate variable name, if we'll be doing post-processing
    val rawId = dataType.process match {
      case None => id
      case Some(_) =>
        val rawId = RawIdentifier(id)
        Utils.addUniqueAttr(extraAttrs, AttrSpec(List(), rawId, dataType))
        rawId
    }

    val expr = parseExprBytes(dataType, io)
    handleAssignment(rawId, expr, rep, isRaw)

    // apply post-processing
    dataType.process.foreach((proc) => attrProcess(proc, rawId, id))
  }

  def parseExprBytes(dataType: BytesType, io: String): String = {
    val expr = parseExpr(dataType, dataType, io, None)

    // apply pad stripping and termination
    dataType match {
      case BytesEosType(terminator, include, padRight, _) =>
        bytesPadTermExpr(expr, padRight, terminator, include)
      case BytesLimitType(_, terminator, include, padRight, _) =>
        bytesPadTermExpr(expr, padRight, terminator, include)
      case _ =>
        expr
    }
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
    val expr = parseExpr(dataType, dataType, newIO, defEndian)
    if (!debug) {
      handleAssignment(id, expr, rep, false)
    } else {
      // Debug mode requires one to actually call "_read" method on constructed user type,
      // and this must be done as a separate statement - or else exception handler would
      // blast the whole structure, not only this element. This, in turn, makes us assign
      // constructed element to a temporary variable in case on repetitions
      rep match {
        case NoRepeat =>
          handleAssignmentSimple(id, expr)
          userTypeDebugRead(privateMemberName(id))
        case _ =>
          val tempVarName = localTemporaryName(id)
          handleAssignmentTempVar(dataType, tempVarName, expr)
          userTypeDebugRead(tempVarName)
          handleAssignment(id, tempVarName, rep, false)
      }
    }
  }

  def attrSwitchTypeParse(
    id: Identifier,
    on: Ast.expr,
    cases: Map[Ast.expr, DataType],
    io: String,
    extraAttrs: ListBuffer[AttrSpec],
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
        attrParse2(id, dataType, io, extraAttrs, rep, false, defEndian, Some(assignType))
      },
      (dataType) => if (switchBytesOnlyAsRaw) {
        dataType match {
          case t: BytesType =>
            val rawId = RawIdentifier(id)
            Utils.addUniqueAttr(extraAttrs, AttrSpec(List(), rawId, dataType))
            attrParse2(rawId, dataType, io, extraAttrs, rep, false, defEndian, Some(assignType))
          case _ =>
            attrParse2(id, dataType, io, extraAttrs, rep, false, defEndian, Some(assignType))
        }
      } else {
        attrParse2(id, dataType, io, extraAttrs, rep, false, defEndian, Some(assignType))
      }
    )
  }

  def handleAssignment(id: Identifier, expr: String, rep: RepeatSpec, isRaw: Boolean): Unit = {
    rep match {
      case RepeatEos => handleAssignmentRepeatEos(id, expr)
      case RepeatExpr(_) => handleAssignmentRepeatExpr(id, expr)
      case RepeatUntil(_) => handleAssignmentRepeatUntil(id, expr, isRaw)
      case NoRepeat => handleAssignmentSimple(id, expr)
    }
  }

  def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit
  def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit
  def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit
  def handleAssignmentSimple(id: Identifier, expr: String): Unit
  def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit = ???

  def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String
  def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean): String
  def userTypeDebugRead(id: String): Unit = {}

  def instanceCalculate(instName: Identifier, dataType: DataType, value: Ast.expr): Unit = {
    if (debug)
      attrDebugStart(instName, dataType, None, NoRepeat)
    handleAssignmentSimple(instName, expression(value))
  }
}
