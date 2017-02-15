package io.kaitai.struct.languages.components

import io.kaitai.struct.Utils
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format._

import scala.collection.mutable.ListBuffer

/**
  * Helper trait for languages where single parsing of every standard or user data type is done as expression, i.e. an
  * rvalue. In these languages, "attrStdTypeParse" is replaced with higher-level API: "stdTypeParseExpr" and
  * "handleAssignment".
  */
trait EveryReadIsExpression extends LanguageCompiler with ObjectOrientedLanguage {
  override def attrParse(attr: AttrLikeSpec, id: Identifier, extraAttrs: ListBuffer[AttrSpec]): Unit = {
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

    if (debug)
      attrDebugStart(id, attr.dataType, Some(io), NoRepeat)

    attr.cond.repeat match {
      case RepeatEos =>
        condRepeatEosHeader(id, io, attr.dataType, needRaw(attr.dataType))
        attrParse2(id, attr.dataType, io, extraAttrs, attr.cond.repeat)
        condRepeatEosFooter
      case RepeatExpr(repeatExpr: Ast.expr) =>
        condRepeatExprHeader(id, io, attr.dataType, needRaw(attr.dataType), repeatExpr)
        attrParse2(id, attr.dataType, io, extraAttrs, attr.cond.repeat)
        condRepeatExprFooter
      case RepeatUntil(untilExpr: Ast.expr) =>
        condRepeatUntilHeader(id, io, attr.dataType, needRaw(attr.dataType), untilExpr)
        attrParse2(id, attr.dataType, io, extraAttrs, attr.cond.repeat)
        condRepeatUntilFooter(id, io, attr.dataType, needRaw(attr.dataType), untilExpr)
      case NoRepeat =>
        attrParse2(id, attr.dataType, io, extraAttrs, attr.cond.repeat)
    }

    if (debug)
      attrDebugEnd(id, attr.dataType, io, NoRepeat)

    // More position management after parsing for ParseInstanceSpecs
    attr match {
      case pis: ParseInstanceSpec =>
        if (pis.pos.isDefined)
          popPos(io)
      case _ => // no seeking required for sequence attributes
    }

    attrParseIfFooter(attr.cond.ifExpr)
  }

  def attrParse2(id: Identifier, dataType: BaseType, io: String, extraAttrs: ListBuffer[AttrSpec], rep: RepeatSpec): Unit = {
    if (debug && rep != NoRepeat)
      attrDebugStart(id, dataType, Some(io), rep)

    dataType match {
      case FixedBytesType(c, _) =>
        attrFixedContentsParse(id, c)
      case t: UserType =>
        attrUserTypeParse(id, t, io, extraAttrs, rep)
      case t: BytesType =>
        attrBytesTypeParse(id, t, io, extraAttrs, rep)
      case SwitchType(on, cases) =>
        attrSwitchTypeParse(id, on, cases, io, extraAttrs, rep)
      case t: StrFromBytesType =>
        val expr = translator.bytesToStr(parseExprBytes(t.bytes, io), Ast.expr.Str(t.encoding))
        handleAssignment(id, expr, rep)
      case t: EnumType =>
        val expr = translator.doEnumById(t.enumSpec.get.name, parseExpr(t.basedOn, io))
        handleAssignment(id, expr, rep)
      case _ =>
        val expr = parseExpr(dataType, io)
        handleAssignment(id, expr, rep)
    }

    if (debug && rep != NoRepeat)
      attrDebugEnd(id, dataType, io, rep)
  }

  def attrBytesTypeParse(id: Identifier, dataType: BytesType, io: String, extraAttrs: ListBuffer[AttrSpec], rep: RepeatSpec): Unit = {
    // use intermediate variable name, if we'll be doing post-processing
    val rawId = dataType.process match {
      case None => id
      case Some(_) =>
        val rawId = RawIdentifier(id)
        Utils.addUniqueAttr(extraAttrs, AttrSpec(rawId, dataType))
        rawId
    }

    val expr = parseExprBytes(dataType, io)
    handleAssignment(rawId, expr, rep)

    // apply post-processing
    dataType.process.foreach((proc) => attrProcess(proc, rawId, id))
  }

  def parseExprBytes(dataType: BytesType, io: String): String = {
    val expr = parseExpr(dataType, io)

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

  def attrUserTypeParse(id: Identifier, dataType: UserType, io: String, extraAttrs: ListBuffer[AttrSpec], rep: RepeatSpec): Unit = {
    val newIO = dataType match {
      case knownSizeType: UserTypeKnownSize =>
        // we have a fixed buffer, thus we shall create separate IO for it
        val rawId = RawIdentifier(id)
        val byteType = knownSizeType match {
          case UserTypeByteLimit(_, size, process) => BytesLimitType(size, None, false, None, process)
          case UserTypeEos(_, process) => BytesEosType(None, false, None, process)
        }

        attrParse2(rawId, byteType, io, extraAttrs, rep)

        val extraType = rep match {
          case NoRepeat => byteType
          case _ => ArrayType(byteType)
        }

        Utils.addUniqueAttr(extraAttrs, AttrSpec(rawId, extraType))

        this match {
          case thisStore: AllocateAndStoreIO =>
            val ourIO = thisStore.allocateIO(rawId, rep)
            Utils.addUniqueAttr(extraAttrs, AttrSpec(ourIO, KaitaiStreamType))
            privateMemberName(ourIO)
          case thisLocal: AllocateIOLocalVar =>
            thisLocal.allocateIO(rawId, rep)
        }
      case _: UserTypeInstream =>
        // no fixed buffer, just use regular IO
        io
    }
    val expr = parseExpr(dataType, newIO)
    if (!debug) {
      handleAssignment(id, expr, rep)
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
          val tempVarName = s"_t_${idToStr(id)}"
          handleAssignmentTempVar(dataType, tempVarName, expr)
          handleAssignment(id, tempVarName, rep)
          userTypeDebugRead(tempVarName)
      }
    }
  }

  def needRaw(dataType: BaseType): Boolean = {
    dataType match {
      case t: UserTypeKnownSize => true
      case _ => false
    }
  }

  def attrSwitchTypeParse(id: Identifier, on: Ast.expr, cases: Map[Ast.expr, BaseType], io: String, extraAttrs: ListBuffer[AttrSpec], rep: RepeatSpec): Unit = {
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
          attrParse2(id, dataType, io, extraAttrs, rep)
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
                attrParse2(RawIdentifier(id), dataType, io, extraAttrs, rep)
              case _ =>
                attrParse2(id, dataType, io, extraAttrs, rep)
            }
          } else {
            attrParse2(id, dataType, io, extraAttrs, rep)
          }
          switchElseEnd()
        case _ =>
          // ignore normal case clauses
      }
    }

    switchEnd()
  }

  def handleAssignment(id: Identifier, expr: String, rep: RepeatSpec): Unit = {
    rep match {
      case RepeatEos => handleAssignmentRepeatEos(id, expr)
      case RepeatExpr(_) => handleAssignmentRepeatExpr(id, expr)
      case RepeatUntil(_) => handleAssignmentRepeatUntil(id, expr)
      case NoRepeat => handleAssignmentSimple(id, expr)
    }
  }

  def attrDebugStart(attrName: Identifier, attrType: BaseType, io: Option[String], repeat: RepeatSpec): Unit = {}
  def attrDebugEnd(attrName: Identifier, attrType: BaseType, io: String, repeat: RepeatSpec): Unit = {}

  def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit
  def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit
  def handleAssignmentRepeatUntil(id: Identifier, expr: String): Unit
  def handleAssignmentSimple(id: Identifier, expr: String): Unit
  def handleAssignmentTempVar(dataType: BaseType, id: String, expr: String): Unit = ???

  def parseExpr(dataType: BaseType, io: String): String
  def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean): String
  def userTypeDebugRead(id: String): Unit = {}

  def instanceCalculate(instName: InstanceIdentifier, dataType: BaseType, value: Ast.expr): Unit = {
    if (debug)
      attrDebugStart(instName, dataType, None, NoRepeat)
    handleAssignmentSimple(instName, expression(value))
  }

  def switchStart(id: Identifier, on: Ast.expr): Unit
  def switchCaseFirstStart(condition: Ast.expr): Unit = switchCaseStart(condition)
  def switchCaseStart(condition: Ast.expr): Unit
  def switchCaseEnd(): Unit
  def switchElseStart(): Unit
  def switchElseEnd(): Unit = switchCaseEnd()
  def switchEnd(): Unit

  /**
    * Controls parsing of typeless (BytesType) alternative in switch case. If true,
    * then target language does not support storing both bytes array and true object
    * in the same variable, so we'll use workaround: bytes array will be read as
    * _raw_ variable (which would be used anyway for all other cases as well). If
    * false (which is default), we'll store *both* true objects and bytes array in
    * the same variable.
    */
  def switchBytesOnlyAsRaw = false
}
