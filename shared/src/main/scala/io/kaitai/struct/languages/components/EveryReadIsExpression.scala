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
    rep: RepeatSpec,
    isRaw: Boolean,
    defEndian: Option[FixedEndian],
    assignTypeOpt: Option[DataType] = None
  ): Unit = {
    val assignType = assignTypeOpt.getOrElse(dataType)
    val needsDebug = attrDebugNeeded(id) && rep != NoRepeat

    if (needsDebug)
      attrDebugStart(id, dataType, Some(io), rep)

    dataType match {
      case t: UserType =>
        attrUserTypeParse(id, t, io, rep, defEndian, assignType)
      case t: BytesType =>
        attrBytesTypeParse(id, t, io, rep, isRaw)
      case st: SwitchType =>
        // nullability of a single switch-type element doesn't affect the nullability of the whole array,
        // see https://github.com/kaitai-io/kaitai_struct/issues/494
        val isNullable = rep == NoRepeat && (if (switchBytesOnlyAsRaw) {
          st.isNullableSwitchRaw
        } else {
          st.isNullable
        })

        attrSwitchTypeParse(id, st.on, st.cases, io, rep, defEndian, isNullable, st.combinedType)
      case t: StrFromBytesType =>
        val expr = translator.bytesToStr(parseExprBytes(t.bytes, io), t.encoding)
        handleAssignment(id, expr, rep, isRaw)
      case t: EnumType =>
        val expr = translator.doEnumById(t.enumSpec.get, parseExpr(t.basedOn, t.basedOn, io, defEndian))
        handleAssignment(id, expr, rep, isRaw)
      case _ =>
        val expr = parseExpr(dataType, assignType, io, defEndian)
        handleAssignment(id, expr, rep, isRaw)
    }

    if (needsDebug)
      attrDebugEnd(id, dataType, io, rep)
  }

  def attrBytesTypeParse(
    id: Identifier,
    dataType: BytesType,
    io: String,
    rep: RepeatSpec,
    isRaw: Boolean
  ): Unit = {
    // use intermediate variable name, if we'll be doing post-processing
    val rawId = dataType.process match {
      case None => id
      case Some(_) => RawIdentifier(id)
    }

    val expr = parseExprBytes(dataType, io)
    handleAssignment(rawId, expr, rep, isRaw)

    // apply post-processing
    dataType.process.foreach((proc) => attrProcess(proc, rawId, id, rep))
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

  def attrUserTypeParse(id: Identifier, dataType: UserType, io: String, rep: RepeatSpec, defEndian: Option[FixedEndian], assignType: DataType): Unit = {
    val newIO = dataType match {
      case knownSizeType: UserTypeFromBytes =>
        // we have a fixed buffer, thus we shall create separate IO for it
        createSubstream(id, knownSizeType.bytes, io, rep, defEndian)
      case _: UserTypeInstream =>
        // no fixed buffer, just use regular IO
        io
    }
    val expr = parseExpr(dataType, dataType, newIO, defEndian)
    if (config.autoRead) {
      handleAssignment(id, expr, rep, false)
    } else {
      // Disabled autoRead requires one to actually call `_read` method on constructed
      // user type, and this must be done as a separate statement - or else exception
      // handler would blast the whole structure, not only this element. This, in turn,
      // makes us assign constructed element to a temporary variable in case of arrays.
      rep match {
        case NoRepeat =>
          handleAssignmentSimple(id, expr)
          userTypeDebugRead(privateMemberName(id), dataType, assignType)
        case _ =>
          val tempVarName = localTemporaryName(id)
          handleAssignmentTempVar(dataType, tempVarName, expr)
          handleAssignment(id, tempVarName, rep, false)
          userTypeDebugRead(tempVarName, dataType, assignType)
      }
    }
  }

  /**
    * Creates a substream for a specific member `id`. A substream will be using underlying bytes
    * type `byteType`, repeat specification `rep` and default endianness of `defEndian`.
    *
    * @param id
    * @param byteType underlying bytes type
    * @param io parent stream to derive substream from
    * @param rep repeat specification for underlying bytes type
    * @param defEndian default endianness specification
    * @return string reference to a freshly created substream
    */
  def createSubstream(id: Identifier, bt: BytesType, io: String, rep: RepeatSpec, defEndian: Option[FixedEndian]): String = {
    if (config.zeroCopySubstream) {
      bt match {
        case blt @ BytesLimitType(_, None, _, None, None)   =>
          createSubstreamFixedSize(id, blt, io, rep, defEndian)
        case _ =>
          // fall back to buffered implementation
          createSubstreamBuffered(id, bt, io, rep, defEndian)
      }
    } else {
      // if zero-copy substreams were declined, always use buffered implementation
      createSubstreamBuffered(id, bt, io, rep, defEndian)
    }
  }

  /**
    * Creates a substream for a specific member `id` of fixed sized `sizeExpr`, based off a parent
    * stream of `io`.
    *
    * Default implementation just short-circuits this to `createSubstreamBuffered`, which is an
    * inefficient, but guaranteed to work implementation.
    *
    * @param id identifier of a member that this stream is for
    * @param sizeExpr expression designating size of substream in bytes
    * @param io parent stream to derive substream from
    * @return string reference to a freshly created substream
    */
  def createSubstreamFixedSize(id: Identifier, blt: BytesLimitType, io: String, rep: RepeatSpec, defEndian: Option[FixedEndian]): String =
    createSubstreamBuffered(id, blt, io, rep, defEndian)

  /**
    * Creates a substream by reading bytes that will comprise the stream first into a buffer in
    * memory, and then wrapping that buffer as a new stream.
    * @param id identifier of a member that this stream is for
    * @param byteType underlying bytes type
    * @param io parent stream to derive substream from
    * @param rep repeat specification for underlying bytes type
    * @param defEndian default endianness specification
    * @return string reference to a freshly created substream
    */
  def createSubstreamBuffered(id: Identifier, byteType: BytesType, io: String, rep: RepeatSpec, defEndian: Option[FixedEndian]): String = {
    val rawId = RawIdentifier(id)

    attrParse2(rawId, byteType, io, rep, true, defEndian)

    val extraType = rep match {
      case NoRepeat => byteType
      case _ => ArrayTypeInStream(byteType)
    }

    this match {
      case thisStore: AllocateAndStoreIO =>
        thisStore.allocateIO(rawId, rep)
      case thisLocal: AllocateIOLocalVar =>
        thisLocal.allocateIO(rawId, rep)
    }
  }

  def attrSwitchTypeParse(
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
        attrParse2(id, dataType, io, rep, false, defEndian, Some(assignType))
      },
      (dataType) => if (switchBytesOnlyAsRaw) {
        dataType match {
          case t: BytesType =>
            attrParse2(RawIdentifier(id), dataType, io, rep, false, defEndian, Some(assignType))
          case _ =>
            attrParse2(id, dataType, io, rep, false, defEndian, Some(assignType))
        }
      } else {
        attrParse2(id, dataType, io, rep, false, defEndian, Some(assignType))
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
  def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Seq[Byte]], include: Boolean): String
  def userTypeDebugRead(id: String, dataType: DataType, assignType: DataType): Unit = ???

  def instanceCalculate(instName: Identifier, dataType: DataType, value: Ast.expr): Unit = {
    if (attrDebugNeeded(instName))
      attrDebugStart(instName, dataType, None, NoRepeat)
    handleAssignmentSimple(instName, expression(value))
  }

  override def attrDebugNeeded(attrId: Identifier): Boolean = {
    if (!config.readStoresPos)
      return false

    attrId match {
      case _: NamedIdentifier | _: NumberedIdentifier | _: InstanceIdentifier => true
      case _ => super.attrDebugNeeded(attrId)
    }
  }
}
