package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype.{DataType, FixedEndian}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.translators.{GoTranslator, ResultLocalVar, ResultString, TranslatorResult}

trait GoReads extends CommonReads with ObjectOrientedLanguage with GoSwitchOps {
  val translator: GoTranslator

  def attrBytesTypeParse(
    id: Identifier,
    dataType: BytesType,
    io: String,
    rep: RepeatSpec,
    isRaw: Boolean
  ): Unit = {
    val rawId = dataType.process match {
      case None => id
      case Some(_) => RawIdentifier(id)
    }
    val expr = parseExprBytes(translator.outVarCheckRes(parseExpr(dataType, io, None)), dataType)
    handleAssignment(rawId, expr, rep, isRaw)
    dataType.process.foreach((proc) => attrProcess(proc, rawId, id, rep))
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
    switchCases[DataType](id, on, cases,
      (dataType) => {
        attrParse2(id, dataType, io, rep, false, defEndian, Some(assignType))
      },
      {
        case dataType@(t: BytesType) =>
          attrParse2(RawIdentifier(id), dataType, io, rep, false, defEndian, Some(assignType))
        case dataType =>
          attrParse2(id, dataType, io, rep, false, defEndian, Some(assignType))
      }
    )
  }

  override def attrParse2(
    id: Identifier,
    dataType: DataType,
    io: String,
    rep: RepeatSpec,
    isRaw: Boolean,
    defEndian: Option[FixedEndian],
    assignType: Option[DataType] = None
  ): Unit = {
    dataType match {
      case t: UserType =>
        attrUserTypeParse(id, t, io, rep, defEndian)
      case t: BytesType =>
        attrBytesTypeParse(id, t, io, rep, isRaw)
      case st: SwitchType =>
        attrSwitchTypeParse(id, st.on, st.cases, io, rep, defEndian, st.isNullableSwitchRaw, st.combinedType)
      case t: StrFromBytesType =>
        val r1 = parseExprBytes(translator.outVarCheckRes(parseExpr(t.bytes, io, defEndian)), t.bytes)
        val expr = translator.bytesToStr(translator.resToStr(r1), t.encoding)
        handleAssignment(id, expr, rep, isRaw)
      case t: EnumType =>
        val r1 = translator.outVarCheckRes(parseExpr(t.basedOn, io, defEndian))
        val enumSpec = t.enumSpec.get
        val expr = translator.trEnumById(enumSpec.name, translator.resToStr(r1))
        handleAssignment(id, expr, rep, isRaw)
      case _: BitsType1 =>
        val expr = parseExpr(dataType, io, defEndian)
        val r1 = translator.outVarCheckRes(expr)
        val r2 = ResultString(s"${translator.resToStr(r1)} != 0")
        handleAssignment(id, r2, rep, isRaw)
      case _ =>
        val expr = parseExpr(dataType, io, defEndian)
        val r = translator.outVarCheckRes(expr)
        handleAssignment(id, r, rep, isRaw)
    }
  }

  def bytesPadTermExpr(id: ResultLocalVar, padRight: Option[Int], terminator: Option[Seq[Byte]], include: Boolean): String = {
    val expr0 = translator.resToStr(id)
    val expr1 = padRight match {
      case Some(padByte) => s"kaitai.BytesStripRight($expr0, $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) =>
        if (term.length == 1) {
          val t = term.head & 0xff
          s"kaitai.BytesTerminate($expr1, $t, $include)"
        } else {
          s"kaitai.BytesTerminateMulti($expr1, ${translator.resToStr(translator.doByteArrayLiteral(term))}, $include)"
        }
      case None => expr1
    }
    expr2
  }

  def parseExprBytes(id: ResultLocalVar, dataType: BytesType): ResultLocalVar = {
    dataType match {
      case BytesEosType(terminator, include, padRight, _) =>
        translator.outTransform(id, bytesPadTermExpr(id, padRight, terminator, include))
      case BytesLimitType(_, terminator, include, padRight, _) =>
        translator.outTransform(id, bytesPadTermExpr(id, padRight, terminator, include))
      case _ =>
        id
    }
  }

  def attrUserTypeParse(id: Identifier, dataType: UserType, io: String, rep: RepeatSpec, defEndian: Option[FixedEndian]): Unit = {
    val newIO = dataType match {
      case knownSizeType: UserTypeFromBytes =>
        // we have a fixed buffer, thus we shall create separate IO for it
        val rawId = RawIdentifier(id)
        val byteType = knownSizeType.bytes

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
