package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype.{DataType, FixedEndian}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.translators.GoTranslator

trait GoReads extends CommonReads with ObjectOrientedLanguage with GoSwitchOps {
  val translator: GoTranslator

  def attrRawBytesTypeParse(
    id: Identifier, t: BytesTerminatedType, value: String
  ): String = {
    val rawId = t.process match {
      case None => id
      case Some(_) => RawIdentifier(id)
    }
    translator.terminatedConstructorFact(id, t, Map(
      "terminator" -> t.terminator,
      "include"    -> t.include,
      "consume"    -> t.consume,
      "eosError"   -> t.eosError,
    ))
    s"New_${idToStr(id)}TerminatedType($value)"
  }

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
      case t: BytesTerminatedType =>
        val rawId = t.process match {
          case None => id
          case Some(_) => RawIdentifier(id)
        }
        val r1 = parseExprBytes(translator.outVarCheckRes(parseExpr(t, io, defEndian)), t)
        val expr = attrRawBytesTypeParse(id, t, r1)
        handleAssignment(rawId, expr, rep, isRaw)
        t.process.foreach((proc) => attrProcess(proc, rawId, id, rep))
      case t: BytesType =>
        attrBytesTypeParse(id, t, io, rep, isRaw)
      case st: SwitchType =>
        attrSwitchTypeParse(id, st.on, st.cases, io, rep, defEndian, st.isNullableSwitchRaw, st.combinedType)
      case t: StrFromBytesType =>
        val r1 = parseExprBytes(translator.outVarCheckRes(parseExpr(t.bytes, io, defEndian)), t.bytes)
        val expr = translator.rawTerminatedBytesToStr(r1, t, id)
        handleAssignment(id, expr, rep, isRaw)
      case t: EnumType =>
        val r1 = translator.outVarCheckRes(parseExpr(t.basedOn, io, defEndian))
        val enumSpec = t.enumSpec.get
        val expr = translator.doEnumById(enumSpec.name, r1)
        handleAssignment(id, expr, rep, isRaw)
      case _: BitsType1 =>
        val expr = parseExpr(dataType, io, defEndian)
        val r1 = translator.outVarCheckRes(expr)
        val r2 = s"${r1} != 0"
        handleAssignment(id, r2, rep, isRaw)
      case _ =>
        val expr = parseExpr(dataType, io, defEndian)
        val r = translator.outVarCheckRes(expr)
        handleAssignment(id, r, rep, isRaw)
    }
  }

  def bytesPadTermExpr(id: String, padRight: Option[Int], terminator: Option[Int], include: Boolean): String = {
    val expr0 = id
    val expr1 = padRight match {
      case Some(padByte) => s"kaitai.BytesStripRight($expr0, $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) => s"kaitai.BytesTerminate($expr1, $term, $include)"
      case None => expr1
    }
    expr2
  }

  def parseExprBytes(id: String, dataType: BytesType): String = {
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

  def createSubstreamBuffered(id: Identifier, byteType: BytesType, io: String, rep: RepeatSpec, defEndian: Option[FixedEndian]): String = {
    val rawId = RawIdentifier(id)

    attrParse2(rawId, byteType, io, rep, true, defEndian)

    if (config.readWrite) {
      if (writeNeedsOuterSize(byteType)) {
        /** @note Must be kept in sync with [[attrBytesTypeParse]] */
        val rawRawId = byteType.process match {
          case None => rawId
          case Some(_) => RawIdentifier(rawId)
        }
        val item = itemExpr(rawRawId, rep)
        val itemSizeExprStr = expression(Ast.expr.Attribute(item, Ast.identifier("size")))

        /** FIXME: cannot use [[handleAssignment]] because [[handleAssignmentRepeatUntil]]
         * always tries to assign the value to the [[Identifier.ITERATOR]] variable */
        if (rep == NoRepeat) {
          handleAssignmentSimple(OuterSizeIdentifier(id), itemSizeExprStr)
        } else {
          handleAssignmentRepeatEos(OuterSizeIdentifier(id), itemSizeExprStr)
        }
      }
      if (writeNeedsInnerSize(byteType)) {
        val item = itemExpr(rawId, rep)
        val itemSizeExprStr = expression(Ast.expr.Attribute(item, Ast.identifier("size")))

        /** FIXME: cannot use [[handleAssignment]] because [[handleAssignmentRepeatUntil]]
         * always tries to assign the value to the [[Identifier.ITERATOR]] variable */
        if (rep == NoRepeat) {
          handleAssignmentSimple(InnerSizeIdentifier(id), itemSizeExprStr)
        } else {
          handleAssignmentRepeatEos(InnerSizeIdentifier(id), itemSizeExprStr)
        }
      }
    }

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

  def parseExpr(dataType: DataType, io: String, defEndian: Option[FixedEndian]): String
}
