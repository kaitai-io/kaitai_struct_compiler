package io.kaitai.struct.languages.components
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._

trait GenericChecks extends LanguageCompiler with EveryReadIsExpression with EveryWriteIsExpression {
  override def attrCheck(attr: AttrLikeSpec, id: Identifier): Unit = {
    attrParseIfHeader(id, attr.cond.ifExpr)

    val io = normalIO

    attr.cond.repeat match {
      case RepeatEos =>
        condRepeatCommonHeader(id, io, attr.dataType)
        attrCheck2(id, attr.dataType, io, attr.cond.repeat, false)
        condRepeatCommonFooter
      case RepeatExpr(repeatExpr: Ast.expr) =>
        attrArraySizeCheck(id, repeatExpr)
        condRepeatCommonHeader(id, io, attr.dataType)
        attrCheck2(id, attr.dataType, io, attr.cond.repeat, false)
        condRepeatCommonFooter
      case rep: RepeatUntil =>
        // the array must not be empty (always contains at least the `repeat-until: {true}` element)
        attrAssertCmp(exprArraySize(Ast.expr.InternalName(id)), Ast.cmpop.Eq, Ast.expr.IntNum(0), idToMsg(id))
        condRepeatCommonHeader(id, io, attr.dataType)
        attrCheck2(id, attr.dataType, io, attr.cond.repeat, false)
        attrAssertUntilCond(id, attr.dataType, rep, false, idToMsg(id))
        condRepeatCommonFooter
      case NoRepeat =>
        attrCheck2(id, attr.dataType, io, attr.cond.repeat, false)
    }

    attrParseIfFooter(attr.cond.ifExpr)
  }

  def attrCheck2(id: Identifier, dataType: DataType, io: String, repeat: RepeatSpec, isRaw: Boolean) = {
    val item = itemExpr(id, repeat, isRaw)
    dataType match {
      case t: BytesType =>
        attrBytesCheck(item, t, idToMsg(id))
      case st: StrFromBytesType =>
        val bytes = exprStrToBytes(item, st.encoding)
        attrBytesCheck(
          bytes,
          st.bytes,
          idToMsg(id)
        )
      case _ => // no checks
    }
  }

  def attrArraySizeCheck(id: Identifier, expectedSize: Ast.expr): Unit =
    attrAssertEqual(
      exprArraySize(Ast.expr.InternalName(id)),
      expectedSize,
      idToMsg(id)
    )

  def attrBytesCheck(bytes: Ast.expr, t: BytesType, msgId: String): Unit = {
    val actualSize = exprByteArraySize(bytes)
    t match {
      case blt: BytesLimitType => {
        if (blt.terminator.isDefined || blt.padRight.isDefined) {
          // size must be "<= declared" (less than or equal to declared size)
          attrAssertLtE(actualSize, blt.size, msgId)
        } else {
          // size must match declared size exactly
          attrAssertEqual(
            actualSize,
            blt.size,
            msgId
          )
        }
        blt.terminator.foreach { (term) =>
          val actualIndexOfTerm = exprByteArrayIndexOf(bytes, term)
          val isPadRightActive = blt.padRight.map(padByte => padByte != term).getOrElse(false)
          if (!blt.include) {
            attrAssertEqual(actualIndexOfTerm, Ast.expr.IntNum(-1), msgId)
            if (isPadRightActive) {
              condIfHeader(Ast.expr.Compare(actualSize, Ast.cmpop.Eq, blt.size))
              // check if the last byte is not `pad-right`
            }
          } else {
            val lastByteIndex = Ast.expr.BinOp(actualSize, Ast.operator.Sub, Ast.expr.IntNum(1))
            if (!isPadRightActive) {
              condIfHeader(Ast.expr.Compare(actualSize, Ast.cmpop.Lt, blt.size))
              // must not be empty (always contains at least the `terminator` byte)
              attrAssertCmp(actualSize, Ast.cmpop.Eq, Ast.expr.IntNum(0), msgId)
              // the user wants to terminate the value prematurely and there's no `pad-right` that
              // could do that, so the last byte of the value must be `terminator`
              attrAssertEqual(actualIndexOfTerm, lastByteIndex, msgId)
              condIfFooter

              condIfHeader(Ast.expr.Compare(actualSize, Ast.cmpop.Eq, blt.size))
            }
            attrBasicCheck(
              Ast.expr.BoolOp(
                Ast.boolop.And,
                Seq(
                  Ast.expr.Compare(actualIndexOfTerm, Ast.cmpop.NotEq, Ast.expr.IntNum(-1)),
                  Ast.expr.Compare(actualIndexOfTerm, Ast.cmpop.NotEq, lastByteIndex)
                )
              ),
              actualIndexOfTerm,
              lastByteIndex,
              msgId
            )
            if (!isPadRightActive) {
              condIfFooter
            } else {
              condIfHeader(Ast.expr.Compare(actualIndexOfTerm, Ast.cmpop.Eq, Ast.expr.IntNum(-1)))
              // check if the last byte is not `pad-right`
            }
          }
          // intentionally deferring the `condIfFooter` call (in case of `isPadRightActive`)
        }
        blt.padRight.foreach { (padByte) =>
          if (blt.terminator.map(term => padByte != term).getOrElse(true)) {
            val lastByte = exprByteArrayLast(bytes)
            attrBasicCheck(
              Ast.expr.BoolOp(
                Ast.boolop.And,
                Seq(
                  Ast.expr.Compare(actualSize, Ast.cmpop.NotEq, Ast.expr.IntNum(0)),
                  Ast.expr.Compare(
                    lastByte,
                    Ast.cmpop.Eq,
                    Ast.expr.IntNum(padByte)
                  )
                )
              ),
              lastByte,
              Ast.expr.IntNum(padByte),
              msgId
            )
          }
        }
        blt.terminator.foreach { (term) =>
          val isPadRightActive = blt.padRight.map(padByte => padByte != term).getOrElse(false)
          // here's the `condIfFooter` call omitted from the previous `blt.terminator.foreach()` block
          if (isPadRightActive)
            condIfFooter
        }
      }
      case btt: BytesTerminatedType => {
        val actualIndexOfTerm = exprByteArrayIndexOf(bytes, btt.terminator)
        // FIXME: does not take `eos-error: false` into account (assumes `eos-error: true`, i.e. the default setting)
        val expectedIndexOfTerm = if (btt.include) {
          // must not be empty (always contains at least the `terminator` byte)
          attrAssertCmp(actualSize, Ast.cmpop.Eq, Ast.expr.IntNum(0), msgId)

          Ast.expr.BinOp(actualSize, Ast.operator.Sub, Ast.expr.IntNum(1))
        } else {
          Ast.expr.IntNum(-1)
        }

        attrAssertEqual(actualIndexOfTerm, expectedIndexOfTerm, msgId)
      }
      case _ => // no checks
    }
  }

  def attrBasicCheck(checkExpr: Ast.expr, actual: Ast.expr, expected: Ast.expr, msg: String): Unit

  private
  def idToMsg(id: Identifier): String = id match {
    case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
    case NamedIdentifier(name) => name
    case RawIdentifier(innerId) => s"_raw_$innerId"
    case IoStorageIdentifier(innerId) => s"_io_$innerId"
    case InstanceIdentifier(name) => name
    case SpecialIdentifier(name) => name
  }

  def exprByteArraySize(name: Ast.expr) =
    Ast.expr.Attribute(
      name,
      Ast.identifier("size")
    )

  def exprByteArrayLast(name: Ast.expr) =
    Ast.expr.Attribute(
      name,
      Ast.identifier("last")
    )

  def exprByteArrayIndexOf(name: Ast.expr, term: Int) =
    Ast.expr.Call(
      Ast.expr.Attribute(
        name,
        Ast.identifier("index_of")
      ),
      Seq(Ast.expr.IntNum(term))
    )

  def exprArraySize(name: Ast.expr) = exprByteArraySize(name)

  def exprArrayLast(name: Ast.expr) = exprByteArrayLast(name)

  def attrAssertUntilCond(id: Identifier, dataType: DataType, repeat: RepeatUntil, isRaw: Boolean, msg: String): Unit = {
    blockScopeHeader
    handleAssignmentTempVar(
      dataType,
      translator.doName(Identifier.ITERATOR),
      translator.translate(itemExpr(id, repeat, isRaw))
    )
    typeProvider._currentIteratorType = Some(dataType)
    attrAssertEqual(
      repeat.expr,
      Ast.expr.Compare(
        Ast.expr.Name(Ast.identifier(Identifier.INDEX)),
        Ast.cmpop.Eq,
        Ast.expr.BinOp(exprArraySize(Ast.expr.InternalName(id)), Ast.operator.Sub, Ast.expr.IntNum(1))
      ),
      msg
    )
    blockScopeFooter
  }

  def attrAssertEqual(actual: Ast.expr, expected: Ast.expr, msg: String): Unit =
    attrAssertCmp(actual, Ast.cmpop.NotEq, expected, msg)

  def attrAssertLtE(actual: Ast.expr, expected: Ast.expr, msg: String): Unit =
    attrAssertCmp(actual, Ast.cmpop.Gt, expected, msg)

  def attrAssertCmp(actual: Ast.expr, op: Ast.cmpop, expected: Ast.expr, msg: String): Unit =
    attrBasicCheck(
      Ast.expr.Compare(actual, op, expected),
      actual,
      expected,
      msg
    )
}
