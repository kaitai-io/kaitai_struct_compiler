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
      case RepeatUntil(untilExpr: Ast.expr) =>
        condRepeatCommonHeader(id, io, attr.dataType)
        attrCheck2(id, attr.dataType, io, attr.cond.repeat, false)
        condRepeatCommonFooter
      case NoRepeat =>
        attrCheck2(id, attr.dataType, io, attr.cond.repeat, false)
    }

    attrParseIfFooter(attr.cond.ifExpr)
  }

  def attrCheck2(id: Identifier, dataType: DataType, io: String, repeat: RepeatSpec, isRaw: Boolean) = {
    val item = writeExprAsExpr(id, repeat, isRaw)
    dataType match {
      case t: BytesType =>
        attrByteSizeCheck(Ast.expr.InternalName(id), t, exprByteArraySize(item), idToMsg(id))
      case st: StrFromBytesType =>
        val bytes = exprStrToBytes(item, st.encoding)
        attrByteSizeCheck(
          bytes,
          st.bytes,
          exprByteArraySize(bytes),
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

  def attrByteSizeCheck(name: Ast.expr, t: BytesType, actualSize: Ast.expr, msgId: String): Unit = {
    t match {
      case blt: BytesLimitType =>
        if (blt.padRight.isDefined) {
          // size must be "<= declared"
          attrAssertLtE(actualSize, blt.size, msgId)
          blt.terminator.foreach { (term) =>
            if (blt.include)
              attrAssertLastByte(name, term, msgId)
          }
        } else {
          blt.terminator match {
            case Some(term) =>
              if (!blt.include) {
                // size must be "<= (declared - 1)", i.e. "< declared"
                attrAssertLt(actualSize, blt.size, msgId)
              } else {
                // terminator is included into the string, so
                // size must be "<= declared"
                attrAssertLtE(actualSize, blt.size, msgId)
                attrAssertLastByte(name, term, msgId)
              }
            case None =>
              // size must match declared size exactly
              attrAssertEqual(
                actualSize,
                blt.size,
                msgId
              )
          }
        }
      case btt: BytesTerminatedType =>
        if (btt.include)
          attrAssertLastByte(name, btt.terminator, msgId)
      case _ => // no checks
    }
  }

  def attrBasicCheck(checkExpr: String, actual: String, expected: String, msg: String): Unit

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

  def exprArraySize(name: Ast.expr) = exprByteArraySize(name)

  def attrAssertLastByte(name: Ast.expr, expectedLast: Int, msg: String): Unit = {
    attrAssertEqual(
      Ast.expr.Attribute(
        name,
        Ast.identifier("last")
      ),
      Ast.expr.IntNum(expectedLast),
      msg
    )
  }

  def attrAssertEqual(actual: Ast.expr, expected: Ast.expr, msg: String): Unit =
    attrAssertCmp(actual, Ast.cmpop.NotEq, expected, msg)

  def attrAssertLtE(actual: Ast.expr, expected: Ast.expr, msg: String): Unit =
    attrAssertCmp(actual, Ast.cmpop.Gt, expected, msg)

  def attrAssertLt(actual: Ast.expr, expected: Ast.expr, msg: String): Unit =
    attrAssertCmp(actual, Ast.cmpop.GtE, expected, msg)

  def attrAssertCmp(actual: Ast.expr, op: Ast.cmpop, expected: Ast.expr, msg: String): Unit =
    attrBasicCheck(
      translator.translate(Ast.expr.Compare(actual, op, expected)),
      translator.translate(actual),
      translator.translate(expected),
      msg
    )
}
