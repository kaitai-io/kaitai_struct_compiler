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
        condRepeatEosHeader2(id, io, attr.dataType, needRaw(attr.dataType))
        attrCheck2(id, attr.dataType, io, attr.cond.repeat, false)
        condRepeatEosFooter
      case RepeatExpr(repeatExpr: Ast.expr) =>
        attrArraySizeCheck(id, repeatExpr)
        condRepeatExprHeader2(id, io, attr.dataType, needRaw(attr.dataType), repeatExpr)
        attrCheck2(id, attr.dataType, io, attr.cond.repeat, false)
        condRepeatExprFooter
      case RepeatUntil(untilExpr: Ast.expr) =>
        condRepeatUntilHeader(id, io, attr.dataType, needRaw(attr.dataType), untilExpr)
        attrCheck2(id, attr.dataType, io, attr.cond.repeat, false)
        condRepeatUntilFooter(id, io, attr.dataType, needRaww(attr.dataType), untilExpr)
      case NoRepeat =>
        attrCheck2(id, attr.dataType, io, attr.cond.repeat, false)
    }

    attrParseIfFooter(attr.cond.ifExpr)
  }

  def attrCheck2(id: Identifier, dataType: DataType, io: String, repeat: RepeatSpec, isRaw: Boolean) = {
    dataType match {
      case t: BytesType =>
        attrByteSizeCheck(id, t, exprByteArraySize(idToName(id)))
      case st: StrFromBytesType =>
        attrByteSizeCheck(id, st.bytes,
          exprByteArraySize(exprStrToBytes(idToName(id), st.encoding))
        )
      case _ => // no checks
    }
  }

  def attrArraySizeCheck(id: Identifier, expectedSize: Ast.expr): Unit =
    attrAssertEqual(
      exprArraySize(idToName(id)),
      expectedSize,
      idToMsg(id)
    )

  def attrByteSizeCheck(id: Identifier, t: BytesType, actualSize: Ast.expr): Unit = {
    t match {
      case blt: BytesLimitType =>
        if (blt.padRight.isDefined) {
          // size must be "<= declared"
          attrAssertLtE(actualSize, blt.size, idToMsg(id))
          blt.terminator.foreach { (term) =>
            if (blt.include)
              attrAssertLastByte(id, term, idToMsg(id))
          }
        } else {
          blt.terminator match {
            case Some(term) =>
              if (!blt.include) {
                // size must be "<= (declared - 1)", i.e. "< declared"
                attrAssertLt(actualSize, blt.size, idToMsg(id))
              } else {
                // terminator is included into the string, so
                // size must be "<= declared"
                attrAssertLtE(actualSize, blt.size, idToMsg(id))
                attrAssertLastByte(id, term, idToMsg(id))
              }
            case None =>
              // size must match declared size exactly
              attrAssertEqual(
                actualSize,
                blt.size,
                idToMsg(id)
              )
          }
        }
      case btt: BytesTerminatedType =>
        if (btt.include)
          attrAssertLastByte(id, btt.terminator, idToMsg(id))
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

  def idToName(id: Identifier): Ast.expr.Name = Ast.expr.Name(Ast.identifier(idToMsg(id)))

  def exprByteArraySize(name: Ast.expr) =
    Ast.expr.Attribute(
      name,
      Ast.identifier("size")
    )

  def exprArraySize(name: Ast.expr) = exprByteArraySize(name)

  def exprStrToBytes(name: Ast.expr, encoding: String) =
    Ast.expr.Call(
      Ast.expr.Attribute(
        name,
        Ast.identifier("to_b")
      ),
      Seq(Ast.expr.Str(encoding))
    )

  def attrAssertLastByte(actualId: Identifier, expectedLast: Int, msg: String): Unit = {
    attrAssertEqual(
      Ast.expr.Attribute(
        idToName(actualId),
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
