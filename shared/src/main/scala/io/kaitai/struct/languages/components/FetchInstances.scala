package io.kaitai.struct.languages.components

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._

trait FetchInstances extends LanguageCompiler with ObjectOrientedLanguage with EveryReadIsExpression {
  override def attrFetchInstances(attr: AttrLikeSpec, id: Identifier): Unit = {
    val isInstance =
      id match {
        case instName: InstanceIdentifier =>
          attrInvokeInstance(instName)
          instanceHasValueIfHeader(instName)
          true
        case _ =>
          false
      }

    if (!isInstance)
      attrParseIfHeader(id, attr.cond.ifExpr)

    val io = normalIO

    if (attr.cond.repeat != NoRepeat)
      condRepeatCommonHeader(id, io, attr.dataType)

    attrFetchInstances2(id, attr.dataType, attr.cond.repeat)

    if (attr.cond.repeat != NoRepeat)
      condRepeatCommonFooter

    if (!isInstance)
      attrParseIfFooter(attr.cond.ifExpr)
    else
      instanceHasValueIfFooter()
  }

  def attrFetchInstances2(id: Identifier, dataType: DataType, rep: RepeatSpec, exprTypeOpt: Option[DataType] = None): Unit = {
    dataType match {
      case _: UserType =>
        val exprType = exprTypeOpt.getOrElse(dataType)
        attrInvokeFetchInstances(Identifier.itemExpr(id, rep), exprType, dataType)
      case st: SwitchType =>
        attrSwitchTypeFetchInstances(id, st.on, st.cases, rep, st.combinedType)
      case _ =>
    }
  }

  def attrSwitchTypeFetchInstances(id: Identifier, on: Ast.expr, cases: Map[Ast.expr, DataType], rep: RepeatSpec, assignType: DataType): Unit = {
    switchCases[DataType](id, on, cases,
      (dataType) => {
        attrFetchInstances2(id, dataType, rep, Some(assignType))
      },
      (dataType) => {
        // TODO: process switchBytesOnlyAsRaw
        attrFetchInstances2(id, dataType, rep, Some(assignType))
      }
    )
  }
}
