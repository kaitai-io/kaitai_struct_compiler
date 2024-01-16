package io.kaitai.struct.languages.components

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._

trait FetchInstances extends LanguageCompiler with ObjectOrientedLanguage with CommonReads {
  override def attrFetchInstances(attr: AttrLikeSpec, id: Identifier): Unit = {
    attrParseIfHeader(id, attr.cond.ifExpr)

    val io = normalIO

    id match {
      case instName: InstanceIdentifier =>
        attrInvokeInstance(instName)
      case _ =>
    }

    if (attr.cond.repeat != NoRepeat)
      condRepeatCommonHeader(id, io, attr.dataType)

    attrFetchInstances2(id, attr.dataType, attr.cond.repeat)

    if (attr.cond.repeat != NoRepeat)
      condRepeatCommonFooter

    attrParseIfFooter(attr.cond.ifExpr)
  }

  def attrFetchInstances2(id: Identifier, dataType: DataType, rep: RepeatSpec, exprTypeOpt: Option[DataType] = None): Unit = {
    dataType match {
      case _: UserType =>
        val exprType = exprTypeOpt.getOrElse(dataType)
        attrInvokeFetchInstances(itemExpr(id, rep), exprType, dataType)
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
