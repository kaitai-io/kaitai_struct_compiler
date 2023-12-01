package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._

trait GenericChecks extends CommonChecks with CommonReads {
   override def attrCheck2(id: Identifier, dataType: DataType, repeat: RepeatSpec, shouldDependOnIo: Option[Boolean], exprTypeOpt: Option[DataType] = None) = {
    val item = itemExpr(id, repeat)
    dataType match {
      case ut: UserType =>
        val itemUserType =
          if (exprTypeOpt.map(exprType => !exprType.isInstanceOf[UserType]).getOrElse(false))
            Ast.expr.CastToType(item, Ast.typeId(true, ut.classSpec.get.name))
          else
            item
        attrUserTypeCheck(id, itemUserType, ut, shouldDependOnIo)
      case t: BytesType =>
        val itemBytes =
          if (exprTypeOpt.map(exprType => !exprType.isInstanceOf[BytesType]).getOrElse(false))
            Ast.expr.CastToType(item, Ast.typeId(false, Seq("bytes")))
          else
            item
        attrBytesCheck(id, itemBytes, t, shouldDependOnIo)
      case st: StrFromBytesType =>
        val itemStr =
          if (exprTypeOpt.map(exprType => !exprType.isInstanceOf[StrType]).getOrElse(false))
            Ast.expr.CastToType(item, Ast.typeId(false, Seq("str")))
          else
            item
        val bytes = exprStrToBytes(itemStr, st.encoding)
        attrBytesCheck(id, bytes, st.bytes, shouldDependOnIo)
      case st: SwitchType =>
        attrSwitchCheck(id, st.on, st.cases, repeat, shouldDependOnIo, st.combinedType)
      case _ => // no checks
    }
  }
}
