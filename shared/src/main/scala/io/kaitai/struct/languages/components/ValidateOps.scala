package io.kaitai.struct.languages.components

import io.kaitai.struct.ClassTypeProvider
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.translators.AbstractTranslator

/**
  * Common interface for validation operations.
  */
trait ValidateOps extends ExceptionNames {
  val translator: AbstractTranslator
  val typeProvider: ClassTypeProvider

  def attrValidate(attrId: Identifier, attr: AttrLikeSpec, valid: ValidationSpec): Unit = {
    val itemValue = Identifier.itemExpr(attrId, attr.cond.repeat)
    valid match {
      case ValidationEq(expected) =>
        attrValidateExprCompare(attrId, attr, Ast.cmpop.Eq, expected, ValidationNotEqualError(attr.dataType))
      case ValidationMin(min) =>
        attrValidateExprCompare(attrId, attr, Ast.cmpop.GtE, min, ValidationLessThanError(attr.dataType))
      case ValidationMax(max) =>
        attrValidateExprCompare(attrId, attr, Ast.cmpop.LtE, max, ValidationGreaterThanError(attr.dataType))
      case ValidationRange(min, max) =>
        attrValidateExprCompare(attrId, attr, Ast.cmpop.GtE, min, ValidationLessThanError(attr.dataType))
        attrValidateExprCompare(attrId, attr, Ast.cmpop.LtE, max, ValidationGreaterThanError(attr.dataType))
      case ValidationAnyOf(values) =>
        val bigOrExpr = Ast.expr.BoolOp(
          Ast.boolop.Or,
          values.map(expected =>
            Ast.expr.Compare(
              itemValue,
              Ast.cmpop.Eq,
              expected
            )
          )
        )

        attrValidateExpr(
          attrId,
          attr.dataType,
          checkExpr = bigOrExpr,
          err = ValidationNotAnyOfError(attr.dataType),
          errArgs = List(
            itemValue,
            Ast.expr.InternalName(IoIdentifier),
            Ast.expr.Str(attr.path.mkString("/", "/", ""))
          )
        )
      case ValidationExpr(expr) =>
        blockScopeHeader
        typeProvider._currentIteratorType = Some(attr.dataType)
        handleAssignmentTempVar(
          attr.dataType,
          translator.translate(Ast.expr.Name(Ast.identifier(Identifier.ITERATOR))),
          translator.translate(itemValue)
        )
        attrValidateExpr(
          attrId,
          attr.dataType,
          expr,
          ValidationExprError(attr.dataType),
          List(
            itemValue,
            Ast.expr.InternalName(IoIdentifier),
            Ast.expr.Str(attr.path.mkString("/", "/", ""))
          )
        )
        blockScopeFooter
    }
  }

  def attrValidateExprCompare(attrId: Identifier, attr: AttrLikeSpec, op: Ast.cmpop, expected: Ast.expr, err: KSError): Unit = {
    val itemValue = Identifier.itemExpr(attrId, attr.cond.repeat)
    attrValidateExpr(
      attrId,
      attr.dataType,
      checkExpr = Ast.expr.Compare(
        itemValue,
        op,
        expected
      ),
      err = err,
      errArgs = List(
        expected,
        itemValue,
        Ast.expr.InternalName(IoIdentifier),
        Ast.expr.Str(attr.path.mkString("/", "/", ""))
      )
    )
  }

  def attrValidateExpr(attrId: Identifier, attrType: DataType, checkExpr: Ast.expr, err: KSError, errArgs: List[Ast.expr]): Unit = {}
  def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit
  def blockScopeHeader: Unit
  def blockScopeFooter: Unit

}
