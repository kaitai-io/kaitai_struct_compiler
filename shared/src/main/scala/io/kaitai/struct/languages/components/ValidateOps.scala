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

  def attrValidate(attrId: Identifier, attr: AttrLikeSpec, valid: ValidationSpec, useIo: Boolean): Unit = {
    valid match {
      case ValidationEq(expected) =>
        attrValidateExprCompare(attrId, attr, Ast.cmpop.Eq, expected, ValidationNotEqualError(attr.dataTypeComposite), useIo)
      case ValidationMin(min) =>
        attrValidateExprCompare(attrId, attr, Ast.cmpop.GtE, min, ValidationLessThanError(attr.dataTypeComposite), useIo)
      case ValidationMax(max) =>
        attrValidateExprCompare(attrId, attr, Ast.cmpop.LtE, max, ValidationGreaterThanError(attr.dataTypeComposite), useIo)
      case ValidationRange(min, max) =>
        attrValidateExprCompare(attrId, attr, Ast.cmpop.GtE, min, ValidationLessThanError(attr.dataTypeComposite), useIo)
        attrValidateExprCompare(attrId, attr, Ast.cmpop.LtE, max, ValidationGreaterThanError(attr.dataTypeComposite), useIo)
      case ValidationAnyOf(values) =>
        val bigOrExpr = Ast.expr.BoolOp(
          Ast.boolop.Or,
          values.map(expected =>
            Ast.expr.Compare(
              Ast.expr.InternalName(attrId),
              Ast.cmpop.Eq,
              expected
            )
          )
        )

        attrValidateExpr(
          attr,
          checkExpr = bigOrExpr,
          err = ValidationNotAnyOfError(attr.dataTypeComposite),
          useIo
        )
      case ValidationExpr(expr) =>
        blockScopeHeader
        typeProvider._currentIteratorType = Some(attr.dataTypeComposite)
        handleAssignmentTempVar(
          attr.dataTypeComposite,
          translator.translate(Ast.expr.Name(Ast.identifier(Identifier.ITERATOR))),
          translator.translate(Ast.expr.InternalName(attrId))
        )
        attrValidateExpr(
          attr,
          expr,
          ValidationExprError(attr.dataTypeComposite),
          useIo
        )
        blockScopeFooter
    }
  }

  def attrValidateExprCompare(
    attrId: Identifier,
    attr: AttrLikeSpec,
    op: Ast.cmpop,
    expected: Ast.expr,
    err: KSError,
    useIo: Boolean
  ): Unit = {
    attrValidateExpr(
      attr,
      checkExpr = Ast.expr.Compare(
        Ast.expr.InternalName(attrId),
        op,
        expected
      ),
      err = err,
      useIo = useIo,
      expected = Some(expected)
    )
  }

  def attrValidateExpr(attr: AttrLikeSpec, checkExpr: Ast.expr, err: KSError, useIo: Boolean, expected: Option[Ast.expr] = None): Unit = {}
  def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit
  def blockScopeHeader: Unit
  def blockScopeFooter: Unit

}
