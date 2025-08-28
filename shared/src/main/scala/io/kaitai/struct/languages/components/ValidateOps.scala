package io.kaitai.struct.languages.components

import io.kaitai.struct.ClassTypeProvider
import io.kaitai.struct.datatype._
import io.kaitai.struct.datatype.DataType.EnumType
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.translators.AbstractTranslator

/**
  * Common interface for validation operations.
  */
trait ValidateOps extends ExceptionNames {
  val translator: AbstractTranslator
  val typeProvider: ClassTypeProvider

  def attrValidate(attr: AttrLikeSpec, valid: ValidationSpec, useIo: Boolean): Unit = {
    val itemValue = Identifier.itemExpr(attr.id, attr.cond.repeat)
    valid match {
      case ValidationEq(expected) =>
        attrValidateExprCompare(attr, Ast.cmpop.Eq, expected, ValidationNotEqualError(attr.dataType), useIo)
      case ValidationMin(min) =>
        attrValidateExprCompare(attr, Ast.cmpop.GtE, min, ValidationLessThanError(attr.dataType), useIo)
      case ValidationMax(max) =>
        attrValidateExprCompare(attr, Ast.cmpop.LtE, max, ValidationGreaterThanError(attr.dataType), useIo)
      case ValidationRange(min, max) =>
        attrValidateExprCompare(attr, Ast.cmpop.GtE, min, ValidationLessThanError(attr.dataType), useIo)
        attrValidateExprCompare(attr, Ast.cmpop.LtE, max, ValidationGreaterThanError(attr.dataType), useIo)
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
          attr,
          checkExpr = bigOrExpr,
          err = ValidationNotAnyOfError(attr.dataType),
          useIo = useIo,
          actual = itemValue
        )
      case ValidationInEnum() =>
        attrValidateInEnum(
          attr,
          attr.dataType.asInstanceOf[EnumType],
          itemValue,
          ValidationNotInEnumError(attr.dataType),
          useIo
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
          attr,
          expr,
          ValidationExprError(attr.dataType),
          useIo,
          itemValue
        )
        blockScopeFooter
    }
  }

  def attrValidateExprCompare(
    attr: AttrLikeSpec,
    op: Ast.cmpop,
    expected: Ast.expr,
    err: KSError,
    useIo: Boolean
  ): Unit = {
    val itemValue = Identifier.itemExpr(attr.id, attr.cond.repeat)
    attrValidateExpr(
      attr,
      checkExpr = Ast.expr.Compare(
        itemValue,
        op,
        expected
      ),
      err = err,
      useIo = useIo,
      actual = itemValue,
      expected = Some(expected)
    )
  }

  def attrValidateExpr(attr: AttrLikeSpec, checkExpr: Ast.expr, err: KSError, useIo: Boolean, actual: Ast.expr, expected: Option[Ast.expr] = None): Unit = {}
  def attrValidateInEnum(attr: AttrLikeSpec, et: EnumType, valueExpr: Ast.expr, err: ValidationNotInEnumError, useIo: Boolean): Unit = {}
  def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit
  def blockScopeHeader: Unit
  def blockScopeFooter: Unit

}
