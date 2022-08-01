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
    valid match {
      case ValidationEq(expected) =>
        attrValidateExprCompare(attrId, attr, Ast.cmpop.Eq, expected, ValidationNotEqualError(attr.dataTypeComposite))
      case ValidationMin(min) =>
        attrValidateExprCompare(attrId, attr, Ast.cmpop.GtE, min, ValidationLessThanError(attr.dataTypeComposite))
      case ValidationMax(max) =>
        attrValidateExprCompare(attrId, attr, Ast.cmpop.LtE, max, ValidationGreaterThanError(attr.dataTypeComposite))
      case ValidationRange(min, max) =>
        attrValidateExprCompare(attrId, attr, Ast.cmpop.GtE, min, ValidationLessThanError(attr.dataTypeComposite))
        attrValidateExprCompare(attrId, attr, Ast.cmpop.LtE, max, ValidationGreaterThanError(attr.dataTypeComposite))
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
          attrId,
          attr.dataTypeComposite,
          checkExpr = bigOrExpr,
          err = ValidationNotAnyOfError(attr.dataTypeComposite),
          errArgs = List(
            Ast.expr.InternalName(attrId),
            Ast.expr.InternalName(IoIdentifier),
            Ast.expr.Str(attr.path.mkString("/", "/", ""))
          )
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
          attrId,
          attr.dataTypeComposite,
          expr,
          ValidationExprError(attr.dataTypeComposite),
          List(
            Ast.expr.InternalName(attrId),
            Ast.expr.InternalName(IoIdentifier),
            Ast.expr.Str(attr.path.mkString("/", "/", ""))
          )
        )
        blockScopeFooter
    }
  }

  def attrValidateExprCompare(attrId: Identifier, attr: AttrLikeSpec, op: Ast.cmpop, expected: Ast.expr, err: KSError): Unit = {
    attrValidateExpr(
      attrId,
      attr.dataTypeComposite,
      checkExpr = Ast.expr.Compare(
        Ast.expr.InternalName(attrId),
        op,
        expected
      ),
      err = err,
      errArgs = List(
        expected,
        Ast.expr.InternalName(attrId),
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
