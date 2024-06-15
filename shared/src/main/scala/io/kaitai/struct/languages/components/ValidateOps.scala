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
    val attrTypeRef = attr.dataTypeComposite.asNonOwning()

    valid match {
      case ValidationEq(expected) =>
        attrValidateExprCompare(attrId, attr, Ast.cmpop.Eq, expected, ValidationNotEqualError(attrTypeRef))
      case ValidationMin(min) =>
        attrValidateExprCompare(attrId, attr, Ast.cmpop.GtE, min, ValidationLessThanError(attrTypeRef))
      case ValidationMax(max) =>
        attrValidateExprCompare(attrId, attr, Ast.cmpop.LtE, max, ValidationGreaterThanError(attrTypeRef))
      case ValidationRange(min, max) =>
        attrValidateExprCompare(attrId, attr, Ast.cmpop.GtE, min, ValidationLessThanError(attrTypeRef))
        attrValidateExprCompare(attrId, attr, Ast.cmpop.LtE, max, ValidationGreaterThanError(attrTypeRef))
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
          attrTypeRef,
          checkExpr = bigOrExpr,
          err = ValidationNotAnyOfError(attrTypeRef),
          errArgs = List(
            Ast.expr.InternalName(attrId),
            Ast.expr.InternalName(IoIdentifier),
            Ast.expr.Str(attr.path.mkString("/", "/", ""))
          )
        )
      case ValidationExpr(expr) =>
        blockScopeHeader
        typeProvider._currentIteratorType = Some(attrTypeRef)
        // Store value of attribute in the temporary variable with a name that is
        // used for `_` variable, because in expression we refer to current value
        // using this variable
        val borrowed = Ast.expr.Name(Ast.identifier(Identifier.THIS))
        handleAssignmentTempVar(
          attrTypeRef,
          translator.translate(borrowed),
          translator.translate(Ast.expr.InternalName(attrId))
        )
        attrValidateExpr(
          attrId,
          attrTypeRef,
          expr,
          ValidationExprError(attrTypeRef),
          List(
            borrowed,
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
      attr.dataTypeComposite.asNonOwning(),
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
