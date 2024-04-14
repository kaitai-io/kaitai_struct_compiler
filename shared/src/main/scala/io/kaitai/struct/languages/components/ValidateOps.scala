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

  def attrValidate(attr: AttrLikeSpec, valid: ValidationSpec): Unit = {
    val attrTypeRef = attr.dataType.asNonOwning()
    val itemValue = Identifier.itemExpr(attr.id, attr.cond.repeat)
    valid match {
      case ValidationEq(expected) =>
        attrValidateExprCompare(attr, Ast.cmpop.Eq, expected, ValidationNotEqualError(attrTypeRef))
      case ValidationMin(min) =>
        attrValidateExprCompare(attr, Ast.cmpop.GtE, min, ValidationLessThanError(attrTypeRef))
      case ValidationMax(max) =>
        attrValidateExprCompare(attr, Ast.cmpop.LtE, max, ValidationGreaterThanError(attrTypeRef))
      case ValidationRange(min, max) =>
        attrValidateExprCompare(attr, Ast.cmpop.GtE, min, ValidationLessThanError(attrTypeRef))
        attrValidateExprCompare(attr, Ast.cmpop.LtE, max, ValidationGreaterThanError(attrTypeRef))
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
          err = ValidationNotAnyOfError(attrTypeRef),
          errArgs = List(
            itemValue,
            Ast.expr.InternalName(IoIdentifier),
            Ast.expr.Str(attr.path.mkString("/", "/", ""))
          )
        )
      case ValidationInEnum() =>
        attrValidateInEnum(
          attr,
          attr.dataType.asInstanceOf[EnumType],
          itemValue,
          ValidationNotInEnumError(attr.dataType),
          List(
            itemValue,
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
        handleAssignmentTempVar(
          attrTypeRef,
          translator.translate(Ast.expr.Name(Ast.identifier(Identifier.THIS))),
          translator.translate(itemValue)
        )
        attrValidateExpr(
          attr,
          expr,
          ValidationExprError(attrTypeRef),
          List(
            itemValue,
            Ast.expr.InternalName(IoIdentifier),
            Ast.expr.Str(attr.path.mkString("/", "/", ""))
          )
        )
        blockScopeFooter
    }
  }

  def attrValidateExprCompare(attr: AttrLikeSpec, op: Ast.cmpop, expected: Ast.expr, err: KSError): Unit = {
    val itemValue = Identifier.itemExpr(attr.id, attr.cond.repeat)
    attrValidateExpr(
      attr,
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

  def attrValidateExpr(attr: AttrLikeSpec, checkExpr: Ast.expr, err: KSError, errArgs: List[Ast.expr]): Unit = {}
  def attrValidateInEnum(attr: AttrLikeSpec, et: EnumType, valueExpr: Ast.expr, err: ValidationNotInEnumError, errArgs: List[Ast.expr]): Unit = {}
  def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit
  def blockScopeHeader: Unit
  def blockScopeFooter: Unit

}
