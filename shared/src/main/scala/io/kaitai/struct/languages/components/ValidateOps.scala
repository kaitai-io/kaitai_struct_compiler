package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._

/**
  * Common interface for validation operations.
  */
trait ValidateOps extends ExceptionNames {
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
              Ast.expr.Name(attrId.toAstIdentifier),
              Ast.cmpop.Eq,
              expected
            )
          )
        )

        attrValidateExpr(
          attrId,
          attr.dataTypeComposite,
          checkExpr = bigOrExpr,
          errName = ksErrorName(ValidationNotAnyOfError(attr.dataTypeComposite)),
          errArgs = List(
            Ast.expr.Name(attrId.toAstIdentifier),
            Ast.expr.Name(IoIdentifier.toAstIdentifier),
            Ast.expr.Str(attr.path.mkString("/", "/", ""))
          )
        )
    }
  }

  def attrValidateExprCompare(attrId: Identifier, attr: AttrLikeSpec, op: Ast.cmpop, expected: Ast.expr, err: KSError): Unit = {
    attrValidateExpr(
      attrId,
      attr.dataTypeComposite,
      checkExpr = Ast.expr.Compare(
        Ast.expr.Name(attrId.toAstIdentifier),
        op,
        expected
      ),
      errName = ksErrorName(err),
      errArgs = List(
        expected,
        Ast.expr.Name(attrId.toAstIdentifier),
        Ast.expr.Name(IoIdentifier.toAstIdentifier),
        Ast.expr.Str(attr.path.mkString("/", "/", ""))
      )
    )
  }

  def attrValidateExpr(attrId: Identifier, attrType: DataType, checkExpr: Ast.expr, errName: String, errArgs: List[Ast.expr]): Unit = {}
}
