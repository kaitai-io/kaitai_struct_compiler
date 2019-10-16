package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype.{DataType, KSError, ValidationNotEqualError}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{AttrSpec, Identifier, IoIdentifier, ValidationEq, ValidationSpec, YAMLParseException}

/**
  * Common interface for validation operations.
  */
trait ValidateOps extends ExceptionNames {
  def attrValidate(attrId: Identifier, attr: AttrSpec, valid: ValidationSpec): Unit = {
    valid match {
      case ValidationEq(expected) =>
        attrValidateExpr(
          attrId,
          attr.dataType,
          Ast.expr.Compare(
            Ast.expr.Name(attrId.toAstIdentifier),
            Ast.cmpop.Eq,
            expected
          ),
          ksErrorName(ValidationNotEqualError(attr.dataType)),
          List(
            expected,
            Ast.expr.Name(attrId.toAstIdentifier),
            Ast.expr.Name(IoIdentifier.toAstIdentifier),
            Ast.expr.Str(attr.path.mkString("/", "/", ""))
          )
        )
    }
  }

  def attrValidateExpr(attrId: Identifier, attrType: DataType, checkExpr: Ast.expr, errName: String, errArgs: List[Ast.expr]): Unit = {}
}
