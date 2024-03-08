package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType.{ArrayType, BytesType, IntType}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.Identifier
import io.kaitai.struct.precompile.{EnumMemberNotFoundError, TypeMismatchError}

/**
  * Validates expressions usage of types (in typecasting operator,
  * enums, sizeof operator, etc) to match actual layout of class specs
  * loaded in type provider.
  *
  * Implemented essentially as a no-op translator, which does all the
  * recursive traversing that regular translator performs, but outputs
  * no result.
  *
  * @param provider TypeProvider that will answer queries on user types
  */
class ExpressionValidator(val provider: TypeProvider)
  extends TypeDetector(provider)
  with CommonMethods[Unit]
  with ByteArraysAsTrueArrays[Unit] {
  /**
    * Validates one expression. Throws exceptions when encounters an error.
    * @param v expression to validate
    */
  def validate(v: Ast.expr): Unit = {
    v match {
      case _: Ast.expr.IntNum |
           _: Ast.expr.FloatNum |
           _: Ast.expr.Str |
           _: Ast.expr.Bool => // all simple literals are good and valid
      case Ast.expr.EnumById(enumType, id, inType) =>
        provider.resolveEnum(inType, enumType.name)
        validate(id)
      case Ast.expr.EnumByLabel(enumType, label, inType) =>
        val enumSpec = provider.resolveEnum(inType, enumType.name)
        if (!enumSpec.map.values.exists(_.name == label.name)) {
          throw new EnumMemberNotFoundError(label.name, enumType.name, enumSpec.path.mkString("/"))
        }
      case Ast.expr.Name(name: Ast.identifier) =>
        if (name.name == Identifier.SIZEOF) {
          CommonSizeOf.getByteSizeOfClassSpec(provider.nowClass)
        } else {
          // local name already checked by type detection
        }
      case Ast.expr.UnaryOp(op: Ast.unaryop, inner: Ast.expr) =>
        validate(inner)
      case Ast.expr.Compare(left: Ast.expr, op: Ast.cmpop, right: Ast.expr) =>
        validate(left)
        validate(right)
      case Ast.expr.BinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) =>
        validate(left)
        validate(right)
      case Ast.expr.BoolOp(op: Ast.boolop, values: Seq[Ast.expr]) =>
        values.foreach(validate)
      case Ast.expr.IfExp(condition, ifTrue, ifFalse) =>
        validate(condition)
        validate(ifTrue)
        validate(ifFalse)
      case Ast.expr.Subscript(container: Ast.expr, idx: Ast.expr) =>
        detectType(container) match {
          case _: ArrayType | _: BytesType =>
            validate(container)
            detectType(idx) match {
              case _: IntType =>
                validate(idx)
              case indexType =>
                throw new TypeMismatchError(s"subscript operation on arrays require index to be integer, but found $indexType")
            }
          case x =>
            throw new TypeMismatchError(s"subscript operation is not supported on object type $x")
        }
      case call: Ast.expr.Attribute =>
        translateAttribute(call)
      case call: Ast.expr.Call =>
        translateCall(call)
      case Ast.expr.List(values: Seq[Ast.expr]) =>
        values.foreach(validate)
      case ctt: Ast.expr.CastToType =>
        validate(ctt.value)
        detectCastType(ctt.typeName)
      case Ast.expr.ByteSizeOfType(typeName) =>
        CommonSizeOf.getBitsSizeOfType(typeName.nameAsStr, detectCastType(typeName))
      case Ast.expr.BitSizeOfType(typeName) =>
        CommonSizeOf.getBitsSizeOfType(typeName.nameAsStr, detectCastType(typeName))
      case Ast.expr.InterpolatedStr(elts: Seq[Ast.expr]) =>
        elts.foreach(validate)
    }
  }

  override def userTypeField(ut: DataType.UserType, value: Ast.expr, name: String): Unit = {
    validate(value)
    // TODO: check that field exists
  }
  override def kaitaiStructField(value: Ast.expr, name: String): Unit = {
    validate(value)
    // TODO: check that field exists
  }

  override def strLength(s: Ast.expr): Unit = validate(s)
  override def strReverse(s: Ast.expr): Unit = validate(s)
  override def strToInt(s: Ast.expr, base: Ast.expr): Unit = {
    validate(s)
    validate(base)
  }
  override def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): Unit = {
    validate(s)
    validate(from)
    validate(to)
  }

  override def bytesToStr(value: Ast.expr, encoding: String): Unit = {
    validate(value)
  }

  override def intToStr(value: Ast.expr): Unit = {
    validate(value)
  }

  override def floatToInt(value: Ast.expr): Unit = validate(value)

  override def kaitaiStreamSize(value: Ast.expr): Unit = validate(value)
  override def kaitaiStreamEof(value: Ast.expr): Unit = validate(value)
  override def kaitaiStreamPos(value: Ast.expr): Unit = validate(value)

  override def arraySubscript(container: Ast.expr, idx: Ast.expr): Unit = {
    validate(container)
    validate(idx)
  }
  override def arrayFirst(a: Ast.expr): Unit = validate(a)
  override def arrayLast(a: Ast.expr): Unit = validate(a)
  override def arraySize(a: Ast.expr): Unit = validate(a)
  override def arrayMin(a: Ast.expr): Unit = validate(a)
  override def arrayMax(a: Ast.expr): Unit = validate(a)

  override def enumToInt(value: Ast.expr, et: DataType.EnumType): Unit = validate(value)

  override def boolToInt(value: Ast.expr): Unit = validate(value)

  override def byteSizeOfValue(attrName: String, valType: DataType): Unit =
    CommonSizeOf.getBitsSizeOfType(attrName, valType)
}
