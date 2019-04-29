package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{ClassSpec, Identifier}
import io.kaitai.struct.precompile.TypeMismatchError

/**
  * BaseTranslator is a common semi-abstract implementation of a translator
  * API (i.e. [[AbstractTranslator]]), which fits target languages that
  * follow "every KS expression is translatable into expression" paradigm.
  * Main [[AbstractTranslator.translate]] method is implemented as a huge
  * case matching, which usually just calls relevant abstract methods for
  * every particular piece of KS expression, i.e. literals, operations,
  * method calls, etc.
  *
  * Given that there are many of these abstract methods, to make it more
  * maintainable, they are grouped into several abstract traits:
  * [[CommonLiterals]], [[CommonOps]], [[CommonMethods]],
  * [[CommonArraysAndCast]].
  *
  * This translator implementation also handles user-defined types and
  * fields properly - it uses given [[TypeProvider]] to resolve these.
  *
  * @param provider TypeProvider that will answer queries on user types
  */
abstract class BaseTranslator(val provider: TypeProvider)
  extends TypeDetector(provider)
  with AbstractTranslator
  with CommonLiterals
  with CommonOps
  with CommonArraysAndCast[String]
  with CommonMethods[String] {

  /**
    * Translates KS expression into an expression in some target language.
    * Note that this implementation may throw errors subclassed off the
    * [[io.kaitai.struct.precompile.ExpressionError]] when encountering
    * some sort of logical error in expression (i.e. invalid usage of
    * operator, type mismatch, etc). Typically, one's supposed to catch
    * and rethrow it, wrapped in [[io.kaitai.struct.precompile.ErrorInInput]]
    * to assist error reporting in KSC.
    *
    * @param v KS expression to translate
    * @return expression in target language as string
    */
  def translate(v: Ast.expr): String = {
    v match {
      case Ast.expr.IntNum(n) =>
        doIntLiteral(n)
      case Ast.expr.FloatNum(n) =>
        doFloatLiteral(n)
      case Ast.expr.Str(s) =>
        doStringLiteral(s)
      case Ast.expr.Bool(n) =>
        doBoolLiteral(n)
      case Ast.expr.EnumById(enumType, id, inType) =>
        val enumSpec = provider.resolveEnum(inType, enumType.name)
        doEnumById(enumSpec.name, translate(id))
      case Ast.expr.EnumByLabel(enumType, label, inType) =>
        val enumSpec = provider.resolveEnum(inType, enumType.name)
        doEnumByLabel(enumSpec.name, label.name)
      case Ast.expr.Name(name: Ast.identifier) =>
        if (name.name == Identifier.SIZEOF) {
          byteSizeOfClassSpec(provider.nowClass)
        } else {
          doLocalName(name.name)
        }
      case Ast.expr.UnaryOp(op: Ast.unaryop, inner: Ast.expr) =>
        unaryOp(op) + (inner match {
          case Ast.expr.IntNum(_) | Ast.expr.FloatNum(_) =>
            translate(inner)
          case _ =>
            s"(${translate(inner)})"
        })
      case Ast.expr.Compare(left: Ast.expr, op: Ast.cmpop, right: Ast.expr) =>
        (detectType(left), detectType(right)) match {
          case (_: NumericType, _: NumericType) =>
            doNumericCompareOp(left, op, right)
          case (_: BooleanType, _: BooleanType) =>
            op match {
              case Ast.cmpop.Eq | Ast.cmpop.NotEq =>
                // FIXME: probably for some languages we'll need non-numeric comparison
                doNumericCompareOp(left, op, right)
              case _ =>
                throw new TypeMismatchError(s"can't compare booleans using $op operator")
            }
          case (_: StrType, _: StrType) =>
            doStrCompareOp(left, op, right)
          case (_: BytesType, _: BytesType) =>
            doBytesCompareOp(left, op, right)
          case (et1: EnumType, et2: EnumType) =>
            val et1Spec = et1.enumSpec.get
            val et2Spec = et2.enumSpec.get
            if (et1Spec != et2Spec) {
              throw new TypeMismatchError(s"can't compare enums type ${et1Spec.nameAsStr} and ${et2Spec.nameAsStr}")
            } else {
              doEnumCompareOp(left, op, right)
            }
          case (ltype, rtype) =>
            throw new TypeMismatchError(s"can't compare $ltype and $rtype")
        }
      case Ast.expr.BinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) =>
        (detectType(left), detectType(right), op) match {
          case (_: NumericType, _: NumericType, _) =>
            numericBinOp(left, op, right)
          case (_: StrType, _: StrType, Ast.operator.Add) =>
            strConcat(left, right)
          case (ltype, rtype, _) =>
            throw new TypeMismatchError(s"can't do $ltype $op $rtype")
        }
      case Ast.expr.BoolOp(op: Ast.boolop, values: Seq[Ast.expr]) =>
        doBooleanOp(op, values)
      case Ast.expr.IfExp(condition, ifTrue, ifFalse) =>
        doIfExp(condition, ifTrue, ifFalse)
      case Ast.expr.Subscript(container: Ast.expr, idx: Ast.expr) =>
        detectType(idx) match {
          case _: IntType =>
            doSubscript(container, idx)
          case idxType =>
            throw new TypeMismatchError(s"can't use $idx as array index (need int, got $idxType)")
        }
      case call: Ast.expr.Attribute =>
        translateAttribute(call)
      case call: Ast.expr.Call =>
        translateCall(call)
      case Ast.expr.List(values: Seq[Ast.expr]) =>
        doGuessArrayLiteral(values)
      case ctt: Ast.expr.CastToType =>
        doCastOrArray(ctt)
      case Ast.expr.ByteSizeOfType(typeName) =>
        doByteSizeOfType(typeName)
      case Ast.expr.BitSizeOfType(typeName) =>
        doBitSizeOfType(typeName)
    }
  }

  def doSubscript(container: Ast.expr, idx: Ast.expr): String
  def doIfExp(condition: Ast.expr, ifTrue: Ast.expr, ifFalse: Ast.expr): String
  def doCast(value: Ast.expr, typeName: DataType): String = translate(value)
  def doByteSizeOfType(typeName: Ast.typeId): String = doIntLiteral(
    CommonSizeOf.bitToByteSize(
      CommonSizeOf.getBitsSizeOfType(
        typeName.nameAsStr, detectCastType(typeName)
      )
    )
  )
  def doBitSizeOfType(typeName: Ast.typeId): String = doIntLiteral(
    CommonSizeOf.getBitsSizeOfType(
      typeName.nameAsStr, detectCastType(typeName)
    )
  )
  def byteSizeOfValue(attrName: String, valType: DataType): String = doIntLiteral(
    CommonSizeOf.bitToByteSize(
      CommonSizeOf.getBitsSizeOfType(attrName, valType)
    )
  )
  def byteSizeOfClassSpec(cs: ClassSpec): String =
    doIntLiteral(CommonSizeOf.getByteSizeOfClassSpec(cs))

  def doArrayLiteral(t: DataType, value: Seq[Ast.expr]): String = "[" + value.map((v) => translate(v)).mkString(", ") + "]"
  def doByteArrayLiteral(arr: Seq[Byte]): String = "[" + arr.map(_ & 0xff).mkString(", ") + "]"
  def doByteArrayNonLiteral(elts: Seq[Ast.expr]): String = ???

  def doLocalName(s: String): String = doName(s)
  def doName(s: String): String
  def userTypeField(userType: UserType, value: Ast.expr, attrName: String): String =
    anyField(value, attrName)

  def doEnumByLabel(enumTypeAbs: List[String], label: String): String
  def doEnumById(enumTypeAbs: List[String], id: String): String

  // Predefined methods of various types
  def strConcat(left: Ast.expr, right: Ast.expr): String = s"${translate(left)} + ${translate(right)}"
  def boolToInt(value: Ast.expr): String =
    doIfExp(value, Ast.expr.IntNum(1), Ast.expr.IntNum(0))

  def kaitaiStreamSize(value: Ast.expr): String = anyField(value, "size")
  def kaitaiStreamEof(value: Ast.expr): String = anyField(value, "is_eof")
  def kaitaiStreamPos(value: Ast.expr): String = anyField(value, "pos")

  // Special convenience definition method + helper
  override def bytesToStr(value: Ast.expr, expr: Ast.expr): String =
    bytesToStr(translate(value), expr)
  def bytesToStr(value: String, expr: Ast.expr): String

  // Helper that does simple "one size fits all" attribute calling, if it is useful
  // for the language
  def anyField(value: Ast.expr, attrName: String): String =
    s"${translate(value)}.${doName(attrName)}"
}
