package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.Identifier
import io.kaitai.struct.languages.JavaCompiler
import io.kaitai.struct.precompile.TypeMismatchError
import io.kaitai.struct.{StringLanguageOutputWriter, Utils}

sealed trait TranslatorResult
case class ResultString(s: String) extends TranslatorResult
case class ResultLocalVar(n: Int) extends TranslatorResult

class GoTranslator(out: StringLanguageOutputWriter, provider: TypeProvider)
  extends TypeDetector(provider)
  with AbstractTranslator
  with CommonLiterals
  with CommonOps {

  override def translate(v: Ast.expr): String = {
    translateExpr(v) match {
      case ResultString(s) => s
      case ResultLocalVar(n) => localVarName(n)
    }
  }

  def translateExpr(v: Ast.expr): TranslatorResult = {
    v match {
      case Ast.expr.IntNum(n) =>
        trIntLiteral(n)
      case Ast.expr.FloatNum(n) =>
        trFloatLiteral(n)
      case Ast.expr.Str(s) =>
        trStringLiteral(s)
      case Ast.expr.Bool(n) =>
        trBoolLiteral(n)

//      case Ast.expr.BoolOp(op, values) =>
      case Ast.expr.BinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) =>
        (detectType(left), detectType(right), op) match {
          case (_: NumericType, _: NumericType, _) =>
            trNumericBinOp(left, op, right)
          case (_: StrType, _: StrType, Ast.operator.Add) =>
            trStrConcat(left, right)
          case (ltype, rtype, _) =>
            throw new TypeMismatchError(s"can't do $ltype $op $rtype")
        }
//      case Ast.expr.UnaryOp(op, operand) =>
//      case Ast.expr.IfExp(condition, ifTrue, ifFalse) =>
//      case Ast.expr.Compare(left, ops, right) =>
//      case Ast.expr.Call(func, args) =>
//      case Ast.expr.EnumByLabel(enumName, label) =>
//      case Ast.expr.EnumById(enumName, id) =>
//      case Ast.expr.Attribute(value, attr) =>
//      case Ast.expr.CastToType(value, typeName) =>
//      case Ast.expr.Subscript(value, idx) =>
      case Ast.expr.Name(name: Ast.identifier) =>
        trName(name.name)
//      case Ast.expr.List(elts) =>
    }
  }

  def trIntLiteral(n: BigInt): TranslatorResult = ResultString(doIntLiteral(n))
  def trFloatLiteral(n: BigDecimal): TranslatorResult = ResultString(doFloatLiteral(n))
  def trStringLiteral(s: String): TranslatorResult = ResultString(doStringLiteral(s))
  def trBoolLiteral(n: Boolean): TranslatorResult = ResultString(doBoolLiteral(n))

  def trNumericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) =
    ResultString(numericBinOp(left, op, right))

  def trStrConcat(left: Ast.expr, right: Ast.expr): TranslatorResult =
    ResultString(translate(left) + " + " + translate(right))

//  override def doArrayLiteral(t: DataType, value: Seq[Ast.expr]): String = {
//    val javaType = JavaCompiler.kaitaiType2JavaTypeBoxed(t)
//    val commaStr = value.map((v) => translate(v)).mkString(", ")
//    s"new ArrayList<$javaType>(Arrays.asList($commaStr))"
//  }
//
//  override def doByteArrayLiteral(arr: Seq[Byte]): String =
//    s"new byte[] { ${arr.mkString(", ")} }"

  override def numericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Mod) =>
        s"${JavaCompiler.kstreamName}.mod(${translate(left)}, ${translate(right)})"
      case _ =>
        super.numericBinOp(left, op, right)
    }
  }

  def trName(s: String): TranslatorResult = {
    s match {
      case Identifier.ROOT |
           Identifier.PARENT |
           Identifier.IO |
           Identifier.ITERATOR |
           Identifier.ITERATOR2 =>
        ResultString(specialName(s))
      case _ =>
        if (provider.isLazy(s)) {
          val v1 = allocateLocalVar()
          out.puts(s"${localVarName(v1)}, err := this.${Utils.upperCamelCase(s)}()")
          outAddErrCheck()
          ResultLocalVar(v1)
        } else {
          ResultString(s"this.${Utils.upperCamelCase(s)}")
        }
    }
  }

  def specialName(id: String): String = id match {
    case Identifier.ROOT | Identifier.PARENT | Identifier.IO =>
      s"this.$id"
    case Identifier.ITERATOR =>
      "_it"
    case Identifier.ITERATOR2 =>
      "_buf"
  }

//  override def doEnumByLabel(enumTypeAbs: List[String], label: String): String =
//    s"${enumClass(enumTypeAbs)}.${label.toUpperCase}"
//  override def doEnumById(enumTypeAbs: List[String], id: String): String =
//    s"${enumClass(enumTypeAbs)}.byId($id)"

  def enumClass(enumTypeAbs: List[String]): String = {
    val enumTypeRel = Utils.relClass(enumTypeAbs, provider.nowClass.name)
    enumTypeRel.map((x) => Utils.upperCamelCase(x)).mkString(".")
  }

  override def doStrCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr) = {
    if (op == Ast.cmpop.Eq) {
      s"${translate(left)}.equals(${translate(right)})"
    } else if (op == Ast.cmpop.NotEq) {
      s"!(${translate(left)}).equals(${translate(right)})"
    } else {
      s"(${translate(left)}.compareTo(${translate(right)}) ${cmpOp(op)} 0)"
    }
  }

  override def doBytesCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): String = {
    op match {
      case Ast.cmpop.Eq =>
        s"Arrays.equals(${translate(left)}, ${translate(right)})"
      case Ast.cmpop.NotEq =>
        s"!Arrays.equals(${translate(left)}, ${translate(right)})"
      case _ =>
        s"(${JavaCompiler.kstreamName}.byteArrayCompare(${translate(left)}, ${translate(right)}) ${cmpOp(op)} 0)"
    }
  }

//  override def doSubscript(container: Ast.expr, idx: Ast.expr): String =
//    s"${translate(container)}.get((int) ${translate(idx)})"
//  override def doIfExp(condition: Ast.expr, ifTrue: Ast.expr, ifFalse: Ast.expr): String =
//    s"(${translate(condition)} ? ${translate(ifTrue)} : ${translate(ifFalse)})"
//  override def doCast(value: Ast.expr, typeName: String): String =
//    s"((${Utils.upperCamelCase(typeName)}) (${translate(value)}))"

  // Predefined methods of various types
//  override def strToInt(s: Ast.expr, base: Ast.expr): String =
//    s"Long.parseLong(${translate(s)}, ${translate(base)})"
//  override def enumToInt(v: Ast.expr, et: EnumType): String =
//    s"${translate(v)}.id()"
//  override def intToStr(i: Ast.expr, base: Ast.expr): String =
//    s"Long.toString(${translate(i)}, ${translate(base)})"
//  override def bytesToStr(bytesExpr: String, encoding: Ast.expr): String =
//    s"new String($bytesExpr, Charset.forName(${translate(encoding)}))"
//  override def strLength(s: Ast.expr): String =
//    s"${translate(s)}.length()"
//  override def strReverse(s: Ast.expr): String =
//    s"new StringBuilder(${translate(s)}).reverse().toString()"
//  override def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): String =
//    s"${translate(s)}.substring(${translate(from)}, ${translate(to)})"

//  override def arrayFirst(a: Ast.expr): String =
//    s"${translate(a)}.get(0)"
//  override def arrayLast(a: Ast.expr): String = {
//    val v = translate(a)
//    s"$v.get($v.size() - 1)"
//  }
//  override def arraySize(a: Ast.expr): String =
//    s"${translate(a)}.size()"
//  override def arrayMin(a: Ast.expr): String =
//    s"Collections.min(${translate(a)})"
//  override def arrayMax(a: Ast.expr): String =
//    s"Collections.max(${translate(a)})"

  private
  var localVarNum = 0

  def allocateLocalVar(): Int = {
    localVarNum += 1
    localVarNum
  }

  def localVarName(n: Int) = s"tmp$n"

  def outAddErrCheck() {
    out.puts("if err != nil {")
    out.inc
    out.puts("return 0, err")
    out.dec
    out.puts("}")
  }
}
