package io.kaitai.struct.translators

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.DataType.IntType
import io.kaitai.struct.languages.PythonCompiler
import io.kaitai.struct.translators.components.CTernaryOperator

class PythonTranslator(provider: TypeProvider) extends BaseTranslator(provider) {
  override def numericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Div) =>
        val prio = binOpPriority(Ast.operator.Div)
        (s"${translate(left, prio)} // ${translate(right, prio)}", prio)
      case _ =>
        super.numericBinOp(left, op, right)
    }
  }

  override def doStringLiteral(s: String): (String, Int) = ("u\"" + s + "\"", 0)
  override def doBoolLiteral(n: Boolean): (String, Int) = (if (n) "True" else "False", 0)

  override def doByteArrayLiteral(arr: Seq[Byte]): (String, Int) =
    (s"struct.pack('${arr.length}b', ${arr.mkString(", ")})", 0)

  override def doLocalName(s: String) = {
    s match {
      case "_" => s
      case _ => s"self.${doName(s)}"
    }
  }
  override def doName(s: String) = s

  override def doEnumByLabel(enumTypeAbs: List[String], label: String) =
    (s"${PythonCompiler.types2class(enumTypeAbs)}.$label", 0)
  override def doEnumById(enumTypeAbs: List[String], id: String) =
    (s"${PythonCompiler.types2class(enumTypeAbs)}($id)", 0)

  override def booleanOp(op: Ast.boolop) = op match {
    case Ast.boolop.Or => "or"
    case Ast.boolop.And => "and"
  }

  override def unaryOp(op: Ast.unaryop) = op match {
    case Ast.unaryop.Not => "not "
    case _ => super.unaryOp(op)
  }

  override def doSubscript(container: Ast.expr, idx: Ast.expr): String =
    s"${translate(container)}[${translate(idx)}]"
  override def doIfExp(condition: Ast.expr, ifTrue: Ast.expr, ifFalse: Ast.expr): (String, Int) = {
    val prio = CTernaryOperator.IF_EXP_PRIO
    (s"${translate(ifTrue, prio)} if ${translate(condition, prio)} else ${translate(ifFalse, prio)}", prio)
  }

  // Predefined methods of various types
  override def strToInt(s: Ast.expr, base: Ast.expr): String = {
    val baseStr = translate(base, 0)
    val add = baseStr match {
      case "10" => ""
      case _ => s", $baseStr"
    }
    s"int(${translate(s)}$add)"
  }
  override def strLength(value: Ast.expr): String =
    s"len(${translate(value)})"
  override def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): String =
    s"${translate(s)}[${translate(from)}:${translate(to)}]"

  override def arrayFirst(a: Ast.expr): String =
    s"${translate(a)}[0]"
  override def arrayLast(a: Ast.expr): String =
    s"${translate(a)}[-1]"

  override def kaitaiStreamSize(value: Ast.expr): String =
    s"${translate(value)}.size()"
  override def kaitaiStreamEof(value: Ast.expr): String =
    s"${translate(value)}.is_eof()"
  override def kaitaiStreamPos(value: Ast.expr): String =
    s"${translate(value)}.pos()"
}
