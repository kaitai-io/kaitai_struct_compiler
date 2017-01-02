package io.kaitai.struct.translators

import io.kaitai.struct.Utils
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.languages.CppCompiler
import io.kaitai.struct.translators.components.CTernaryOperator

class CppTranslator(provider: TypeProvider) extends BaseTranslator(provider) with CTernaryOperator {
  // TODO: add string escaping
  override def doStringLiteral(s: String): (String, Int) = ("std::string(\"" + s + "\")", 0)

  override def doArrayLiteral(t: BaseType, values: Seq[expr]): (String, Int) =
    throw new RuntimeException("C++ literal arrays are not implemented yet")

  override def doByteArrayLiteral(arr: Seq[Byte]): (String, Int) =
    ("std::string(\"" + Utils.hexEscapeByteArray(arr) + "\", " + arr.length + ")", 0)

  override def numericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Mod) =>
        (s"${CppCompiler.kstreamName}::mod(${translate(left, 0)}, ${translate(right, 0)})", 0)
      case _ =>
        super.numericBinOp(left, op, right)
    }
  }

  override def userTypeField(value: expr, attrName: String): String =
    s"${translate(value)}->${doName(attrName)}"

  override def doName(s: String) = s match {
    case "_" => s
    case _ => s"$s()"
  }

  override def doEnumByLabel(enumType: List[String], label: String): (String, Int) =
    ((enumType.last + "_" + label).toUpperCase, 0)
  override def doEnumById(enumType: List[String], id: String): (String, Int) =
    (s"static_cast<${CppCompiler.type2class(enumType)}>($id)", 0)

  override def doStrCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr) = {
    op match {
      case Ast.cmpop.Eq | Ast.cmpop.NotEq =>
        super.doStrCompareOp(left, op, right)
      case _ =>
        (
          s"${translate(left, 0)}.compare(${translate(right, 0)}) ${cmpOp(op)} 0",
          cmpOpPriority(op)
        )
    }
  }

  override def doSubscript(container: expr, idx: expr): String =
    s"${translate(container)}->at(${translate(idx)})"

  // Predefined methods of various types
  override def strToInt(s: expr, base: expr): String = {
    val baseStr = translate(base, 0)
    s"std::stoi(${translate(s)}" + (baseStr match {
      case "10" => ""
      case _ => s", 0, $baseStr"
    }) + ")"
  }
  override def strLength(s: expr): String =
    s"${translate(s)}.length()"
  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"${translate(s)}.substr(${translate(from)}, (${translate(to)}) - (${translate(from)}))"

  override def arrayFirst(a: expr): String =
    s"${translate(a)}->front()"
  override def arrayLast(a: expr): String =
    s"${translate(a)}->back()"
}
