package io.kaitai.struct.translators

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.exprlang.DataType._

trait TypeProvider {
  def determineType(name: String): BaseType
}

class TypeMismatchError(msg: String) extends RuntimeException(msg)

abstract class BaseTranslator(val provider: TypeProvider) {

  def translate(v: Ast.expr): String = {
    v match {
      case Ast.expr.Num(n) =>
        doIntLiteral(n)
      case Ast.expr.Str(s) =>
        doStringLiteral(s)
      case Ast.expr.Name(name: Ast.identifier) =>
        doName(name.name)
      case Ast.expr.UnaryOp(op: Ast.unaryop, v: Ast.expr) =>
        s"${unaryOp(op)}${translate(v)}"
      case Ast.expr.Compare(left: Ast.expr, op: Ast.cmpop, right: Ast.expr) =>
        val ltype = detectType(left)
        val rtype = detectType(right)
        if (ltype == IntType && rtype == IntType) {
          doIntCompareOp(left, op, right)
        } else if (ltype == StrType && rtype == StrType) {
          doStrCompareOp(left, op, right)
        } else {
          throw new RuntimeException(s"can't compare ${ltype} and ${rtype}")
        }
      case Ast.expr.BinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) =>
        val ltype = detectType(left)
        val rtype = detectType(right)
        if (ltype == IntType && rtype == IntType) {
          intBinOp(left, op, right)
        } else if (ltype == StrType && rtype == StrType && op == Ast.operator.Add) {
          strConcat(left, right)
        } else {
          throw new RuntimeException(s"can't do ${ltype} ${op} ${rtype}")
        }
      case Ast.expr.BoolOp(op: Ast.boolop, values: Seq[Ast.expr]) =>
        doBooleanOp(op, values)
      case Ast.expr.Subscript(container: Ast.expr, idx: Ast.expr) =>
        doSubscript(container, idx)
      case Ast.expr.Attribute(value: Ast.expr, attr: Ast.identifier) =>
        val valType = detectType(value)
        valType match {
          case StrType =>
            attr.name match {
              case "length" => strLength(value)
            }
          case IntType =>
            throw new RuntimeException(s"don't know how to call anything on ${valType}")
        }
      case Ast.expr.Call(func: Ast.expr, args: Seq[Ast.expr]) =>
        func match {
          case Ast.expr.Attribute(obj: Ast.expr, methodName: Ast.identifier) =>
            val objType = detectType(obj)
            (objType, methodName.name) match {
              case (StrType, "substring") => strSubstring(obj, args(0), args(1))
              case _ => throw new RuntimeException(s"don't know how to call method '$methodName' of object type '$objType'")
            }
        }
    }
  }

  def intBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) = {
    s"(${translate(left)} ${binOp(op)} ${translate(right)})"
  }

  def binOp(op: Ast.operator): String = {
    op match {
      case Ast.operator.Add => "+"
      case Ast.operator.Sub => "-"
      case Ast.operator.Mult => "*"
      case Ast.operator.Div => "/"
    }
  }

  def doIntCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr) = {
    s"${translate(left)} ${cmpOp(op)} ${translate(right)}"
  }

  def doStrCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr) = {
    s"${translate(left)} ${cmpOp(op)} ${translate(right)}"
  }

  def cmpOp(op: Ast.cmpop): String = {
    op match {
      case Ast.cmpop.Lt => "<"
      case Ast.cmpop.LtE => "<="
      case Ast.cmpop.Gt => ">"
      case Ast.cmpop.GtE => ">="
      case Ast.cmpop.Eq => "=="
      case Ast.cmpop.NotEq => "!="
    }
  }

  def doBooleanOp(op: Ast.boolop, values: Seq[Ast.expr]): String = {
    val opStr = s" ${booleanOp(op)} "
    values.map(translate).mkString(opStr)
  }

  def booleanOp(op: Ast.boolop) = op match {
    case Ast.boolop.Or => "||"
    case Ast.boolop.Or => "&&"
  }

  def unaryOp(op: Ast.unaryop) = op match {
    case Ast.unaryop.Invert => "~"
    case Ast.unaryop.Minus => "-"
    case Ast.unaryop.Not => "!"
  }

  def doSubscript(container: expr, idx: expr): String

  // Literals
  def doIntLiteral(n: Any): String = n.toString
  def doStringLiteral(s: String): String = "\"" + s + "\""

  def doName(s: String): String

  // Predefined methods of various types
  def strConcat(left: Ast.expr, right: Ast.expr): String = s"${translate(left)} + ${translate(right)}"
  def strToInt(s: Ast.expr, base: Ast.expr): String
  def strLength(s: Ast.expr): String
  def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): String

  def detectType(v: Ast.expr): BaseType = {
    v match {
      case Ast.expr.Num(_) => IntType
      case Ast.expr.Str(_) => StrType
      case Ast.expr.Name(name: Ast.identifier) => provider.determineType(name.name)
      case Ast.expr.UnaryOp(op: Ast.unaryop, v: Ast.expr) =>
        val t = detectType(v)
        t match {
          case IntType => IntType
          case _ => throw new RuntimeException(s"unable to apply unary operator ${op} to ${t}")
        }
      case Ast.expr.Compare(left: Ast.expr, op: Ast.cmpop, right: Ast.expr) =>
        val ltype = detectType(left)
        val rtype = detectType(right)
        if (ltype == rtype) {
          BooleanType
        } else {
          throw new RuntimeException(s"can't compare ${ltype} and ${rtype}")
        }
      case Ast.expr.BinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) =>
        val ltype = detectType(left)
        val rtype = detectType(right)
        if (ltype == rtype) {
          ltype
        } else {
          throw new RuntimeException(s"can't apply operator ${op} to ${ltype} and ${rtype}")
        }
      case Ast.expr.BoolOp(op: Ast.boolop, values: Seq[Ast.expr]) =>
        values.foreach(v => {
          val t = detectType(v)
          if (t != BooleanType) {
            throw new RuntimeException(s"unable to use ${t} argument in ${op} boolean expression")
          }
        })
        BooleanType
      case Ast.expr.Subscript(container: Ast.expr, idx: Ast.expr) =>
        detectType(container) match {
          case ArrayType(elType: BaseType) =>
            detectType(idx) match {
              case IntType => elType
              case idxType => throw new TypeMismatchError(s"unable to index an array using ${idxType}")
            }
          case cntType => throw new TypeMismatchError(s"unable to apply operation [] to ${cntType}")
        }
      case Ast.expr.Attribute(value: Ast.expr, attr: Ast.identifier) =>
        val valType = detectType(value)
        valType match {
          case StrType =>
            attr.name match {
              case "length" => IntType
              case _ => throw new RuntimeException(s"called invalid attribute '${attr.name}' on expression of type ${valType}")
            }
          case IntType =>
            throw new RuntimeException(s"don't know how to call anything on ${valType}")
        }
      case Ast.expr.Call(func: Ast.expr, args: Seq[Ast.expr]) =>
        func match {
          case Ast.expr.Attribute(obj: Ast.expr, methodName: Ast.identifier) =>
            val objType = detectType(obj)
            (objType, methodName.name) match {
              case (StrType, "substring") => StrType
              case _ => throw new RuntimeException(s"don't know how to call method '$methodName' of object type '$objType'")
            }
        }
    }
  }
}
