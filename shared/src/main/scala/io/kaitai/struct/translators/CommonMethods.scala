package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.Identifier
import io.kaitai.struct.precompile.{MethodNotFoundError, MethodNotFoundErrorWithArg, TypeMismatchError, WrongMethodCall}

sealed trait MethodArgType
object MethodArgType {
  case object IntArg extends MethodArgType {
    override def toString = "integer"
  }
  case object FloatArg extends MethodArgType {
    override def toString = "float"
  }
  case object StrArg extends MethodArgType {
    override def toString = "string"
  }
  case object BooleanArg extends MethodArgType {
    override def toString = "boolean"
  }
  case object BytesArg extends MethodArgType {
    override def toString = "byte array"
  }
  case object ArrayArg extends MethodArgType {
    override def toString = "array"
  }

  def byDataType(dataType: DataType): Option[MethodArgType] = {
    dataType match {
      case _: IntType => Some(IntArg)
      case _: FloatType => Some(FloatArg)
      case _: StrType => Some(StrArg)
      case _: BooleanType => Some(BooleanArg)
      case _: BytesType => Some(BytesArg)
      case _: ArrayType => Some(ArrayArg)
      case _ => None
    }
  }

  def isArgAcceptable(actualType: DataType, expectedType: MethodArgType): Boolean =
    byDataType(actualType).map((t) => t == expectedType).getOrElse(false)
}

abstract trait CommonMethods[T] extends TypeDetector {
  import MethodArgType._

  sealed trait MethodSig {
    def name: String
    def expectedArgs: String
    def accepts(argsValues: Seq[Ast.expr]): Boolean
  }

  case class MethodSig0(
    name: String,
    returnType: DataType,
    method: (Ast.expr) => T
  ) extends MethodSig {
    override def expectedArgs: String = "()"
    override def accepts(argsValues: Seq[Ast.expr]): Boolean = argsValues.isEmpty
  }

  case class MethodSig1(
    name: String,
    returnType: DataType,
    argTypes: MethodArgType,
    method: (Ast.expr, Ast.expr) => T
  ) extends MethodSig {
    override def expectedArgs: String = s"($argTypes)"
    override def accepts(argsValues: Seq[Ast.expr]): Boolean = argsValues match {
      case Seq(arg0) => isArgAcceptable(detectType(arg0), argTypes)
      case _ => false
    }
  }

  case class MethodSig2(
    name: String,
    returnType: DataType,
    argTypes: (MethodArgType, MethodArgType),
    method: (Ast.expr, Ast.expr, Ast.expr) => T
  ) extends MethodSig {
    override def expectedArgs: String = s"(${argTypes._1}, ${argTypes._2})"
    override def accepts(argsValues: Seq[Ast.expr]): Boolean = argsValues match {
      case Seq(arg0, arg1) =>
        isArgAcceptable(detectType(arg0), argTypes._1) &&
          isArgAcceptable(detectType(arg1), argTypes._2)
      case _ => false
    }
  }

  val METHODS_BY_TYPE: Map[MethodArgType, List[MethodSig]] = Map(
    BytesArg -> List(
      MethodSig0("first", Int1Type(false), bytesFirst),
      MethodSig0("last", Int1Type(false), bytesLast),
      MethodSig0("length", CalcIntType, bytesLength),
      MethodSig0("size", CalcIntType, bytesLength),
      MethodSig0("min", Int1Type(false), bytesMin),
      MethodSig0("max", Int1Type(false), bytesMax),

      // TODO: implement a better way to signal that we want not just any string, but string literal
      MethodSig1("to_s", CalcStrType, StrArg, { case (obj, arg0) =>
        arg0 match {
          case Ast.expr.Str(encoding) =>
            bytesToStr(obj, encoding)
          case x =>
            throw new TypeMismatchError(s"to_s: argument #0: expected string literal, got $x")
        }
      })
    ),
    IntArg -> List(
      MethodSig0("to_s", CalcStrType, intToStr),
    ),
    FloatArg -> List(
      MethodSig0("to_i", CalcIntType, floatToInt),
    ),
    StrArg -> List(
      MethodSig0("length", CalcIntType, strLength),
      MethodSig0("reverse", CalcStrType, strReverse),
      MethodSig0("to_i", CalcIntType, { strToInt(_, Ast.expr.IntNum(10)) }),
      MethodSig1("to_i", CalcIntType, IntArg, strToInt),
      MethodSig2("substring", CalcStrType, (IntArg, IntArg), strSubstring)
    ),
    BooleanArg -> List(
      MethodSig0("to_i", CalcBooleanType, boolToInt)
    ),

    // TODO: do something about return type for arrays here
    ArrayArg -> List(
      MethodSig0("first", AnyType, arrayFirst),
      MethodSig0("last", AnyType, arrayLast),
      MethodSig0("size", AnyType, arraySize),
      MethodSig0("min", AnyType, arrayMin),
      MethodSig0("max", AnyType, arrayMax),
    ),
  )

  /**
    * Constant for precedence to use from within method call when we need to make sure parenthesis
    * will appear when necessary. Mostly used when a language will use postfix method call on an
    * object, like `obj.method(args)`:
    *
    *  - if `obj` is simple (like variable reference or a literal - `obj.method(args)` or
    *    `42.method(args)`, no parenthesis are required.
    *  - if `obj` is complex (like an expression `a + b`), then this will ensure framing as
    *    `(a + b).method(args)`.
    */
  val METHOD_PRECEDENCE = 999

  /**
    * Translates a certain attribute call (as in `foo.bar`) into a rendition
    * of expression in certain target language.
    * @note Must be kept in sync with [[TypeDetector.detectAttributeType]]
    * @param call attribute call expression to translate
    * @return result of translation as [[T]]
    */
  def translateAttribute(call: Ast.expr.Attribute): T = {
    val attr = call.attr
    val value = call.value
    val valType = detectType(value)

    // Special case: will be compiled as compile-time determined constant
    if (attr.name == Identifier.SIZEOF)
      return byteSizeOfValue(value.toString, valType)

    valType match {
      case KaitaiStructType | CalcKaitaiStructType(_) =>
        attr.name match {
          case Identifier.PARENT | Identifier.IO => kaitaiStructField(value, attr.name)
        }
      case ut: UserType =>
        userTypeField(ut, value, attr.name)
      case KaitaiStreamType | OwnedKaitaiStreamType =>
        attr.name match {
          case "size" => kaitaiStreamSize(value)
          case "eof" => kaitaiStreamEof(value)
          case "pos" => kaitaiStreamPos(value)
        }
      case et: EnumType =>
        attr.name match {
          case "to_i" => enumToInt(value, et)
          case _ => throw new TypeMismatchError(s"called invalid attribute '${attr.name}' on expression of type $valType")
        }
      case _ =>
        MethodArgType.byDataType(valType) match {
          case Some(argType) => invokeMethod(argType, attr.name, value)
          case _ => throw new TypeMismatchError(s"internal compiler error: tried to call attribute '${attr.name}' on expression of type $valType")
        }
    }
  }

  /**
    * Translates a certain function call (as in `foo.bar(arg1, arg2)`) into a
    * rendition of expression in certain target language.
    *
    * @note Must be kept in sync with [[TypeDetector.detectCallType]]
    * @param call function call expression to translate
    * @return result of translation as [[T]]
    */
  def translateCall(call: Ast.expr.Call): T = {
    val func = call.func
    val args = call.args

    func match {
      case Ast.expr.Attribute(obj: Ast.expr, methodName: Ast.identifier) =>
        val objType = detectType(obj)
        MethodArgType.byDataType(objType) match {
          case Some(argType) =>
            invokeMethod(argType, methodName.name, obj, args)
          case None =>
            throw new MethodNotFoundError(methodName.name, objType)
        }
    }
  }

  private def invokeMethod(argType: MethodArgType, methodName: String, obj: Ast.expr, args: Seq[Ast.expr] = Seq()): T = {
    METHODS_BY_TYPE.get(argType) match {
      case Some(methodList) =>
        val methodSigs = methodList.filter(_.name == methodName)
        if (methodSigs.isEmpty) {
          throw new MethodNotFoundErrorWithArg(methodName, argType)
        } else {
          val expectedArgProblems: List[String] = methodSigs.map { methodSig =>
            if (methodSig.accepts(args)) {
              return methodSig match {
                case ms0: MethodSig0 =>
                  ms0.method(obj)
                case ms1: MethodSig1 =>
                  ms1.method(obj, args(0))
                case ms2: MethodSig2 =>
                  ms2.method(obj, args(0), args(1))
              }
            } else {
              methodSig.expectedArgs
            }
          }
          throw new WrongMethodCall(argType, methodName, expectedArgProblems, "(" + args.mkString(", ") + ")")
        }
      case None =>
        throw new MethodNotFoundErrorWithArg(methodName, argType)
    }
  }

  def userTypeField(ut: UserType, value: Ast.expr, name: String): T
  def kaitaiStructField(value: Ast.expr, name: String): T

  def bytesSubscript(container: Ast.expr, idx: Ast.expr): T
  def bytesFirst(b: Ast.expr): T
  def bytesLast(b: Ast.expr): T
  def bytesLength(b: Ast.expr): T
  def bytesMin(b: Ast.expr): T
  def bytesMax(b: Ast.expr): T

  def strLength(s: Ast.expr): T
  def strReverse(s: Ast.expr): T
  def strToInt(s: Ast.expr, base: Ast.expr): T
  def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): T

  def bytesToStr(value: Ast.expr, encoding: String): T

  def intToStr(value: Ast.expr): T

  def floatToInt(value: Ast.expr): T

  def kaitaiStreamSize(value: Ast.expr): T
  def kaitaiStreamEof(value: Ast.expr): T
  def kaitaiStreamPos(value: Ast.expr): T

  def arraySubscript(container: Ast.expr, idx: Ast.expr): T
  def arrayFirst(a: Ast.expr): T
  def arrayLast(a: Ast.expr): T
  def arraySize(a: Ast.expr): T
  def arrayMin(a: Ast.expr): T
  def arrayMax(a: Ast.expr): T

  def enumToInt(value: Ast.expr, et: EnumType): T

  def boolToInt(value: Ast.expr): T

  def byteSizeOfValue(attrName: String, valType: DataType): T
}
