package io.kaitai.struct

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.translators.{TypeDetector, TypeMismatchError}

/**
  * Validates all expressions used inside the given ClassSpec to use expected types.
  * @param topClass class to start check with
  */
class TypeValidator(topClass: ClassSpec) {
  val provider = new ClassTypeProvider(topClass)
  val detector = new TypeDetector(provider)

  /**
    * Starts the check from top-level class.
    */
  def validate(): Unit =
    validateClass(topClass, List())

  /**
    * Performs validation of a single ClassSpec: would validate
    * sequence attributes (`seq`), instances (`instances`) and all
    * nested subtypes (`types`) recursively. `doc` and `enums` are
    * not checked, as they contain no expressions.
    * @param curClass class to check
    * @param path original .ksy path to make error messages more meaningful
    */
  def validateClass(curClass: ClassSpec, path: List[String]): Unit = {
    provider.nowClass = curClass

    curClass.seq.zipWithIndex.foreach { case (attr, attrIdx) =>
      validateAttr(attr, path ++ List("seq", attrIdx.toString))
    }

    curClass.instances.foreach { case (instName, inst) =>
      inst match {
        case pis: ParseInstanceSpec =>
          validateAttr(pis, path ++ List("instances", instName.name))
        case vis: ValueInstanceSpec =>
          // TODO
      }
    }

    curClass.types.foreach { case (nestedName, nestedClass) =>
      validateClass(nestedClass, path ++ List("types", nestedName))
    }
  }

  /**
    * Performs validation of a single parsed attribute (either from a sequence
    * or a parse instance).
    * @param attr attribute to check
    * @param path original .ksy path to make error messages more meaningful
    */
  def validateAttr(attr: AttrLikeSpec, path: List[String]) {
    attr.cond.ifExpr match {
      case Some(ifExpr) =>
        detector.detectType(ifExpr) match {
          case _: BooleanType => // good
          case actual => throw YAMLParseException.exprType("boolean", actual, path ++ List("if"))
        }
      case None =>
        // good
    }

    provider._currentIteratorType = Some(attr.dataType)
    attr.cond.repeat match {
      case RepeatExpr(expr) =>
        detector.detectType(expr) match {
          case _: IntType => // good
          case actual => throw YAMLParseException.exprType("integer", actual, path ++ List("repeat-expr"))
        }
      case RepeatUntil(expr) =>
        detector.detectType(expr) match {
          case _: BooleanType => // good
          case actual => throw YAMLParseException.exprType("boolean", actual, path ++ List("repeat-until"))
        }
      case RepeatEos | NoRepeat =>
        // good
    }

    validateDataType(attr.dataType, path)
  }

  /**
    * Validates single non-composite data type, checking all expressions
    * inside data type definition.
    * @param dataType data type to check
    * @param path original .ksy path to make error messages more meaningful
    */
  def validateDataType(dataType: DataType, path: List[String]) {
    dataType match {
      case blt: BytesLimitType =>
        detector.detectType(blt.size) match {
          case _: IntType => // good
          case actual => throw YAMLParseException.exprType("integer", actual, path ++ List("size"))
        }
      case st: StrFromBytesType =>
        validateDataType(st.bytes, path)
      case ut: UserTypeFromBytes =>
        validateDataType(ut.bytes, path)
      case st: SwitchType =>
        validateSwitchType(st, path)
      case _ =>
        // all other types don't need any specific checks
    }
  }

  def validateSwitchType(st: SwitchType, path: List[String]) {
    val onType = detector.detectType(st.on)
    st.cases.foreach { case (caseExpr, caseType) =>
      val casePath = path ++ List("type", "cases", caseExpr.toString)
      try {
        TypeDetector.assertCompareTypes(onType, detector.detectType(caseExpr), Ast.cmpop.Eq)
      } catch {
        case tme: TypeMismatchError =>
          throw new YAMLParseException(tme.getMessage, casePath)
      }
      validateDataType(caseType, casePath)
    }
  }

//  def checkAssert[T](
//    expr: Ast.expr,
//    expectStr: String,
//    path: List[String],
//    pathKey: String
//  ): Unit = {
//    detector.detectType(expr) match {
//      case x: T => // good
//        Console.err.println("! " + x.isInstanceOf[T])
//        Console.err.println("! " + x)
//      case actual => throw YAMLParseException.exprType(expectStr, actual, path ++ List(pathKey))
//    }
//  }
}
