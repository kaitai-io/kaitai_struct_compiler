package io.kaitai.struct.precompile

import io.kaitai.struct.{ClassTypeProvider, Log}
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.problems.{CompilationProblem, ErrorInInput, ExpressionTypeError, KSYParseError, ParamMismatchError}
import io.kaitai.struct.translators.{ExpressionValidator, TypeDetector}

import scala.reflect.ClassTag

/**
  * Validates all expressions used inside the given ClassSpec to use expected types.
  * Also ensures that all expressions usage of types (in typecasting operator,
  * enums, sizeof operator, etc) matches loaded classes layout & names.
  * @param specs bundle of class specifications (used only to find external references)
  * @param topClass class to start check with
  */
class TypeValidator(specs: ClassSpecs) extends PrecompileStep {
  val provider = new ClassTypeProvider(specs, specs.firstSpec)
  val detector = new ExpressionValidator(provider)

  /**
    * Starts the check from top-level class.
    */
  def run(): Iterable[CompilationProblem] = specs.mapTopLevel { (specName, curClass) =>
    Log.typeValid.info(() => s"validating top level class '$specName'")
    provider.topClass = curClass
    curClass.mapRec(validateClass).map(problem => problem.localizedInType(curClass))
  }

  /**
    * Performs validation of a single ClassSpec: would validate
    * sequence attributes (`seq`), instances (`instances`) and all
    * nested subtypes (`types`) recursively. `doc` and `enums` are
    * not checked, as they contain no expressions.
    *
    * @param curClass class to check
    */
  def validateClass(curClass: ClassSpec): Iterable[CompilationProblem] = {
    Log.typeValid.info(() => s"validateClass(${curClass.nameAsStr})")
    provider.nowClass = curClass

    val res1 = curClass.seq.flatMap(validateAttr)

    val res2 = curClass.instances.flatMap { case (_, inst) =>
      inst match {
        case pis: ParseInstanceSpec =>
          validateParseInstance(pis)
        case vis: ValueInstanceSpec =>
          validateValueInstance(vis)
      }
    }

    List(res1, res2).flatten
  }

  /**
    * Performs validation of a single parsed attribute (either from a sequence
    * or a parse instance).
    * @param attr attribute to check
    */
  def validateAttr(attr: AttrLikeSpec): Iterable[CompilationProblem] = {
    Log.typeValid.info(() => s"validateAttr(${attr.id.humanReadable})")

    val path = attr.path

    val problemsIf: Iterable[CompilationProblem] = attr.cond.ifExpr.flatMap((ifExpr) =>
      checkAssert[BooleanType](ifExpr, "boolean", path, "if")
    )

    provider._currentIteratorType = Some(attr.dataType)
    val problemsRepeat: Iterable[CompilationProblem] = attr.cond.repeat match {
      case RepeatExpr(expr) =>
        checkAssert[IntType](expr, "integer", path, "repeat-expr")
      case RepeatUntil(expr) =>
        checkAssert[BooleanType](expr, "boolean", path, "repeat-until")
      case RepeatEos | NoRepeat =>
        None
    }

    val problemsDataType = validateDataType(attr.dataType, path)

    List(problemsIf, problemsRepeat, problemsDataType).flatten
  }

  def validateParseInstance(pis: ParseInstanceSpec): Iterable[CompilationProblem] = {
    val problemsAttr: Iterable[CompilationProblem] = validateAttr(pis)

    Log.typeValid.info(() => s"validateParseInstance(${pis.id.humanReadable})")

    val problemsIo: Iterable[CompilationProblem] = pis.io match {
      case Some(io) => checkAssertObject(io, KaitaiStreamType, "IO stream", pis.path, "io")
      case None => None // all good
    }

    val problemsPos: Iterable[CompilationProblem] = pis.pos match {
      case Some(pos) => checkAssert[IntType](pos, "integer", pis.path, "pos")
      case None => None // all good
    }

    List(problemsAttr, problemsIo, problemsPos).flatten
  }

  def validateValueInstance(vis: ValueInstanceSpec): Option[CompilationProblem] = {
    try {
      detector.validate(vis.value)
      None
    } catch {
      case err: ExpressionError =>
        Some(ErrorInInput(err, vis.path ++ List("value")))
    }
  }

  /**
    * Validates single non-composite data type, checking all expressions
    * inside data type definition.
    *
    * @param dataType data type to check
    * @param path original .ksy path to make error messages more meaningful
    */
  def validateDataType(dataType: DataType, path: List[String]): Iterable[CompilationProblem] = {
    // validate args vs params
    val problemsArgsVsParams: Iterable[CompilationProblem] = dataType match {
      case ut: UserType =>
        // we only validate non-opaque types, opaque are unverifiable by definition
        if (!ut.isOpaque) {
          validateArgsVsParams(ut.args, ut.classSpec.get.params, path ++ List("type"))
        } else {
          None
        }
      case _ =>
        None // no args or params in non-user types
    }

    val problemsTypeSpecific: Iterable[CompilationProblem] = dataType match {
      case blt: BytesLimitType =>
        checkAssert[IntType](blt.size, "integer", path, "size")
      case st: StrFromBytesType =>
        validateDataType(st.bytes, path)
      case ut: UserTypeFromBytes =>
        validateDataType(ut.bytes, path)
      case st: SwitchType =>
        validateSwitchType(st, path)
      case _ =>
        None // all other types don't need any specific checks
    }

    List(problemsArgsVsParams, problemsTypeSpecific).flatten
  }

  def validateSwitchType(st: SwitchType, path: List[String]): Iterable[CompilationProblem] = {
    val onType = detector.detectType(st.on)

    detector.validate(st.on)
    st.cases.flatMap { case (caseExpr, caseType) =>
      val casePath = path ++ List("type", "cases", caseExpr.toString)
      val problems1 = if (caseExpr != SwitchType.ELSE_CONST) {
        try {
          TypeDetector.assertCompareTypes(onType, detector.detectType(caseExpr), Ast.cmpop.Eq)
          detector.validate(caseExpr)
          None
        } catch {
          case err: Throwable =>
            Some(ErrorInInput(err, casePath))
        }
      } else {
        None
      }
      // All properties of types is declared on the common level for all variants so
      // we don't use `casePath` here
      // FIXME: We need to filter repeated errors here, because some errors influences
      // many cases
      val problems2 = validateDataType(caseType, path)
      problems1 ++ problems2
    }
  }

  /**
    * Validates that arguments given for a certain type match list of parameters
    * declared for that type.
    * @param args arguments given in invocation
    * @param params parameters declared in a user type
    * @param path path where invocation happens
    * @return
    */
  def validateArgsVsParams(args: Seq[Ast.expr], params: List[ParamDefSpec], path: List[String]): Iterable[CompilationProblem] = {
    if (args.size != params.size)
      return Some(KSYParseError.invalidParamCount(params.size, args.size, path).problem)

    args.indices.flatMap { (i) =>
      val arg = args(i)
      val param = params(i)
      val tArg = detector.detectType(arg)
      detector.validate(arg)
      val tParam = param.dataType

      if (!TypeDetector.canAssign(tArg, tParam)) {
        Some(ParamMismatchError(i, tArg, param.id.humanReadable, tParam, path))
      } else {
        None
      }
    }
  }

  /**
    * Checks that expression's type conforms to a given datatype, otherwise
    * throw a human-readable exception, with some pointers that would help
    * finding the expression in source .ksy.
    *
    * Note: `T: Manifest` is required due to JVM type erasure. See
    * http://stackoverflow.com/a/42533114 for more info.
    *
    * @param expr expression to check
    * @param expectStr string to include
    * @param path path to expression base
    * @param pathKey key that contains expression in given path
    * @tparam T type that expression must conform to
    */
  def checkAssert[T: ClassTag](
    expr: Ast.expr,
    expectStr: String,
    path: List[String],
    pathKey: String
  ): Option[CompilationProblem] = {
    try {
      detector.detectType(expr) match {
        case _: T => // good
        case st: SwitchType =>
          st.combinedType match {
            case _: T => // good
            case actual =>
              return Some(ExpressionTypeError(expectStr, actual, path ++ List(pathKey)))
          }
        case actual =>
          return Some(ExpressionTypeError(expectStr, actual, path ++ List(pathKey)))
      }
      detector.validate(expr)
      None
    } catch {
      case err: InvalidIdentifier =>
        Some(ErrorInInput(err, path ++ List(pathKey)))
      case err: ExpressionError =>
        Some(ErrorInInput(err, path ++ List(pathKey)))
    }
  }

  /**
    * Checks that expression's type conforms to a given datatype, otherwise
    * throw a human-readable exception, with some pointers that would help
    * finding the expression in source .ksy.
    *
    * This version works with case objects.
    *
    * @param expr expression to check
    * @param expectStr string to include
    * @param path path to expression base
    * @param pathKey key that contains expression in given path
    */
  def checkAssertObject(
    expr: Ast.expr,
    expected: Object,
    expectStr: String,
    path: List[String],
    pathKey: String
  ): Option[CompilationProblem] = {
    try {
      val detected = detector.detectType(expr)
      if (detected == expected) {
        // good
      } else {
        detected match {
          case st: SwitchType =>
            val combinedType = st.combinedType
            if (combinedType == expected) {
              // good
            } else {
              return Some(ExpressionTypeError(expectStr, combinedType, path ++ List(pathKey)))
            }
          case actual =>
            return Some(ExpressionTypeError(expectStr, actual, path ++ List(pathKey)))
        }
      }
      detector.validate(expr)
      None
    } catch {
      case err: InvalidIdentifier =>
        Some(ErrorInInput(err, path ++ List(pathKey)))
      case err: ExpressionError =>
        Some(ErrorInInput(err, path ++ List(pathKey)))
    }
  }
}
