package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType.SwitchType
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.Identifier

/**
  * An interface for switching operations.
  */
trait SwitchOps {
  def switchStart(id: Identifier, on: Ast.expr): Unit
  def switchCaseFirstStart(condition: Ast.expr): Unit = switchCaseStart(condition)
  def switchCaseStart(condition: Ast.expr): Unit
  def switchCaseEnd(): Unit
  def switchElseStart(): Unit
  def switchElseEnd(): Unit = switchCaseEnd()
  def switchEnd(): Unit

  /**
    * Controls parsing of typeless (BytesType) alternative in switch case. If true,
    * then target language does not support storing both bytes array and true object
    * in the same variable, so we'll use workaround: bytes array will be read as
    * _raw_ variable (which would be used anyway for all other cases as well). If
    * false (which is default), we'll store *both* true objects and bytes array in
    * the same variable.
    */
  def switchBytesOnlyAsRaw = false

  def switchTaggedUnionDeclaration(attrName: Identifier, cases: Map[Ast.expr, DataType]): Unit = {}

  /**
    * Determines whether the switch type should be implemented using a tagged
    * union in targets that support it (at the time of writing, only Zig).
    *
    * @param st switch type of interest
    * @return `true` if a tagged union should be used, `false` otherwise
    */
  def switchUsesTaggedUnion(st: SwitchType): Boolean = false

  /**
    * Generate switch cases by calling case procedures. Suitable for a wide variety of
    * target languages that something remotely resembling C-like `switch`-`case` statement.
    * Thanks to customizable argument type for case procedures, can be used for switch type
    * handling and a variety of other cases (i.e. switching between customizable endianness,
    * etc).
    * @param id attribute identifier
    * @param on on expression to decide upon
    * @param cases cases map: keys should be expressions, values are arbitrary typed objects
    *              that will be passed to case procedures
    * @param normalCaseProc procedure that would handle "normal" (i.e. non-else case)
    * @param elseCaseProc procedure that would handle "else" case
    * @tparam T type of object to pass to procedures
    */
  def switchCases[T](
    id: Identifier,
    on: Ast.expr,
    cases: Map[Ast.expr, T],
    normalCaseProc: (T) => Unit,
    elseCaseProc: (T) => Unit
  ): Unit = {
    switchCasesRender(id, on, cases, normalCaseProc, elseCaseProc)
  }

  protected def switchCasesRender[T](
    id: Identifier,
    on: Ast.expr,
    cases: Map[Ast.expr, T],
    normalCaseProc: (T) => Unit,
    elseCaseProc: (T) => Unit
  ): Unit = {
    val someNormalCases = cases.exists { case (caseExpr, _) =>
      caseExpr != SwitchType.ELSE_CONST
    }

    if (someNormalCases) {
      switchStart(id, on)

      // Pass 1: only normal case clauses
      var first = true

      cases.foreach { case (condition, result) =>
        condition match {
          case SwitchType.ELSE_CONST =>
            // skip for now
          case _ =>
            if (first) {
              switchCaseFirstStart(condition)
              first = false
            } else {
              switchCaseStart(condition)
            }
            normalCaseProc(result)
            switchCaseEnd()
        }
      }

      // Pass 2: else clause, if it is there
      cases.get(SwitchType.ELSE_CONST).foreach { (result) =>
        switchElseStart()
        elseCaseProc(result)
        switchElseEnd()
      }

      switchEnd()
    } else {
      cases.get(SwitchType.ELSE_CONST).foreach { (result) =>
        elseCaseProc(result)
      }
    }
  }
}
