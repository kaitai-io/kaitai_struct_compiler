package io.kaitai.struct.languages.components

import io.kaitai.struct.ClassTypeProvider
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType.SwitchType
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.Identifier
import io.kaitai.struct.translators.BaseTranslator

/**
  * Trait to be used when language needs two implementation of switching:
  * a "true" one utilizing switch-like statement and an "if emulation"
  * which utilizes series of if-then-else statements to emulate a switch.
  *
  * "True" switches are typically more efficient, but are limited to a
  * subset of types. Examples of consumers of this pattern are C++, C#, Java.
  */
trait SwitchIfOps extends SwitchOps {
  val translator: BaseTranslator
  def typeProvider: ClassTypeProvider

  /**
    * Determines if this particular implementation of switches would be ok with true
    * built-in `switch` mechanism, or it will require `if`-based emulation.
    *
    * @param onType type we'll be switching over
    * @return true if `if`-based emulation is required
    */
  def switchRequiresIfs(onType: DataType): Boolean

  def switchIfStart(id: Identifier, on: Ast.expr, onType: DataType): Unit
  def switchIfCaseFirstStart(condition: Ast.expr): Unit = switchIfCaseStart(condition)
  def switchIfCaseStart(condition: Ast.expr): Unit
  def switchIfCaseEnd(): Unit
  def switchIfElseStart(): Unit
  def switchIfElseEnd(): Unit = switchIfCaseEnd()
  def switchIfEnd(): Unit

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
  override def switchCases[T](
    id: Identifier,
    on: Ast.expr,
    cases: Map[Ast.expr, T],
    normalCaseProc: (T) => Unit,
    elseCaseProc: (T) => Unit
  ): Unit = {
    val onType = translator.detectType(on)
    typeProvider._currentSwitchType = Some(onType)
    val switchIfs = switchRequiresIfs(onType)

    if (switchIfs) {
      switchCasesUsingIf(id, on, onType, cases, normalCaseProc, elseCaseProc)
    } else {
      switchCasesRender(id, on, cases, normalCaseProc, elseCaseProc)
    }
  }

  protected def switchCasesUsingIf[T](
    id: Identifier,
    on: Ast.expr,
    onType: DataType,
    cases: Map[Ast.expr, T],
    normalCaseProc: (T) => Unit,
    elseCaseProc: (T) => Unit
  ): Unit = {
    val someNormalCases = cases.filter { case (caseExpr, _) =>
      caseExpr != SwitchType.ELSE_CONST
    }.size > 0

    if (someNormalCases) {
      switchIfStart(id, on, onType)

      // Pass 1: only normal case clauses
      var first = true

      cases.foreach { case (condition, result) =>
        condition match {
          case SwitchType.ELSE_CONST =>
            // skip for now
          case _ =>
            if (first) {
              switchIfCaseFirstStart(condition)
              first = false
            } else {
              switchIfCaseStart(condition)
            }
            normalCaseProc(result)
            switchIfCaseEnd()
        }
      }

      // Pass 2: else clause, if it is there
      cases.get(SwitchType.ELSE_CONST).foreach { (result) =>
        switchIfElseStart()
        elseCaseProc(result)
        switchIfElseEnd()
      }

      switchIfEnd()
    } else {
      cases.get(SwitchType.ELSE_CONST).foreach { (result) =>
        elseCaseProc(result)
      }
    }
  }
}
