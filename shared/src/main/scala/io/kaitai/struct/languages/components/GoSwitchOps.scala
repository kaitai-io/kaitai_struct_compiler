package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType.{BytesType, SwitchType}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.Identifier
import io.kaitai.struct.translators.GoTranslator

trait GoSwitchOps extends SwitchOps {
  val translator: GoTranslator

  def switchShouldUseCompareFn(onType: DataType): (Option[String], () => Unit)
  def switchCaseStartCompareFn(compareFn: String, switchOn: Ast.expr, condition: Ast.expr): Unit

  override def switchCases[T](
    id: Identifier,
    on: Ast.expr,
    cases: Map[Ast.expr, T],
    normalCaseProc: (T) => Unit,
    elseCaseProc: (T) => Unit
  ): Unit = {
    val onType = translator.detectType(on)
    switchShouldUseCompareFn(onType) match {
      case (Some(compareFn: String), compareFnCallback) =>
        switchCasesUsingCompareFn(id, on, compareFn, compareFnCallback, cases, normalCaseProc, elseCaseProc)
      case (None, _) =>
        switchCasesRender(id, on, cases, normalCaseProc, elseCaseProc)
    }
  }

  protected def switchCasesUsingCompareFn[T](
    id: Identifier,
    on: Ast.expr,
    compareFn: String,
    compareFnCallback: () => Unit,
    cases: Map[Ast.expr, T],
    normalCaseProc: (T) => Unit,
    elseCaseProc: (T) => Unit
  ): Unit = {
    switchStart(id, Ast.expr.Bool(true))

    cases.foreach { case (condition, result) =>
      condition match {
        case SwitchType.ELSE_CONST =>
        case _ =>
          compareFnCallback()
          switchCaseStartCompareFn(compareFn, on, condition)
          normalCaseProc(result)
          switchCaseEnd()
      }
    }

    cases.get(SwitchType.ELSE_CONST).foreach { (result) =>
      switchElseStart()
      elseCaseProc(result)
      switchElseEnd()
    }

    switchEnd()
  }
}
