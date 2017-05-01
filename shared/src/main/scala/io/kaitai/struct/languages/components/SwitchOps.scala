package io.kaitai.struct.languages.components

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
}
