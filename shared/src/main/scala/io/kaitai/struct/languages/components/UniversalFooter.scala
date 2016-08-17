package io.kaitai.struct.languages.components

import io.kaitai.struct.exprlang.Ast.expr

/**
  * All footers in the language look the same and can be written by the same
  * simple argument-less method.
  */
trait UniversalFooter {
  /**
    * Single method that outputs all kind of footers in the language.
    */
  def universalFooter: Unit

  def classFooter(name: String): Unit = universalFooter
  def classConstructorFooter: Unit = universalFooter
  def condRepeatExprFooter = universalFooter
  def condRepeatEosFooter: Unit = universalFooter
  def condIfFooter(expr: expr): Unit = universalFooter
  def instanceFooter: Unit = universalFooter
}
