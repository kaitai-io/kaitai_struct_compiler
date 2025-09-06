package io.kaitai.struct.languages.components

import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format.ClassSpec

/**
  * All footers in the language look the same and can be written by the same
  * simple argument-less method.
  */
trait UniversalFooter extends LanguageCompiler {
  /**
    * Single method that outputs all kind of footers in the language.
    */
  def universalFooter: Unit

  def classFooter(name: String): Unit = universalFooter
  def classConstructorFooter: Unit = universalFooter
  override def readFooter: Unit = universalFooter
  override def writeFooter: Unit = universalFooter
  override def writeInstanceFooter: Unit = universalFooter
  override def checkInstanceFooter: Unit = universalFooter
  def condRepeatExprFooter = universalFooter
  def condRepeatEosFooter: Unit = universalFooter
  override def condRepeatCommonFooter: Unit = universalFooter
  def condIfFooter: Unit = universalFooter
  def instanceFooter: Unit = universalFooter
}
