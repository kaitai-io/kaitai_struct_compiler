package io.kaitai.struct.translators

import io.kaitai.struct.exprlang.Ast

/**
  * Provides default implementations for byte arrays operations equal to true array operations.
  * Useful for languages which do not make (major) distinctions between true arrays and byte arrays.
  * @tparam T translation result
  */
trait ByteArraysAsTrueArrays[T] extends CommonMethods[T] {
  override def bytesSubscript(container: Ast.expr, idx: Ast.expr): T = arraySubscript(container, idx)
  override def bytesFirst(b: Ast.expr): T = arrayFirst(b)
  override def bytesLast(b: Ast.expr): T = arrayLast(b)
  override def bytesLength(b: Ast.expr): T = arraySize(b)
  override def bytesMin(b: Ast.expr): T = arrayMin(b)
  override def bytesMax(b: Ast.expr): T = arrayMax(b)
}
