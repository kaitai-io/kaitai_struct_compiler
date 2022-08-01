package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.precompile.TypeMismatchError

/**
  * Common implementation of arrays translations:
  *
  * * type guessing
  * * type enforcing with a cast
  * * rendering of byte arrays and true arrays
  * * call to actual casting implementation
  *
  * @tparam T translation result type
  */
trait CommonArraysAndCast[T] extends TypeDetector {
  /**
    * Processes elements inside a given [[exprlang.Ast.expr.List]] element to render them
    * as either byte array literal or true array.
    * @param values elements from a list
    * @return translation result
    */
  def doGuessArrayLiteral(values: Seq[Ast.expr]): T = {
    val elementType = detectArrayType(values)
    elementType match {
      case Int1Type(_) =>
        val literalBytes: Seq[Byte] = values.map {
          case Ast.expr.IntNum(x) =>
            if (x < 0 || x > 0xff) {
              throw new TypeMismatchError(s"got a weird byte value in byte array: $x")
            } else {
              x.toByte
            }
          case n =>
            throw new TypeMismatchError(s"got $n in byte array, unable to put it literally")
        }
        doByteArrayLiteral(literalBytes)
      case _ =>
        doArrayLiteral(elementType, values)
    }
  }

  /**
    * Processes an [[exprlang.Ast.expr.CastToType]] element, by checking if
    * this is an literal array type enforcement cast first, and
    * rendering it accordingly as proper literal, or invoking
    * the normal [[doCast]] otherwise.
    * @param v CastToType element
    * @return translation result
    */
  def doCastOrArray(v: Ast.expr.CastToType): T = {
    val castToType = detectCastType(v.typeName)

    v.value match {
      case array: Ast.expr.List =>
        // Special handling for literal arrays: if cast is present,
        // then we don't need to guess the data type
        castToType match {
          case _: BytesType =>
            doByteArray(array.elts)
          case ArrayTypeInStream(elType) =>
            doArrayLiteral(elType, array.elts)
          case _ =>
            // No luck, this is some kind of weird cast, not a type enforcement;
            // Just do it and let real type casting deal with it.
            doCast(v.value, castToType)
        }
      case _ =>
        doCast(v.value, castToType)
    }
  }

  def doCast(value: Ast.expr, typeName: DataType): T
  def doArrayLiteral(t: DataType, value: Seq[Ast.expr]): T
  def doByteArrayLiteral(arr: Seq[Byte]): T
  def doByteArrayNonLiteral(elts: Seq[Ast.expr]): T

  private def doByteArray(elts: Seq[Ast.expr]): T = {
    valuesAsByteArrayLiteral(elts) match {
      case Some(arr) =>
        doByteArrayLiteral(arr)
      case None =>
        doByteArrayNonLiteral(elts)
    }
  }

  private def valuesAsByteArrayLiteral(elts: Seq[Ast.expr]): Option[Seq[Byte]] = {
    Some(elts.map {
      case Ast.expr.IntNum(x) =>
        if (x < 0 || x > 0xff) {
          return None
        } else {
          x.toByte
        }
      case _ =>
        return None
    })
  }
}
