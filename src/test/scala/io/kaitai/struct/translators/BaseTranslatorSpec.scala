package io.kaitai.struct.translators

import io.kaitai.struct.exprlang.DataType.BaseType
import io.kaitai.struct.exprlang.Expressions
import org.scalatest.Matchers._

abstract trait BaseTranslatorSpec {
  case class Always(t: BaseType) extends TypeProvider {
    override def determineType(name: String): BaseType = t
    override def determineType(parentType: String, name: String): BaseType = t
  }

  def tryOne(t: BaseType, srcStr: String, expectedStr: String, expectedType: BaseType): Unit = {
    tryOne(Always(t), srcStr, expectedStr, expectedType)
  }

  def tryOne(tp: TypeProvider, srcStr: String, expectedStr: String, expectedType: BaseType): Unit = {
    val e = Expressions.parse(srcStr)
    val tr = getTranslator(tp)
    tr.detectType(e) should be(expectedType)
    tr.translate(e) should be(expectedStr)
  }

  def getTranslator(tp: TypeProvider): BaseTranslator
}
