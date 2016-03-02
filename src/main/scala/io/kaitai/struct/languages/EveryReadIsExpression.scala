package io.kaitai.struct.languages

import io.kaitai.struct.format.AttrSpec

/**
  * Helper trait for languages where single parsing of every standard or user data type is done as expression, i.e. an
  * rvalue. In these languages, "attrStdTypeParse" is replaced with higher-level API: "stdTypeParseExpr" and
  * "handleAssignment".
  */
trait EveryReadIsExpression extends LanguageCompiler {
  override def attrStdTypeParse(id: String, attr: AttrSpec, endian: Option[String]): Unit = {
    handleAssignment(id, attr, stdTypeParseExpr(attr, endian), normalIO)
  }

  def stdTypeParseExpr(attr: AttrSpec, endian: Option[String]): String
  def handleAssignment(id: String, attr: AttrSpec, expr: String, io: String): Unit
}
