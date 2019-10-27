package io.kaitai.struct.languages.components

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{Identifier, IoIdentifier}

trait ObjectOrientedLanguage extends LanguageCompiler {
  def headerComment = "This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild"

  def expression(e: Ast.expr): String = translator.translate(e)

  /**
    * Renders identifier to a string, specifically for a given
    * language and settings. This usually includes things like
    * case and separator conversion and does *not* include things
    * like prepending "@" or "this." or "self." that might be
    * used to access private member.
    *
    * @param id identifier to render
    * @return identifier as string
    */
  def idToStr(id: Identifier): String

  /**
    * Renders identifier as a proper reference to a private member
    * that represents this field. This might include some prefixes
    * like "@" or "this." or "self.".
    *
    * @param id identifier to render
    * @return identifier as string
    */
  def privateMemberName(id: Identifier): String

  /**
    * Renders identifier as a proper reference to a public member
    * that represents this field.
    *
    * @param id identifier to render
    * @return identifier as string
    */
  def publicMemberName(id: Identifier): String

  /**
    * Renders identifier as a proper reference to a local temporary
    * variable appropriately named to hold a temporary reference to
    * this field.
    *
    * @param id identifier to render
    * @return identifier as string
    */
  def localTemporaryName(id: Identifier): String

  /**
    * Renders identifier as a parameter (method argument) name.
    * Default implementation just calls [[idToStr]].
    * @param id
    * @return
    */
  def paramName(id: Identifier): String = idToStr(id)

  override def normalIO: String = privateMemberName(IoIdentifier)
}
