package io.kaitai.struct.languages.components

import io.kaitai.struct.format.{Identifier, IoIdentifier}

trait ObjectOrientedLanguage {
  def headerComment = "This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild"

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

  def normalIO: String = privateMemberName(IoIdentifier)
}
