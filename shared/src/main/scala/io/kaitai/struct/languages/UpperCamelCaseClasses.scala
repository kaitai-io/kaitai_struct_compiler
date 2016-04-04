package io.kaitai.struct.languages

import io.kaitai.struct.Utils

trait UpperCamelCaseClasses {
  def type2class(name: String) = Utils.upperCamelCase(name)
}
