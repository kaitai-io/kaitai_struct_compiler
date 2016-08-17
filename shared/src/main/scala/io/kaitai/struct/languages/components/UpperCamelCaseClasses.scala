package io.kaitai.struct.languages.components

import io.kaitai.struct.Utils

trait UpperCamelCaseClasses {
  def type2class(name: String) = Utils.upperCamelCase(name)
}
