package io.kaitai.struct.languages

trait UpperCamelCaseClasses {
  def type2class(name: String) = name.split("_").map(x => x.charAt(0).toUpper + x.substring(1)).mkString
}
