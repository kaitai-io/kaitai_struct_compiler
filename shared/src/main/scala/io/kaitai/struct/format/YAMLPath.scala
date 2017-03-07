package io.kaitai.struct.format

trait YAMLPath {
  def path: List[String]

  def pathStr: String = path.mkString("/")
}
