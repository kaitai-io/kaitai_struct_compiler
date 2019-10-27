package io.kaitai.struct.format

/**
  * Common trait for all format parts that stores YAML path that corresponds
  * to particular format part. Used to throw path-localized exceptions, i.e.
  * [[YAMLParseException]] and [[io.kaitai.struct.precompile.ErrorInInput]],
  * and implement better error messaging.
  */
trait YAMLPath {
  def path: List[String]

  def pathStr: String = path.mkString("/")
}
