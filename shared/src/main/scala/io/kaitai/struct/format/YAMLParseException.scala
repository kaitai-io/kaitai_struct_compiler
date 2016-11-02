package io.kaitai.struct.format

class YAMLParseException(msg: String, path: List[String])
  extends RuntimeException(s"/${path.mkString("/")}: $msg", null)

object YAMLParseException {
  def badType(expected: String, got: Any, path: List[String]): YAMLParseException = {
    new YAMLParseException(s"expected $expected, got $got (${got.getClass})", path)
  }
}
