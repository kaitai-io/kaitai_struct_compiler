package io.kaitai.struct.format

class YAMLParseException(msg: String, path: List[String])
  extends RuntimeException(s"/${path.mkString("/")}: $msg", null)

object YAMLParseException {
  def badType(expected: String, got: Any, path: List[String]): YAMLParseException = {
    val gotStr = got match {
      case null => "null"
      case _ => s"$got (${got.getClass})"
    }
    new YAMLParseException(s"expected $expected, got $gotStr", path)
  }

  def incompatibleVersion(expected: KSVersion, got: KSVersion, path: List[String]): YAMLParseException =
    new YAMLParseException(s"this ksy requires compiler version at least $expected, but you have $got", path)
}
