package io.kaitai.struct.format

class YAMLParseException(msg: String, path: List[String])
  extends RuntimeException(s"/${path.mkString("/")}: $msg", null)
