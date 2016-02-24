package io.kaitai.struct.format

trait ProcessExpr {
  def outputType: String
}

case object ProcessZlib extends ProcessExpr {
  override def outputType: String = null
}
case object ProcessHexStrToInt extends ProcessExpr {
  override def outputType: String = "u4"
}
case class ProcessXor(key: String) extends ProcessExpr {
  override def outputType: String = null
}

object ProcessExpr {
  private val ReXor = "^xor\\(\\s*(.*?)\\s*\\)$".r

  def fromStr(s: String): Option[ProcessExpr] = {
    if (s == null)
      return None
    Some(s match {
      case "zlib" =>
        ProcessZlib
      case "hexstr_to_int" =>
        ProcessHexStrToInt
      case ReXor(arg) =>
        ProcessXor(arg)
      case _ =>
        throw new RuntimeException(s"Invalid process: '$s'")
    })
  }
}
