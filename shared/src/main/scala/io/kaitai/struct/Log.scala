package io.kaitai.struct

sealed trait Logger {
  def info(msgGen: () => String)
}

case object NullLogger extends Logger {
  override def info(msgGen: () => String): Unit = {}
}

case object ConsoleLogger extends Logger {
  override def info(msgGen: () => String): Unit = {
    val msg: String = msgGen()
    Console.println(msg)
  }
}

object Log {
  val VALID_SUBSYS = Seq(
    "file",
    "value",
    "parent"
  )

  var fileOps: Logger = NullLogger
  var typeProcValue: Logger = NullLogger
  var typeProcParent: Logger = NullLogger

  def initFromVerboseFlag(subsystems: Seq[String]): Unit = {
    fileOps = NullLogger
    typeProcParent = NullLogger

    subsystems.foreach {
      case "file" => fileOps = ConsoleLogger
      case "value" => typeProcValue = ConsoleLogger
      case "parent" => typeProcParent = ConsoleLogger
    }
  }
}
