package io.kaitai.struct

sealed trait Logger {
  def info(msgGen: () => String): Unit
  def warn(msgGen: () => String): Unit
}

case object NullLogger extends Logger {
  override def info(msgGen: () => String): Unit = {}
  override def warn(msgGen: () => String): Unit = {}
}

case object ConsoleLogger extends Logger {
  override def info(msgGen: () => String): Unit = {
    val msg: String = msgGen()
    Console.println(msg)
  }

  override def warn(msgGen: () => String): Unit = info(msgGen)
}

object Log {
  val VALID_SUBSYS = Seq(
    "file",
    "value",
    "parent",
    "type_resolve",
    "type_valid",
    "seq_sizes",
    "import",
    "enum_resolve"
  )

  var fileOps: Logger = NullLogger
  var typeProcValue: Logger = NullLogger
  var typeProcParent: Logger = NullLogger
  var typeResolve: Logger = NullLogger
  var typeValid: Logger = NullLogger
  var seqSizes: Logger = NullLogger
  var importOps: Logger = NullLogger
  var enumResolve: Logger = NullLogger

  def initFromVerboseFlag(subsystems: Seq[String]): Unit = {
    fileOps = NullLogger
    typeProcParent = NullLogger

    subsystems.foreach {
      case "file" => fileOps = ConsoleLogger
      case "value" => typeProcValue = ConsoleLogger
      case "parent" => typeProcParent = ConsoleLogger
      case "type_resolve" => typeResolve = ConsoleLogger
      case "type_valid" => typeValid = ConsoleLogger
      case "seq_sizes" => seqSizes = ConsoleLogger
      case "import" => importOps = ConsoleLogger
      case "enum_resolve" => enumResolve = ConsoleLogger
    }
  }
}
