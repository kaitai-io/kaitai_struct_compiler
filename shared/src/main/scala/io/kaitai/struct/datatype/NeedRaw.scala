package io.kaitai.struct.datatype

sealed trait NeedRaw {
  val level: Int
  val hasIO: Boolean = false
  val hasRaw: Boolean = false
}

case object NotRaw extends NeedRaw {
  val level = 0
}
case object RawIo extends NeedRaw {
  val level = 1
  override val hasIO = true
}
case object RawProcess extends NeedRaw {
  val level = 1
  override val hasRaw = true
}
case object RawIoProcess extends NeedRaw {
  val level = 2
  override val hasIO = true
  override val hasRaw = true
}
