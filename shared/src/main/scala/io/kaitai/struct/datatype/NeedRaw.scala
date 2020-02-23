package io.kaitai.struct.datatype

sealed trait NeedRaw {
  val level: Int
}

case object NotRaw extends NeedRaw {
  val level = 0
}
case object RawIo extends NeedRaw {
  val level = 1
}
case object RawProcess extends NeedRaw {
  val level = 1
}
case object RawIoProcess extends NeedRaw {
  val level = 2
}
