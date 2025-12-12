package io.kaitai.struct

object InputLoader {
  sealed trait InputFormat
  object KSY extends InputFormat
  object KSC extends InputFormat

  def detectFormat(fileName: String): InputFormat = {
    if (fileName.endsWith(".ksy")) {
      KSY
    } else if (fileName.endsWith(".ksc")) {
      KSC
    } else {
      throw new IllegalArgumentException(s"Unknown file format for file: $fileName")
    }
  }
}
