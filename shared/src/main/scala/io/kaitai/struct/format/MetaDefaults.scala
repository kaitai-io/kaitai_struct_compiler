package io.kaitai.struct.format

import io.kaitai.struct.datatype.Endianness

case class MetaDefaults(
  endian: Option[Endianness],
  encoding: Option[String]
) {
  def updateWith(metaOpt: Option[MetaSpec]): MetaDefaults = {
    metaOpt match {
      case None => this
      case Some(meta) =>
        MetaDefaults(
          meta.endian.orElse(this.endian),
          meta.encoding.orElse(this.encoding)
        )
    }
  }
}
