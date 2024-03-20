package io.kaitai.struct.datatype

import io.kaitai.struct.exprlang.Ast

/** Defines "working" sub-stream, as beginning part of full stream.
  * All data after terminator byte is ignored and not available for parsing.
  *
  * @param value Byte at which stop reading stream
  * @param include Specifies if terminator byte should be included in the final value
  * @param consume Specify if terminator byte should be "consumed" when reading.
  *        If `true`: the stream pointer will point to the byte after the terminator byte
  *        If `false`: the stream pointer will point to the terminator byte itself
  * @param mandatory If `true`, terminator must be present in the input stream, otherwise
  *        reaching end of stream before encountering terminator also possible.
  *        Corresponds to an `eos-error` key
  */
sealed case class Terminator(
  value: Int,
  include: Boolean,
  consume: Boolean,
  mandatory: Boolean,
)
