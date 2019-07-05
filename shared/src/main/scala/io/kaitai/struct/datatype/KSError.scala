package io.kaitai.struct.datatype

/**
  * Represents all errors & exceptions that Kaitai Struct-generated code
  * in target languages could throw in runtime.
  */
sealed trait KSError

/**
  * Error to be thrown when validation on equality fails.
  */
case object ValidationNotEqualError extends KSError
