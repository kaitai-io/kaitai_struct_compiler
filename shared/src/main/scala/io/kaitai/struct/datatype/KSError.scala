package io.kaitai.struct.datatype

/**
  * Represents all errors & exceptions that Kaitai Struct-generated code
  * in target languages could throw in runtime.
  */
sealed trait KSError {
  def name: String
}

/**
  * Error to be thrown when validation on equality fails.
  */
case object ValidationNotEqualError extends KSError {
  def name = "ValidationNotEqualError"
}

/**
  * Exception that is thrown when we can't decided on endianness
  * and thus can't proceed with parsing.
  */
case object UndecidedEndiannessError extends KSError {
  def name = "UndecidedEndiannessError"
}
