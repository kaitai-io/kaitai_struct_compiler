package io.kaitai.struct.datatype

/**
  * Represents all errors & exceptions that Kaitai Struct-generated code
  * in target languages could throw in runtime.
  */
sealed trait KSError {
  def name: String
}

object KSError {
  def fromName(name: String): KSError = name match {
    case "EndOfStreamError" => EndOfStreamError
    case "UndecidedEndiannessError" => UndecidedEndiannessError
    case "ValidationNotEqualError" => ValidationNotEqualError
  }
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

/**
  * Generic exception that is thrown when we're reached past
  * the end of current stream.
  */
case object EndOfStreamError extends KSError {
  def name = "EndOfStreamError"
}
