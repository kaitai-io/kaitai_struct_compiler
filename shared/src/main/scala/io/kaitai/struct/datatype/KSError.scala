package io.kaitai.struct.datatype

/**
  * Represents all errors & exceptions that Kaitai Struct-generated code
  * in target languages could throw in runtime.
  */
sealed trait KSError {
  def name: String
}

object KSError {
  val RE_VALIDATION_NOT_EQUAL = "^ValidationNotEqualError<(.*)>$".r

  def fromName(name: String): KSError = name match {
    case "EndOfStreamError" => EndOfStreamError
    case "UndecidedEndiannessError" => UndecidedEndiannessError
    case RE_VALIDATION_NOT_EQUAL(dataTypeStr) =>
      ValidationNotEqualError(DataType.pureFromString(dataTypeStr))
  }
}

/**
  * Error to be thrown when validation on equality fails.
  * @param dt data type used in validation process
  */
case class ValidationNotEqualError(dt: DataType) extends KSError {
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
