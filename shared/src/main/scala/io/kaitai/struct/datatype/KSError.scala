package io.kaitai.struct.datatype

/**
  * Represents all errors & exceptions that Kaitai Struct-generated code
  * in target languages could throw in runtime.
  */
sealed trait KSError {
  def name: String
}

object KSError {
  val RE_EXCEPTION_WITH_TYPE = "^(.*)<(.*)>$".r

  def fromName(name: String): KSError = name match {
    case "EndOfStreamError" => EndOfStreamError
    case "UndecidedEndiannessError" => UndecidedEndiannessError
    case RE_EXCEPTION_WITH_TYPE(excName, dataTypeStr) =>
      val dataType = DataType.pureFromString(dataTypeStr)
      val excClass = excName match {
        case "ValidationNotEqualError" => ValidationNotEqualError
        case "ValidationLessThanError" => ValidationLessThanError
        case "ValidationGreaterThanError" => ValidationGreaterThanError
        case "ValidationNotAnyOfError" => ValidationNotAnyOfError
      }
      excClass(dataType)
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
  * Error to be thrown when validation fails with actual < min.
  * @param dt data type used in validation process
  */
case class ValidationLessThanError(dt: DataType) extends KSError {
  def name = "ValidationLessThanError"
}

/**
  * Error to be thrown when validation fails with actual > max.
  * @param dt data type used in validation process
  */
case class ValidationGreaterThanError(dt: DataType) extends KSError {
  def name = "ValidationGreaterThanError"
}

/**
  * Error to be thrown when validation fails with actual being not any item of the list.
  * @param dt data type used in validation process
  */
case class ValidationNotAnyOfError(dt: DataType) extends KSError {
  def name = "ValidationNotAnyOfError"
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
