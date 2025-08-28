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
    case "ConversionError" => ConversionError
    case RE_EXCEPTION_WITH_TYPE(excName, dataTypeStr) =>
      val dataType = DataType.pureFromString(dataTypeStr)
      val excClass = excName match {
        case "ValidationNotEqualError" => ValidationNotEqualError
        case "ValidationLessThanError" => ValidationLessThanError
        case "ValidationGreaterThanError" => ValidationGreaterThanError
        case "ValidationNotAnyOfError" => ValidationNotAnyOfError
        case "ValidationNotInEnumError" => ValidationNotInEnumError
        case "ValidationExprError" => ValidationExprError
      }
      excClass(dataType)
  }
}

/**
  * Error to be thrown when validation fails. All such errors have a data type.
  * @param dt data type used in validation process
  */
abstract class ValidationError(val dt: DataType) extends KSError

/**
  * Error to be thrown when validation on equality fails.
  * @param _dt data type used in validation process
  */
case class ValidationNotEqualError(_dt: DataType) extends ValidationError(_dt) {
  def name = "ValidationNotEqualError"
}

/**
  * Error to be thrown when validation fails with actual < min.
  * @param _dt data type used in validation process
  */
case class ValidationLessThanError(_dt: DataType) extends ValidationError(_dt) {
  def name = "ValidationLessThanError"
}

/**
  * Error to be thrown when validation fails with actual > max.
  * @param _dt data type used in validation process
  */
case class ValidationGreaterThanError(_dt: DataType) extends ValidationError(_dt) {
  def name = "ValidationGreaterThanError"
}

/**
  * Error to be thrown when validation fails with actual being not any item of the list.
  * @param _dt data type used in validation process
  */
case class ValidationNotAnyOfError(_dt: DataType) extends ValidationError(_dt) {
  def name = "ValidationNotAnyOfError"
}

/**
  * Error to be thrown when validation fails with actual not being in the enum.
  * @param _dt data type used in validation process
  */
case class ValidationNotInEnumError(_dt: DataType) extends ValidationError(_dt) {
  def name = "ValidationNotInEnumError"
}

/**
  * Error to be thrown when validation fails with actual not matching the custom
  * validation expression.
  * @param _dt data type used in validation process
  */
case class ValidationExprError(_dt: DataType) extends ValidationError(_dt) {
  def name = "ValidationExprError"
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

/**
 * Exception that maps to real target language exceptions resulting from invalid string-to-number
 * conversions.
 */
case object ConversionError extends KSError {
  def name = "ConversionError"
}
