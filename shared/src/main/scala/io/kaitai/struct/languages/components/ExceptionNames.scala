package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype.{KSError, ValidationNotEqualError}

/**
  * Stores per-language knowledge on how a particular KS-generated
  * runtime exceptions will be named in particular language.
  */
trait ExceptionNames {
  /**
    * Resolves string name of exception in target language.
    * Default implementation provides UpperCamelCase renditions
    * of original names.
    * @param err KS-generated error that might be thrown in runtime
    * @return name of exception as a string
    */
  def ksErrorName(err: KSError): String = err match {
    case ValidationNotEqualError => "ValidationNotEqualError"
  }
}
