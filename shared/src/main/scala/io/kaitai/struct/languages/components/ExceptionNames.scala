package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype.KSError

/**
  * Stores per-language knowledge on how a particular KS-generated
  * runtime exceptions will be named in particular language.
  */
trait ExceptionNames {
  /**
    * Resolves string name of exception in target language.
    * Suggested implementation is to use `err.name` that provides
    * UpperCamelCase renditions of original names.
    * @param err KS-generated error that might be thrown in runtime
    * @return name of exception as a string
    */
  def ksErrorName(err: KSError): String
}
