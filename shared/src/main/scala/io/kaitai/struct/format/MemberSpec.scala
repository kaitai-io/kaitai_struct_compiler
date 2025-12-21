package io.kaitai.struct.format

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.precompile.InternalCompilerError

/**
  * Base trait for everything that would be compiled to be members of the class,
  * i.e. sequence attributes, parse instances, value instances, parameters.
  */
trait MemberSpec extends YAMLPath {
  def id: Identifier
  def dataType: DataType
  def doc: DocSpec

  def dataTypeComposite = dataType

  /**
    * Determines if this attribute can be "null" in some circumstances or not.
    * In some target languages, it would affect data types used, init and
    * cleanup procedures.
    * @return True if this attribute can be "null", false if it's never "null"
    */
  def isNullable: Boolean

  /**
    * Determines if this attribute can be "null" in some circumstances or not.
    * This version is for languages like C++, which make a special exception
    * for raw byte arrays placement for switch statements.
    * @return True if this attribute can be "null", false if it's never "null"
    */
  def isNullableSwitchRaw: Boolean

  /**
    * Updates underlying datatype if possible, used in precompile phase to clarify
    * datatypes.
    * @param newDataType new datatype
    */
  def updateDataType(newDataType: DataType): Unit =
    throw InternalCompilerError("updateDataType is not supported")
}
