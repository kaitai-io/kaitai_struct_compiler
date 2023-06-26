package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{ClassSpec, EnumSpec}
import io.kaitai.struct.format.Identifier

/**
  * Common interface of a "type provider", i.e. a class that answers
  * queries on resolving particular user types / attributes by given
  * name (which is encountered in an expression).
  */
trait TypeProvider {
  def nowClass: ClassSpec
  def determineType(attrName: String): DataType
  def determineType(attrId: Identifier): DataType
  def determineType(inClass: ClassSpec, attrName: String): DataType
  def determineType(inClass: ClassSpec, attrId: Identifier): DataType
  def resolveEnum(typeName: Ast.typeId, enumName: String): EnumSpec
  def resolveType(typeName: Ast.typeId): DataType
  def isLazy(attrName: String): Boolean
  def isLazy(inClass: ClassSpec, attrName: String): Boolean
}
