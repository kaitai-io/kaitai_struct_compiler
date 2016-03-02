package io.kaitai.struct.exprlang

object DataType {
  sealed trait BaseType
  case object IntType extends BaseType
  case object StrType extends BaseType
  case object BooleanType extends BaseType
  case class ArrayType(elType: BaseType) extends BaseType
  case class MapType(keyType: BaseType, valueType: BaseType) extends BaseType
  case object ObjType extends BaseType
}
