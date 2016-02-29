package io.kaitai.struct.exprlang

object DataType {
  sealed trait BaseType
  case object IntType extends BaseType
  case object StrType extends BaseType
  case object BooleanType extends BaseType
  case object ArrayType extends BaseType
  case object ObjType extends BaseType
}
