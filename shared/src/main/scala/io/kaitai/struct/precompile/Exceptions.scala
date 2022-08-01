package io.kaitai.struct.precompile

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.format.ClassSpec

/**
 * Base class for all expression-related errors, not localized to a certain path
 * in source file.
 */
sealed abstract class ExpressionError(msg: String) extends RuntimeException(msg)
class TypeMismatchError(msg: String) extends ExpressionError(msg)
class TypeUndecidedError(msg: String) extends ExpressionError(msg)

sealed abstract class NotFoundError(msg: String) extends ExpressionError(msg)
class TypeNotFoundError(val name: String, val curClass: ClassSpec)
  extends NotFoundError(s"unable to find type '$name', searching from ${curClass.nameAsStr}")
class FieldNotFoundError(val name: String, val curClass: ClassSpec)
  extends NotFoundError(s"unable to access '$name' in ${curClass.nameAsStr} context")
class EnumNotFoundError(val name: String, val curClass: ClassSpec)
  extends NotFoundError(s"unable to find enum '$name', searching from ${curClass.nameAsStr}")
class EnumMemberNotFoundError(val label: String, val enum: String, val enumDefPath: String)
  extends NotFoundError(s"unable to find enum member '$enum::$label' (enum '$enum' defined at /$enumDefPath)")
class MethodNotFoundError(val name: String, val dataType: DataType)
  extends NotFoundError(s"don't know how to call method '$name' of object type '$dataType'")

/**
  * Internal compiler logic error: should never happen, but at least we want to
  * handle it gracefully if it's happening.
  * @param msg message for the user
  */
case class InternalCompilerError(msg: String) extends RuntimeException(msg)
