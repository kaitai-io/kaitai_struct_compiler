package io.kaitai.struct.precompile

import io.kaitai.struct.format.ClassSpec

/**
  * Base class for all expression-related errors, not localized to a certain path
  * in source file.
  */
sealed abstract class ExpressionError(msg: String) extends RuntimeException(msg)
class TypeMismatchError(msg: String) extends ExpressionError(msg)
class TypeUndecidedError(msg: String) extends ExpressionError(msg)

sealed abstract class NotFoundError(msg: String) extends ExpressionError(msg)
class TypeNotFoundError(val name: String, val context: String)
  extends NotFoundError("x")
class FieldNotFoundError(val name: String, val curClass: ClassSpec)
  extends NotFoundError(s"unable to access '$name' in ${curClass.nameAsStr} context")
class EnumNotFoundError(val name: String, val curClass: ClassSpec)
  extends NotFoundError(s"unable to find enum '$name', searching from ${curClass.nameAsStr}")
