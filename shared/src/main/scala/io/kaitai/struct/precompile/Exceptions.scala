package io.kaitai.struct.precompile

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.format.ClassSpec
import io.kaitai.struct.translators.MethodArgType

/**
 * Base class for all expression-related errors, not localized to a certain path
 * in source file.
 */
sealed abstract class ExpressionError(msg: String) extends RuntimeException(msg)
class TypeMismatchError(msg: String) extends ExpressionError(msg)
class TypeUndecidedError(msg: String) extends ExpressionError(msg)
class WrongMethodCall(val dataType: MethodArgType, val methodName: String, val expectedSigs: Iterable[String], val actualSig: String)
  extends ExpressionError(s"wrong arguments to method call `$methodName` on $dataType: expected ${expectedSigs.mkString(" or ")}, got $actualSig")

sealed abstract class NotFoundError(msg: String) extends ExpressionError(msg)
class TypeNotFoundError(val name: String, val curClass: ClassSpec)
  extends NotFoundError(s"unable to find type '$name', searching from ${curClass.nameAsStr}")
class FieldNotFoundError(val name: String, val curClass: ClassSpec)
  extends NotFoundError(s"unable to access '$name' in ${curClass.nameAsStr} context")
class EnumNotFoundError(val name: String, val curClass: ClassSpec)
  extends NotFoundError(s"unable to find enum '$name', searching from ${curClass.nameAsStr}")
class EnumMemberNotFoundError(val label: String, val enumName: String, val enumDefPath: String)
  extends NotFoundError(s"unable to find enum member '$enumName::$label' (enum '$enumName' defined at /$enumDefPath)")

// TODO: get rid of MethodNotFoundError in favor of MethodNotFoundErrorWithArg, rename it back
// requires refactoring of [[TypeDetector]]
class MethodNotFoundError(val name: String, val dataType: DataType)
  extends NotFoundError(s"don't know how to call method '$name' of object type '$dataType'")
class MethodNotFoundErrorWithArg(val name: String, val argType: MethodArgType)
  extends NotFoundError(s"don't know how to call method '$name' of object type '$argType'")

/**
  * Internal compiler logic error: should never happen, but at least we want to
  * handle it gracefully if it's happening.
  * @param msg message for the user
  */
case class InternalCompilerError(msg: String) extends RuntimeException(msg)
