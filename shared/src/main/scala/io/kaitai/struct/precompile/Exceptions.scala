package io.kaitai.struct.precompile

import io.kaitai.struct.format.ClassSpec

/**
  * Container for a real exception that happened due to some known problem
  * with input file, and we know where exactly is the culprit (path and file).
  * @param err inner exception
  * @param path YAML path components in file
  * @param file file to report as erroneous, None means "main compilation unit"
  */
case class ErrorInInput(err: Throwable, val path: List[String] = List(), val file: Option[String] = None)
  extends RuntimeException(ErrorInInput.message(err, path, file), err)

object ErrorInInput {
  private def message(err: Throwable, path: List[String], file: Option[String]) = {
    val fileStr = file match {
      case Some(x) => x.replace('\\', '/')
      case None => "(main)"
    }
    val msg = Option(err.getMessage).getOrElse(err.toString)
    s"$fileStr: /${path.mkString("/")}: $msg"
  }
}

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
