package io.kaitai.struct.precompile

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.format.ClassSpec

/**
  * Unified YAML parser exception which pinpoints the row/col of the problem.
  * Used to re-wrap YAML parser library-specific exceptions into this unified
  * format that neutral codebase understands.
  * @param msg user-readable error message
  * @param file file to report as erroneous, None means "main compilation unit"
  * @param line line number which triggered the error (1-based)
  * @param col column number which triggered the error (1-based)
  */
case class YAMLParserError(
  msg: String,
  val file: Option[String] = None,
  val line: Option[Int],
  val col: Option[Int]
) extends RuntimeException(YAMLParserError.message(msg, file, line, col))

object YAMLParserError {
  private def message(msg: String, file: Option[String], line: Option[Int], col: Option[Int]): String = {
    ErrorInInput.formatFileName(file) +
      (line match {
        case Some(lineNum) => s":${lineNum}" + (col match {
          case Some(colNum) => s":${colNum}"
          case None => ""
        })
        case None => ""
      }) + ": " + msg
  }
}

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
  private def message (err: Throwable, path: List[String], file: Option[String]) = {
    val msg = Option(err.getMessage).getOrElse (err.toString)
    s"${formatFileName(file)}: /${path.mkString("/")}: $msg"
  }

  def formatFileName(file: Option[String]): String = file match {
    case Some (x) => x.replace ('\\', '/')
    case None => "(main)"
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
class MethodNotFoundError(val name: String, val dataType: DataType)
  extends NotFoundError(s"don't know how to call method '$name' of object type '$dataType'")

/**
  * Internal compiler logic error: should never happen, but at least we want to
  * handle it gracefully if it's happening.
  * @param msg message for the user
  */
case class InternalCompilerError(msg: String) extends RuntimeException(msg)
