package io.kaitai.struct

/**
  * Namespace for all the objects related to compilation results.
  */
object CompileLog {
  trait CanHasErrors {
    def hasErrors: Boolean
  }

  sealed trait InputEntry extends Jsonable with CanHasErrors

  case class InputFailure(errors: List[CompileError]) extends InputEntry {
    override def toJson: String = JSON.mapToJson(Map("errors" -> errors))
    override def hasErrors = true
  }

  case class InputSuccess(
    firstSpecName: String,
    output: Map[String, Map[String, SpecEntry]]
  ) extends InputEntry {
    override def toJson: String = JSON.mapToJson(Map(
      "firstSpecName" -> firstSpecName,
      "output" -> output
    ))

    override def hasErrors: Boolean =
      output.values.map(_.values.map(_.hasErrors).max).max
  }

  /** Compilation result of a single [[io.kaitai.struct.format.ClassSpec]] into a single target language. */
  sealed trait SpecEntry extends Jsonable with CanHasErrors

  case class SpecFailure(errors: List[CompileError]) extends SpecEntry {
    override def toJson: String = JSON.mapToJson(Map("errors" -> errors))
    override def hasErrors: Boolean = true
  }

  case class SpecSuccess(
    topLevelName: String,
    files: List[FileSuccess]
  ) extends SpecEntry {
    override def toJson: String = JSON.mapToJson(Map(
      "topLevelName" -> topLevelName,
      "files" -> files
    ))
    override def hasErrors: Boolean = false
  }

  case class FileSuccess(
    fileName: String,
    contents: String
  ) extends Jsonable {
    override def toString = s"FileSuccess(fileName=$fileName)"
    override def toJson: String = JSON.mapToJson(Map("fileName" -> fileName))
  }

  case class CompileError(
    file: String,
    path: List[String],
    message: String
  ) extends Jsonable {
    override def toJson: String = JSON.mapToJson(Map(
      "file" -> file,
      "path" -> path,
      "message" -> message
    ))
  }
}
