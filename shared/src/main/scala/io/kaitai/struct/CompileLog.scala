package io.kaitai.struct

object CompileLog {
  sealed trait InputEntry extends Jsonable

  case class InputFailure(errors: List[CompileError]) extends InputEntry {
    override def toJson: String = JSON.mapToJson(Map("errors" -> errors))
  }

  case class InputSuccess(
    firstSpecName: String,
    output: Map[String, Map[String, SpecEntry]]
  ) extends InputEntry {
    override def toJson: String = JSON.mapToJson(Map(
      "firstSpecName" -> firstSpecName,
      "output" -> output
    ))
  }

  /** Compilation result of a single [[io.kaitai.struct.format.ClassSpec]] into a single target language. */
  sealed trait SpecEntry extends Jsonable

  case class SpecFailure(errors: List[CompileError]) extends SpecEntry {
    override def toJson: String = JSON.mapToJson(Map("errors" -> errors))
  }

  case class SpecSuccess(
    topLevelName: String,
    files: List[FileSuccess]
  ) extends SpecEntry {
    override def toJson: String = JSON.mapToJson(Map(
      "topLevelName" -> topLevelName,
      "files" -> files
    ))
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
