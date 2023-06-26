package io.kaitai.struct

import io.kaitai.struct.problems.CompilationProblem
import io.kaitai.struct.problems.ProblemSeverity

/**
  * Namespace for all the objects related to compilation results.
  */
object CompileLog {
  trait CanHasErrors {
    def hasErrors: Boolean
  }

  sealed trait InputEntry extends Jsonable with CanHasErrors

  case class InputFailure(problems: Iterable[CompilationProblem]) extends InputEntry {
    override def toJson: String = {
      JSON.mapToJson(problemsToMap(problems))
    }
    override def hasErrors = true
  }

  case class InputSuccess(
    firstSpecName: String,
    output: Map[String, Map[String, SpecEntry]],
    problems: Iterable[CompilationProblem]
  ) extends InputEntry {
    override def toJson: String = JSON.mapToJson(Map(
      "firstSpecName" -> firstSpecName,
      "output" -> output
    ) ++ problemsToMap(problems))

    override def hasErrors: Boolean =
      output.values.map(_.values.map(_.hasErrors).max).max
  }

  /** Compilation result of a single [[io.kaitai.struct.format.ClassSpec]] into a single target language. */
  sealed trait SpecEntry extends Jsonable with CanHasErrors

  case class SpecFailure(errors: List[CompilationProblem]) extends SpecEntry {
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

  def problemsToMap(problems: Iterable[CompilationProblem]): Map[String, Iterable[CompilationProblem]] = {
    val problemsByIsWarning = problems.groupBy(_.severity == ProblemSeverity.Warning)
    problemsByIsWarning.map { case (isWarning, suchProblems) =>
      (if (isWarning) "warnings" else "errors") -> suchProblems
    }
  }
}
