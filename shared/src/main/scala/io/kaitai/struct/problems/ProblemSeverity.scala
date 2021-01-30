package io.kaitai.struct.problems

sealed trait ProblemSeverity {
  def message: String
}

object ProblemSeverity {
  case object Fatal extends ProblemSeverity {
    def message = "fatal error"
  }
  case object Error extends ProblemSeverity {
    def message = "error"
  }
  case object Warning extends ProblemSeverity {
    def message = "warning"
  }
}
