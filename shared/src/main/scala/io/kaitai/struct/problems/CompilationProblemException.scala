package io.kaitai.struct.problems

import io.kaitai.struct.format.ClassSpec

/**
  * Trivial wrapper for CompilationProblem to act as an exception
  *
  * @param problem inner compilation problem
  */
case class CompilationProblemException(problem: CompilationProblem) extends RuntimeException(problem.message) {
  /**
    * @param typeSpec type spec this exception is firing about
    * @return copy of exception with problem localized to a file containing specific type, if necessary
    */
  def localizedInType(typeSpec: ClassSpec): CompilationProblemException =
    CompilationProblemException(problem.localizedInType(typeSpec))
}
