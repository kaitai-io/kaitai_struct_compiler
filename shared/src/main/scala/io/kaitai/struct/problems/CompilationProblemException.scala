package io.kaitai.struct.problems

/**
  * Trivial wrapper for CompilationProblem to act as an exception
  * @param problem inner compilation problem
  */
case class CompilationProblemException(problem: CompilationProblem) extends RuntimeException(problem.message)
