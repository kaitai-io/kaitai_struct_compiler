package io.kaitai.struct.precompile

import io.kaitai.struct.problems.CompilationProblem

/**
  * Common trait for all precompile steps to follow. Mandates usage of `run()` method.
  */
trait PrecompileStep {
  /**
    * Runs a particular precompile step, performing its actions and verifying if there
    * are any problems encountered along the way.
    *
    * @return List of problems (warnings/errors) encountered during precompile step.
    *         Fatal errors (i.e. those which make it meaningless to continue with
    *         further steps) can be also thrown as exceptions.
    */
  def run(): Iterable[CompilationProblem]
}
