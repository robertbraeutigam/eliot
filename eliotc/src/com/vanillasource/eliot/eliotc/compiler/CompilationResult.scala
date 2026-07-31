package com.vanillasource.eliot.eliotc.compiler

import com.vanillasource.eliot.eliotc.feedback.CompilerError

/** The outcome of one [[CompilationSession.compileOnce]]: the diagnostics to print/publish, whether the target plugin
  * produced what it was asked for, plus the live generator.
  *
  * The generator is a `CompilationProcess`, so LSP features query it directly — e.g.
  * `generator.getFact(MonomorphicValue(fqn))` for hover. Its facts are immutable; a later compile produces a *new*
  * generator, so queries should target the latest result.
  *
  * A run succeeded only when it both reported no errors *and* produced its target: a target that declines without
  * erroring yields no artefact, and must not be mistaken for a successful build.
  */
case class CompilationResult(
    generator: IncrementalFactGenerator,
    errors: Seq[CompilerError],
    targetProduced: Boolean
) {
  def succeeded: Boolean = errors.isEmpty && targetProduced
}
