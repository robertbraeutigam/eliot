package com.vanillasource.eliot.eliotc.compiler

import com.vanillasource.eliot.eliotc.feedback.CompilerError

/** The outcome of one [[CompilationSession.compileOnce]]: the diagnostics to print/publish, plus the live generator.
  *
  * The generator is a `CompilationProcess`, so LSP features query it directly — e.g.
  * `generator.getFact(MonomorphicValue(fqn))` for hover. Its facts are immutable; a later compile produces a *new*
  * generator, so queries should target the latest result.
  */
case class CompilationResult(
    generator: IncrementalFactGenerator,
    errors: Seq[CompilerError]
)
