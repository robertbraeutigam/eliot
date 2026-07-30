package com.vanillasource.eliot.eliotc.processor.common

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.{abort, recoverWithAborted, CompilerIO}
import com.vanillasource.eliot.eliotc.processor.{CompilerFactKey, CompilerProcessor}

/** Runs all given processors in isolation and then returns a context that has all errors produced by all processors.
  *
  * Every processor runs regardless of what earlier ones did (errors and aborts are recovered between children), but the
  * abort signal is preserved: if any child aborted — the sanctioned way for a processor to *decline* producing a fact —
  * the whole sequence ends aborted, so the engine can tell an explicit decline apart from a generation that silently
  * produced neither fact nor error.
  */
class SequentialCompilerProcessors(processors: Seq[CompilerProcessor]) extends CompilerProcessor {
  /** Processors that cannot handle this key at all are skipped before any of that machinery is set up — of the ~50
    * processors offered every key, all but one or two are in that position, and error-isolating each of them to have it
    * immediately do nothing was measurably a tenth of a compilation.
    */
  override def generate(factKey: CompilerFactKey[?]): CompilerIO[Unit] =
    processors
      .filter(_.handles(factKey))
      .traverse(processor => recoverWithAborted(processor.generate(factKey))(()))
      .flatMap(results => abort[Unit].whenA(results.exists(_._2)))

  /** A sequence handles what any of its members handles. */
  override def handles(factKey: CompilerFactKey[?]): Boolean = processors.exists(_.handles(factKey))

  override def wrapWith(wrapper: CompilerProcessor => CompilerProcessor): CompilerProcessor =
    SequentialCompilerProcessors(processors.map(_.wrapWith(wrapper)))
}
