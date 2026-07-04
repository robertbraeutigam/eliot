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
  override def generate(factKey: CompilerFactKey[?]): CompilerIO[Unit] =
    processors
      .traverse(processor => recoverWithAborted(processor.generate(factKey))(()))
      .flatMap(results => abort[Unit].whenA(results.exists(_._2)))

  override def wrapWith(wrapper: CompilerProcessor => CompilerProcessor): CompilerProcessor =
    SequentialCompilerProcessors(processors.map(_.wrapWith(wrapper)))
}
