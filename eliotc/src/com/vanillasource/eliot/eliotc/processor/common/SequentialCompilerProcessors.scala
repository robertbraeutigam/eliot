package com.vanillasource.eliot.eliotc.processor.common

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.{CompilerIO, recover}
import com.vanillasource.eliot.eliotc.processor.{CompilerFactKey, CompilerProcessor}

/** Runs all given processors in isolation and then returns a context that has all errors produced by all processors.
  * The resulting CompilerIO is never in an aborted state.
  */
class SequentialCompilerProcessors(processors: Seq[CompilerProcessor]) extends CompilerProcessor {
  override def generate(factKey: CompilerFactKey[?]): CompilerIO[Unit] =
    processors.traverse_(processor => recover(processor.generate(factKey))(()))

  override def wrapWith(wrapper: CompilerProcessor => CompilerProcessor): CompilerProcessor =
    SequentialCompilerProcessors(processors.map(_.wrapWith(wrapper)))
}
