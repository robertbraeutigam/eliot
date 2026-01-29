package com.vanillasource.eliot.eliotc.visualization

import cats.effect.IO
import com.vanillasource.eliot.eliotc.processor.{CompilationProcess, CompilerFact, CompilerFactKey}

/** Wraps a CompilationProcess to track fact requests and registrations. Records which processor is
  * requesting/registering facts for visualization.
  */
final class TrackedCompilationProcess(
    underlying: CompilationProcess,
    tracker: FactVisualizationTracker,
    processorName: String
) extends CompilationProcess {

  override def getFact[V <: CompilerFact, K <: CompilerFactKey[V]](key: K): IO[Option[V]] =
    for {
      _      <- tracker.recordFactRequest(processorName, key)
      result <- underlying.getFact(key)
    } yield result

  override def registerFact(value: CompilerFact): IO[Unit] =
    for {
      _ <- tracker.recordFactProduction(processorName, value.key())
      _ <- underlying.registerFact(value)
    } yield ()
}
