package com.vanillasource.eliot.eliotc.visualization

import cats.data.ReaderT
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.processor.{CompilationProcess, CompilerFactKey, CompilerProcessor}

/** Wraps a CompilerProcessor to track its fact interactions. Intercepts the CompilationProcess to record which facts
  * this processor requests and produces.
  */
final class TrackedCompilerProcessor(
    underlying: CompilerProcessor,
    tracker: FactVisualizationTracker,
    processorName: String
) extends CompilerProcessor {

  override def generate(factKey: CompilerFactKey[?]): CompilerIO[Unit] =
    ReaderT { process =>
      val trackedProcess = new TrackedCompilationProcess(process, tracker, processorName)
      underlying.generate(factKey).run(trackedProcess)
    }
}

object TrackedCompilerProcessor {

  /** Wraps a processor tree to track all interactions.
    */
  def wrapProcessor(processor: CompilerProcessor, tracker: FactVisualizationTracker): CompilerProcessor =
    TrackedCompilerProcessor(processor, tracker, processor.getClass.getName)
}
