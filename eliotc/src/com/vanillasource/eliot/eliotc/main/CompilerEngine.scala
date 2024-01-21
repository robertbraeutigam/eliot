package com.vanillasource.eliot.eliotc.main

import cats.effect.IO
import com.vanillasource.eliot.eliotc.engine.{FactEngine, FactProcessor, RunningFactEngine}
import com.vanillasource.eliot.eliotc.main.CompilerEngine.CompilerFactProcessor
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor}

class CompilerEngine(processors: Seq[CompilerProcessor]) {
  private val engineProcessors: Seq[CompilerFactProcessor] = processors.map(CompilerFactProcessor.apply)

  def resolve(facts: Seq[CompilerFact]): IO[Map[Any, CompilerFact]] =
    FactEngine(engineProcessors).resolve(facts.map(f => (f.key(), f)).toMap)
}

object CompilerEngine {
  private case class CompilerFactProcessor(processor: CompilerProcessor) extends FactProcessor[Any, CompilerFact] {
    override def process(fact: CompilerFact)(using
        runningFactEngine: RunningFactEngine[Any, CompilerFact]
    ): IO[Unit] =
      processor.process(fact)(using CompilerRunningFactEngine(runningFactEngine))
  }

  private case class CompilerRunningFactEngine(engine: RunningFactEngine[Any, CompilerFact])
      extends CompilationProcess {
    override def registerFact(value: CompilerFact): IO[Unit] = engine.registerFact(value.key(), value)

    override def getFact[K <: CompilerFactKey](key: K): IO[Option[key.FactType]] =
      engine.getFact(key).map(_.asInstanceOf[Option[key.FactType]])
  }
}
