package com.vanillasource.eliot.eliotc.main

import cats.effect.IO
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor, Init}
import com.vanillasource.eliot.eliotc.engine.{FactEngine, FactProcessor, RunningFactEngine}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.main.Compiler.CompilerFactProcessor
import com.vanillasource.eliot.eliotc.source.{
  InitSourcePaths,
  SourceContentReader,
  SourcedErrorPrinter,
  WalkSourcePaths
}
import com.vanillasource.eliot.eliotc.token.Tokenizer

case class Compiler(cmdLineArguments: CommandLineArguments) extends Logging {
  private val processors: Seq[CompilerProcessor]           = Seq(
    InitSourcePaths(cmdLineArguments.paths),
    WalkSourcePaths(),
    SourceContentReader(),
    SourcedErrorPrinter(),
    Tokenizer()
  )
  private val engineProcessors: Seq[CompilerFactProcessor] = processors.map(CompilerFactProcessor.apply)

  def run(): IO[Unit] = for {
    _     <- info("compiler starting...")
    engine = FactEngine(engineProcessors)
    _     <- engine.resolve(Map((Init, Init)))
  } yield ()
}

object Compiler {
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
