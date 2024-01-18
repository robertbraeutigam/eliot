package com.vanillasource.eliot.eliotc.main

import cats.effect.IO
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor, Init}
import com.vanillasource.eliot.eliotc.engine.{FactEngine, FactProcessor, RunningFactEngine}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.main.Compiler.CompilerFactProcessor
import com.vanillasource.eliot.eliotc.source.{InitSourcePaths, SourceContentReader, WalkSourcePaths}

case class Compiler(cmdLineArguments: CommandLineArguments) extends Logging {
  private val processors: Seq[CompilerProcessor]           = Seq(
    InitSourcePaths(cmdLineArguments.paths),
    WalkSourcePaths(),
    SourceContentReader()
  )
  private val engineProcessors: Seq[CompilerFactProcessor] = processors.map(CompilerFactProcessor.apply)

  def run(): IO[Unit] = for {
    _     <- info("compiler starting...")
    engine = FactEngine(engineProcessors)
    _     <- engine.resolve(Map((Init, Init)))
  } yield ()
}

object Compiler {
  private case class CompilerFactProcessor(processor: CompilerProcessor) extends FactProcessor[Any, CompilerFact[_]] {
    override def process(fact: CompilerFact[_])(using
        runningFactEngine: RunningFactEngine[Any, CompilerFact[_]]
    ): IO[Unit] =
      processor.process(fact)(using CompilerRunningFactEngine(runningFactEngine))
  }

  private case class CompilerRunningFactEngine(engine: RunningFactEngine[Any, CompilerFact[_]])
      extends CompilationProcess {
    override def registerFact(value: CompilerFact[_]): IO[Unit] = engine.registerFact(value.key(), value)

    override def getFact[K, F <: CompilerFact[K]](key: K): IO[Option[F]] =
      engine.getFact(key).map(_.asInstanceOf[Option[F]])
  }
}
