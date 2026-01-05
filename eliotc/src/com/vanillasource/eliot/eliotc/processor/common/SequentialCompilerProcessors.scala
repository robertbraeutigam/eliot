package com.vanillasource.eliot.eliotc.processor.common

import cats.Monad
import cats.data.{EitherT, ReaderT, StateT}
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.CompilerError
import com.vanillasource.eliot.eliotc.processor.CompilerIO.{CompilerIO, abort, currentErrors, registerCompilerError}
import com.vanillasource.eliot.eliotc.processor.{CompilationProcess, CompilerFactKey, CompilerProcessor}

class SequentialCompilerProcessors(processors: Seq[CompilerProcessor]) extends CompilerProcessor {
  override def generate(factKey: CompilerFactKey[?]): CompilerIO[Unit] =
    for {
      initialErrors <- currentErrors
      _             <- processors.traverse_(processor => runIsolated(processor.generate(factKey)))
      finalErrors   <- currentErrors
      _             <- if (finalErrors.size > initialErrors.size) abort[Unit] else Monad[CompilerIO].unit
    } yield ()

  private def runIsolated(processor: CompilerIO[Unit]): CompilerIO[Unit] =
    for {
      process <- ReaderT.ask[StateStage, CompilationProcess]
      result <- ReaderT.liftF[StateStage, CompilationProcess, Either[cats.data.Chain[CompilerError], (cats.data.Chain[CompilerError], Unit)]](
                  StateT.liftF(
                    EitherT.liftF(processor.run(process).run(cats.data.Chain.empty).value)
                  )
                )
      _ <- result match {
             case Left(errors)          => errors.traverse_(registerCompilerError)
             case Right((newErrors, _)) => newErrors.traverse_(registerCompilerError)
           }
    } yield ()

  private type EitherStage[T] = EitherT[IO, cats.data.Chain[CompilerError], T]
  private type StateStage[T]  = StateT[EitherStage, cats.data.Chain[CompilerError], T]
}
