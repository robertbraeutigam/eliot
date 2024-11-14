package com.vanillasource.eliot.eliotc.processor

import cats.effect.{IO, Ref}
import com.vanillasource.eliot.eliotc.source.SourcedError
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor}

/** Tracks facts registered locally in the processor.
  */
abstract class TrackingCompilerProcessor extends CompilerProcessor {
  final override def process(fact: CompilerFact)(using delegateProcess: CompilationProcess): IO[Unit] = for {
    hasGeneratedErrorsFlag <- Ref.of[IO, Boolean](false)
    _                      <- processTrack(fact)(using
                                new TrackingCompilationProcess() {
                                  override def registerFact(value: CompilerFact): IO[Unit] = (value match
                                    case SourcedError(_) => hasGeneratedErrorsFlag.set(true)
                                    case _               => IO.unit
                                  ) >> delegateProcess.registerFact(value)

                                  override def getFact[K <: CompilerFactKey](key: K): IO[Option[key.FactType]] = delegateProcess.getFact(key)

                                  override def hasGeneratedErrors: IO[Boolean] = hasGeneratedErrorsFlag.get
                                }
                              )
  } yield ()

  def processTrack(fact: CompilerFact)(using TrackingCompilationProcess): IO[Unit]
}
