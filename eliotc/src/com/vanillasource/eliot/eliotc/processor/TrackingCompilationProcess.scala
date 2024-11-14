package com.vanillasource.eliot.eliotc.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact}

trait TrackingCompilationProcess extends CompilationProcess {
  def hasGeneratedErrors: IO[Boolean]

  def registerFactOnSuccess(value: CompilerFact): IO[Unit] =
    hasGeneratedErrors.ifM(IO.unit, registerFact(value))
}
