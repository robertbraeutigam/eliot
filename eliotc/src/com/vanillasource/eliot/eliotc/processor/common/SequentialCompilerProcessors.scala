package com.vanillasource.eliot.eliotc.processor.common

import cats.Monad
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.{CompilerIO, abort, currentErrors, recover}
import com.vanillasource.eliot.eliotc.processor.{CompilerFactKey, CompilerProcessor}

class SequentialCompilerProcessors(processors: Seq[CompilerProcessor]) extends CompilerProcessor {
  override def generate(factKey: CompilerFactKey[?]): CompilerIO[Unit] =
    for {
      initialErrors <- currentErrors
      _             <- processors.traverse_(processor => runIsolated(processor.generate(factKey)))
      finalErrors   <- currentErrors
      _             <- if (finalErrors.size > initialErrors.size) abort[Unit] else Monad[CompilerIO].unit
    } yield ()
  private def runIsolated(processor: CompilerIO[Unit]): CompilerIO[Unit] =
    recover(processor)(())
}
