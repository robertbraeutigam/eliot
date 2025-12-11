package com.vanillasource.eliot.eliotc.processor

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFactKey, CompilerProcessor}

class SequentialCompilerProcessors(processors: Seq[CompilerProcessor]) extends CompilerProcessor {
  override def generate(factKey: CompilerFactKey[?])(using CompilationProcess): IO[Unit] =
    processors.traverse_(_.generate(factKey))
}
