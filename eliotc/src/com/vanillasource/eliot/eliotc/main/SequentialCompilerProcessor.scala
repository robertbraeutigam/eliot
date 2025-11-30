package com.vanillasource.eliot.eliotc.main

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFactKey, CompilerProcessor}

class SequentialCompilerProcessor(processors: Seq[CompilerProcessor]) extends CompilerProcessor {
  override def generate(factKey: CompilerFactKey[_])(using CompilationProcess): IO[Unit] =
    processors.traverse_(_.generate(factKey))
}
