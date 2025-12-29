package com.vanillasource.eliot.eliotc.processor.impl

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.processor.{CompilationProcess, CompilerFactKey, CompilerProcessor}

class SequentialCompilerProcessors(processors: Seq[CompilerProcessor]) extends CompilerProcessor {
  override def generate(factKey: CompilerFactKey[?])(using CompilationProcess): CompilerIO[Unit] =
    processors.traverse_(_.generate(factKey))
}
