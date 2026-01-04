package com.vanillasource.eliot.eliotc.processor.common

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.processor.{CompilerFactKey, CompilerProcessor}

class SequentialCompilerProcessors(processors: Seq[CompilerProcessor]) extends CompilerProcessor {
  override def generate(factKey: CompilerFactKey[?]): CompilerIO[Unit] =
    processors.traverse_(_.generate(factKey))
}
