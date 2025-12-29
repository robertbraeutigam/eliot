package com.vanillasource.eliot.eliotc.processor.impl

import cats.effect.IO
import com.vanillasource.eliot.eliotc.processor.{CompilationProcess, CompilerFactKey, CompilerProcessor}

class NullProcessor extends CompilerProcessor {
  override def generate(factKey: CompilerFactKey[?])(using CompilationProcess): IO[Unit] = IO.unit
}
