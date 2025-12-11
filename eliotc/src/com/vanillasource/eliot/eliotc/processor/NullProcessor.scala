package com.vanillasource.eliot.eliotc.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFactKey, CompilerProcessor}

class NullProcessor extends CompilerProcessor {
  override def generate(factKey: CompilerFactKey[?])(using CompilationProcess): IO[Unit] = IO.unit
}
