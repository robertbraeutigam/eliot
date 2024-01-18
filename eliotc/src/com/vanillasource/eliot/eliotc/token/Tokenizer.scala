package com.vanillasource.eliot.eliotc.token

import cats.effect.IO
import com.vanillasource.eliot.eliotc.source.SourceContent
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}

class Tokenizer extends CompilerProcessor {
  override def process(fact: CompilerFact[_])(using CompilationProcess): IO[Unit] = fact match {
    case SourceContent(file, content) => ???
    case _                            => IO.unit
  }
}
