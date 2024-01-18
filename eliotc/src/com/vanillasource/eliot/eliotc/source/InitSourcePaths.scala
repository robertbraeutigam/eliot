package com.vanillasource.eliot.eliotc.source

import cats.effect.IO
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor, CompilerSignal, Init}

import java.io.File

/** Puts all the source paths as facts into the compiler.
  */
class InitSourcePaths(paths: Seq[File]) extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact[_])(using process: CompilationProcess): IO[Unit] = fact match
    case Init => process.registerFacts(paths.map(SourcePath.apply))
    case _    => IO.unit
}
