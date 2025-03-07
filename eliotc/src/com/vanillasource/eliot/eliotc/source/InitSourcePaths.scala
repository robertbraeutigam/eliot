package com.vanillasource.eliot.eliotc.source

import cats.effect.IO
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor, CompilerSignal, Init}

import java.io.File

/** Puts all the source paths as facts into the compiler.
  */
class InitSourcePaths(rootPaths: Seq[File]) extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using process: CompilationProcess): IO[Unit] = fact match
    case Init => process.registerFacts(rootPaths.map(p => SourcePath(p, p)))
    case _    => IO.unit
}
