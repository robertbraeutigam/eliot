package com.vanillasource.eliot.eliotc.source

import cats.effect.{IO, Resource}
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}

import scala.io.Source

class SourceContentReader extends CompilerProcessor {
  override def process(fact: CompilerFact)(using process: CompilationProcess): IO[Unit] = fact match
    case SourceFile(file) =>
      Resource.make(IO(Source.fromFile(file)))(source => IO(source.close())).use { source =>
        for {
          content <- IO.blocking(source.getLines().mkString("\n"))
          _       <- process.registerFact(SourceContent(file, content))
        } yield ()
      }
    case _                => IO.unit
}
