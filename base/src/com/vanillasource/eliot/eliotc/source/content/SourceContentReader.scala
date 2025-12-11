package com.vanillasource.eliot.eliotc.source.content

import cats.effect.{IO, Resource}
import com.vanillasource.eliot.eliotc.source.pos.{PositionRange, Sourced}
import com.vanillasource.eliot.eliotc.CompilationProcess.registerFact
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFactKey, CompilerProcessor}

import java.io.File
import scala.io.Source

class SourceContentReader extends CompilerProcessor {
  override def generate(factKey: CompilerFactKey[?])(using CompilationProcess): IO[Unit] = factKey match {
    case SourceContent.Key(file) => generateContentFor(file)
    case _                       => IO.unit
  }

  private def generateContentFor(file: File)(using CompilationProcess): IO[Unit] = {
    Resource.make(IO(Source.fromFile(file)))(source => IO(source.close())).use { source =>
      for {
        contentLines <- IO.blocking(source.getLines())
        _            <-
          registerFact(
            SourceContent(file, Sourced(file, PositionRange.zero, contentLines.mkString("\n")))
          )
      } yield ()
    }
  }
}
