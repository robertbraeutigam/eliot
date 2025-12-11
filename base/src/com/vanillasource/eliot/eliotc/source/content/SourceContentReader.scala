package com.vanillasource.eliot.eliotc.source.content

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.source.pos.{Position, PositionRange, Sourced}
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor}

import java.io.File
import java.nio.file.Path
import scala.io.Source

class SourceContentReader[F[_]: Sync] extends CompilerProcessor[F] {
  override def generate(factKey: CompilerFactKey[?])(using CompilationProcess[F]): F[Unit] = factKey match {
    case SourceContent.Key(file) => generateContentFor(file)
    case _                       => Monad[F].unit
  }

  private def generateContentFor(file: File)(using process: CompilationProcess): F[Unit] = {
    Resource.make(Monad[F].delay(Source.fromFile(file)))(source => Sync[F].blocking(source.close())).use { source =>
      for {
        contentLines <- Sync[F].blocking(source.getLines())
        _            <-
          process.registerFact(
            SourceContent(file, Sourced(file, PositionRange.zero, contentLines.mkString("\n")))
          )
      } yield ()
    }
  }
}
