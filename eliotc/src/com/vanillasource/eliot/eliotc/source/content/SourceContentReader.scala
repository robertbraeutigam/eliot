package com.vanillasource.eliot.eliotc.source.content

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor}

import java.io.File
import java.nio.file.Path
import scala.io.Source

class SourceContentReader(rootPaths: Seq[Path]) extends CompilerProcessor {
  override def generate(factKey: CompilerFactKey)(using CompilationProcess): IO[Unit] = factKey match {
    case SourceContent.Key(file) => generateContentFor(file)
    case _                       => IO.unit
  }

  private def generateContentFor(file: File)(using process: CompilationProcess): IO[Unit] = {
    Resource.make(IO(Source.fromFile(file)))(source => IO(source.close())).use { source =>
      for {
        content <- IO.blocking(source.getLines().mkString("\n"))
        _       <- process.registerFact(SourceContent(file, content))
      } yield ()
    }
  }
}
