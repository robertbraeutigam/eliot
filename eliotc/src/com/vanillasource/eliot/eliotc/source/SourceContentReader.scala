package com.vanillasource.eliot.eliotc.source

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor}

import java.nio.file.Path
import scala.io.Source

class SourceContentReader(rootPaths: Seq[Path]) extends CompilerProcessor {
  override def generate(factKey: CompilerFactKey)(using CompilationProcess): IO[Unit] = factKey match {
    case SourceContent.Key(path) => generateContentFor(path)
    case _                       => IO.unit
  }

  private def generateContentFor(path: Path)(using process: CompilationProcess): IO[Unit] = {
    rootPaths.find(_.resolve(path).toFile.isFile).traverse_ { rootPath =>
      val file = rootPath.resolve(path).toFile

      Resource.make(IO(Source.fromFile(file)))(source => IO(source.close())).use { source =>
        for {
          content <- IO.blocking(source.getLines().mkString("\n"))
          _       <- process.registerFact(SourceContent(path, rootPath, content))
        } yield ()
      }
    }
  }
}
