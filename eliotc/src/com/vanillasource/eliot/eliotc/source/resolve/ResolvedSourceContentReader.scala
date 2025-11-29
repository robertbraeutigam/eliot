package com.vanillasource.eliot.eliotc.source.resolve

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.source.content.SourceContent
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFactKey, CompilerProcessor}

import java.nio.file.Path
import scala.io.Source

class ResolvedSourceContentReader(rootPaths: Seq[Path]) extends CompilerProcessor {
  override def generate(factKey: CompilerFactKey)(using CompilationProcess): IO[Unit] = factKey match {
    case ResolvedSourceContent.Key(path) => generateContentFor(path)
    case _                               => IO.unit
  }

  private def generateContentFor(path: Path)(using process: CompilationProcess): IO[Unit] = {
    rootPaths.find(_.resolve(path).toFile.isFile).traverse_ { rootPath =>
      val file = rootPath.resolve(path).toFile

      process
        .getFact(SourceContent.Key(file))
        .map(_.traverse_(content => process.registerFact(ResolvedSourceContent(path, rootPath, content.content))))
    }
  }
}
