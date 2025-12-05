package com.vanillasource.eliot.eliotc.source.resolve

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.User.*
import com.vanillasource.eliot.eliotc.source.content.SourceContent
import com.vanillasource.eliot.eliotc.unify.UnifiedSourceAST
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFactKey, CompilerProcessor}

import java.io.File
import java.nio.file.Path

class ResolvedSourceContentReader(rootPaths: Seq[Path]) extends CompilerProcessor {
  override def generate(factKey: CompilerFactKey[_])(using CompilationProcess): IO[Unit] = factKey match {
    case ResolvedSourceContent.Key(path) => generateContentFor(path)
    case _                               => IO.unit
  }

  private def generateContentFor(path: Path)(using process: CompilationProcess): IO[Unit] =
    for {
      files    <- potentialFiles(path)
      contents <-
        files.traverse[IO, Option[SourceContent]](file => process.getFact(SourceContent.Key(file))).map(_.flatten)
      _        <- if (contents.isEmpty) {
                    compilerGlobalError(s"Could not find path $path at given roots: ${rootPaths.mkString(", ")}")
                  } else {
                    process.registerFact(ResolvedSourceContent(path, contents.map(_.content)))
                  }
    } yield ()

  private def potentialFiles(path: Path): IO[Seq[File]] = IO.blocking {
    rootPaths
      .map(_.resolve(path).toFile)
      .filter(_.isFile)
  }
}
