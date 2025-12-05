package com.vanillasource.eliot.eliotc.source.scan

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.User.*
import com.vanillasource.eliot.eliotc.source.content.SourceContent
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFactKey, CompilerProcessor}

import java.nio.file.Path

class PathScanner(rootPaths: Seq[Path]) extends CompilerProcessor {
  override def generate(factKey: CompilerFactKey[_])(using CompilationProcess): IO[Unit] = factKey match {
    case PathScan.Key(path) => scan(path)
    case _                  => IO.unit
  }

  private def scan(path: Path)(using process: CompilationProcess): IO[Unit] =
    for {
      files <- IO.blocking(
                 rootPaths
                   .map(_.resolve(path).toFile)
                   .filter(_.isFile)
               )
      _     <- if (files.isEmpty) {
                 compilerGlobalError(s"Could not find path $path at given roots: ${rootPaths.mkString(", ")}")
               } else {
                 process.registerFact(PathScan(path, files))
               }
    } yield ()
}
