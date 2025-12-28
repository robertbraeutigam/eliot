package com.vanillasource.eliot.eliotc.source.scan

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.CompilationProcess.{getFact, registerFact}
import com.vanillasource.eliot.eliotc.feedback.User.*
import com.vanillasource.eliot.eliotc.source.content.SourceContent
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFactKey, CompilerProcessor}

import java.nio.file.Path

class PathScanner(rootPaths: Seq[Path]) extends CompilerProcessor with Logging {
  override def generate(factKey: CompilerFactKey[?])(using CompilationProcess): IO[Unit] = factKey match {
    case PathScan.Key(path) => scan(path)
    case _                  => IO.unit
  }

  private def scan(path: Path)(using CompilationProcess): IO[Unit] =
    for {
      files <- rootPaths
                 .map(_.resolve(path).toFile)
                 .traverse(file => getFact(SourceContent.Key(file)))
                 .map(_.flatten)
                 .map(_.map(_.file))
      _     <- if (files.isEmpty) {
                 compilerGlobalError(s"Could not find path $path at given roots: ${rootPaths.mkString(", ")}")
               } else {
                 debug[IO](s"Scanned $path into: ${files.mkString(", ")}") >> registerFact(PathScan(path, files))
               }
    } yield ()
}
