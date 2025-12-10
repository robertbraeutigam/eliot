package com.vanillasource.eliot.eliotc.source.scan

import cats.effect.Sync
import cats.Monad
import cats.effect.std.Console
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.feedback.User.*
import com.vanillasource.eliot.eliotc.source.content.SourceContent
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFactKey, CompilerProcessor}

import java.nio.file.Path

class PathScanner[F[_]: {Sync, Console}](rootPaths: Seq[Path]) extends CompilerProcessor[F] with Logging {
  override def generate(factKey: CompilerFactKey[?])(using CompilationProcess[F]): F[Unit] = factKey match {
    case PathScan.Key(path) => scan(path)
    case _                  => Monad[F].unit
  }

  private def scan(path: Path)(using process: CompilationProcess[F]): F[Unit] =
    for {
      files <- Sync[F].blocking(
                 rootPaths
                   .map(_.resolve(path).toFile)
                   .filter(_.isFile)
               )
      _     <- if (files.isEmpty) {
                 compilerGlobalError(s"Could not find path $path at given roots: ${rootPaths.mkString(", ")}")
               } else {
                 debug[F](s"Scanned $path into: ${files.mkString(", ")}") >>
                   process.registerFact(PathScan(path, files))
               }
    } yield ()
}
