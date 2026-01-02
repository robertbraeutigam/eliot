package com.vanillasource.eliot.eliotc.source.scan

import cats.Monad
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.feedback.User.*
import com.vanillasource.eliot.eliotc.processor.{CompilerFactKey, CompilerProcessor}
import com.vanillasource.eliot.eliotc.source.content.SourceContent

import java.nio.file.Path

class PathScanner(rootPaths: Seq[Path]) extends CompilerProcessor with Logging {
  override def generate(factKey: CompilerFactKey[?]): CompilerIO[Unit] = factKey match {
    case PathScan.Key(path) => scan(path)
    case _                  => Monad[CompilerIO].unit
  }

  private def scan(path: Path): CompilerIO[Unit] =
    for {
      contentFacts <- rootPaths
                        .map(_.resolve(path).toFile)
                        .toList
                        .traverse(file => getFact(SourceContent.Key(file)))
      files         = contentFacts.flatten.map(_.file)
      _            <- if (files.isEmpty) {
                        compilerGlobalError(s"Could not find path $path at given roots: ${rootPaths.mkString(", ")}")
                          .to[CompilerIO]
                      } else {
                        registerFactIfClear(PathScan(path, files))
                      }
    } yield ()
}
