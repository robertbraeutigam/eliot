package com.vanillasource.eliot.eliotc.source.scan

import cats.Monad
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.feedback.User.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.source.content.SourceContent

import java.nio.file.Path

class PathScanner(rootPaths: Seq[Path]) extends SingleKeyTypeProcessor[PathScan.Key] with Logging {
  override protected def generateFact(key: PathScan.Key): CompilerIO[Unit] =
    for {
      contentFacts <- rootPaths
                        .map(_.resolve(key.path).toFile)
                        .toList
                        .traverse(file => getFact(SourceContent.Key(file)))
      files         = contentFacts.flatten.map(_.file)
      _            <- if (files.isEmpty) {
                        compilerGlobalError(s"Could not find path ${key.path} at given roots: ${rootPaths.mkString(", ")}")
                          .to[CompilerIO]
                      } else {
                        registerFactIfClear(PathScan(key.path, files))
                      }
    } yield ()
}
