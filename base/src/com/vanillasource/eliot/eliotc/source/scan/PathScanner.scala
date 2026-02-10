package com.vanillasource.eliot.eliotc.source.scan

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.feedback.User.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor
import com.vanillasource.eliot.eliotc.source.stat.FileStat

import java.nio.file.Path

class PathScanner(rootPaths: Seq[Path]) extends SingleFactProcessor[PathScan.Key] with Logging {
  override protected def generateSingleFact(key: PathScan.Key): CompilerIO[PathScan] =
    for {
      contentFacts <- rootPaths
                        .map(_.resolve(key.path).toFile)
                        .toList
                        .traverse(file => getFactOrAbort(FileStat.Key(file)))
      files         = contentFacts.filter(_.lastModified.isDefined).map(_.file)
      _            <- debug[CompilerIO](s"Found files: ${files.mkString(", ")}")
      _            <- (compilerGlobalError(s"Could not find path ${key.path} at given roots: ${rootPaths.mkString(", ")}")
                        .to[CompilerIO] >> abort).whenA(files.isEmpty)
    } yield PathScan(key.path, files.map(_.toURI))
}
