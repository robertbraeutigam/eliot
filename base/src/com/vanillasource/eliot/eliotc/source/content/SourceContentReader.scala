package com.vanillasource.eliot.eliotc.source.content

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.{CompilerError, Logging}
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.{SingleFactProcessor, SingleKeyTypeProcessor}
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerErrorWithContent
import com.vanillasource.eliot.eliotc.source.file.FileContent

import java.io.File
import scala.io.Source

/** Generates the source code for a given File, i.e. reads it from disk. Note, that this generator does not fail if the
  * File is missing, so it can be used to probe whether a file is present. If File is not present, or not readable, this
  * will just silently ignore the issue and not produce a fact.
  */
class SourceContentReader extends SingleFactProcessor[SourceContent.Key] with Logging {
  override protected def generateSingleFact(key: SourceContent.Key): CompilerIO[SourceContent] =
    if (key.uri.getScheme === "file") {
      // Get file based content
      for {
        fileContent <- getFactOrAbort(FileContent.Key(new File(key.uri.toURL.getFile)))
      } yield SourceContent(key.uri, fileContent.content)
    } else {
      // Scheme is not handled
      compilerErrorWithContent(
        Sourced(key.uri, PositionRange.zero, s"Unhandled source scheme '${key.uri.getScheme}'."),
        ""
      ) >> abort
    }
}
