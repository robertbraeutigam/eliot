package com.vanillasource.eliot.eliotc.source.file

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.{CompilerError, Logging}
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced

import scala.io.Source

/** Generates the source code for a given File, i.e. reads it from disk. Note, that this generator does not fail if the
  * File is missing, so it can be used to probe whether a file is present. If File is not present, or not readable, this
  * will just silently ignore the issue and not produce a fact.
  */
class FileContentReader extends SingleFactProcessor[FileContent.Key] with Logging {
  // FIXME: remove sourced, SourcedContent does that
  override protected def generateSingleFact(key: FileContent.Key): CompilerIO[FileContent] =
    Resource
      .make(IO(Source.fromFile(key.file)))(source => IO.blocking(source.close()))
      .use { source =>
        IO.blocking(source.getLines()).map { contentLines =>
          FileContent(key.file, Sourced(key.file.toURI, PositionRange.zero, contentLines.mkString("\n")))
        }
      }
      .attempt
      .to[CompilerIO]
      .flatMap {
        case Right(fact) => fact.pure[CompilerIO]
        case Left(_)     =>
          registerCompilerError(
            CompilerError("Could not read file.", Seq.empty, key.file.getPath, "", PositionRange.zero)
          ) >> abort
      }

}
