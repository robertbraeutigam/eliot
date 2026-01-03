package com.vanillasource.eliot.eliotc.source.content

import cats.effect.{IO, Resource}
import cats.Monad
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.{CompilerFactKey, CompilerProcessor}

import java.io.File
import scala.io.Source

/** Generates the source code for a given File, i.e. reads it from disk. Note, that this generator does not fail if the
  * File is missing, so it can be used to probe whether a file is present. If File is not present, or not readable, this
  * will just silently ignore the issue and not produce a fact.
  */
class SourceContentReader extends CompilerProcessor with Logging {
  override def generate(factKey: CompilerFactKey[?]): CompilerIO[Unit] = factKey match {
    case SourceContent.Key(file) => generateContentFor(file)
    case _                       => Monad[CompilerIO].unit
  }

  private def generateContentFor(file: File): CompilerIO[Unit] =
    Resource
      .make(IO(Source.fromFile(file)))(source => IO.blocking(source.close()))
      .use { source =>
        IO.blocking(source.getLines()).map { contentLines =>
          SourceContent(file, Sourced(file, PositionRange.zero, contentLines.mkString("\n")))
        }
      }
      .attempt
      .to[CompilerIO]
      .flatMap {
        case Right(fact) => registerFactIfClear(fact)
        case Left(_)     => Monad[CompilerIO].unit
      }
}
