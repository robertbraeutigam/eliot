package com.vanillasource.eliot.eliotc.source.content

import cats.effect.{IO, Resource}
import cats.Monad
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.{CompilerFactKey, CompilerProcessor}

import java.io.File
import scala.io.Source

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
      .to[CompilerIO]
      .flatMap(registerFactIfClear)
}
