package com.vanillasource.eliot.eliotc.source.resource

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.{CompilerError, Logging}
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced

import java.io.InputStream
import java.nio.charset.StandardCharsets

class ResourceContentReader extends SingleFactProcessor[ResourceContent.Key] with Logging {
  override protected def generateSingleFact(key: ResourceContent.Key): CompilerIO[ResourceContent] =
    Resource
      .make(IO(key.uri.toURL.openStream()))(stream => IO.blocking(stream.close()))
      .use { stream =>
        IO.blocking(new String(stream.readAllBytes(), StandardCharsets.UTF_8)).map { content =>
          ResourceContent(key.uri, Sourced(key.uri, PositionRange.zero, content))
        }
      }
      .attempt
      .to[CompilerIO]
      .flatMap {
        case Right(fact) => fact.pure[CompilerIO]
        case Left(_)     =>
          registerCompilerError(
            CompilerError("Could not read resource.", Seq.empty, key.uri.toString, "", PositionRange.zero)
          ) >> abort
      }
}
