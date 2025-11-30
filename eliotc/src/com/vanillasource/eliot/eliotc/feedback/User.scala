package com.vanillasource.eliot.eliotc.feedback

import cats.effect.IO
import cats.effect.std.Console

object User {
  def compilerGenericError(msg: String): IO[Unit] = Console[IO].errorln(msg)

  def compilerGlobalError(msg: String): IO[Unit] = compilerGenericError(s"eliotc:$msg")
}
