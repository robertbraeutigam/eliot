package com.vanillasource.eliot.eliotc.feedback

import cats.effect.IO
import cats.effect.std.Console

object User {
  def compilerGenericError(msg: String)(using console: Console[IO]): IO[Unit] = console.errorln(msg)

  def compilerGlobalError(msg: String)(using Console[IO]): IO[Unit] = compilerGenericError(s"eliotc:$msg")
}
