package com.vanillasource.eliot.eliotc.feedback

import cats.effect.IO
import cats.effect.std.Console

trait User {
  self: Logging =>

  def compilerError(msg: String): IO[Unit] =
    error(s"(console) $msg") >>
      Console[IO].errorln(s"eliotc: $msg")
}
