package com.vanillasource.eliot.eliotc.feedback

import cats.effect.std.Console

object User {
  def compilerGenericError[F[_]: Console](msg: String): F[Unit] = Console[F].errorln(msg)

  def compilerGlobalError[F[_]: Console](msg: String): F[Unit] = compilerGenericError(s"eliotc:$msg")
}
