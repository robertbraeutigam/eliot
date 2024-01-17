package com.vanillasource.eliot.eliotc

import cats.effect.IO

trait CompilerEngine {
  def getFact[K, F <: CompilerFact[K]](key: K): IO[Option[F]]

  def registerFact(value: CompilerFact[_]): IO[Unit]
}
