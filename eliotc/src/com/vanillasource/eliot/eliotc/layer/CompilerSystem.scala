package com.vanillasource.eliot.eliotc.layer

import cats.effect.IO
import com.vanillasource.eliot.eliotc.CompilerProcessor

trait CompilerSystem {
  def registerProcessor(compilerProcessor: CompilerProcessor): IO[Unit]
}
