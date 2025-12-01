package com.vanillasource.eliot.eliotc.layer

import cats.effect.IO
import scopt.OParser

trait Layer {
  def commandLineParser(): OParser[_, Configuration]

  def initialize(configuration: Configuration, system: CompilerSystem): IO[Unit]
}
