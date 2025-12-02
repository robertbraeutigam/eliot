package com.vanillasource.eliot.eliotc.layer

import cats.data.StateT
import cats.effect.IO
import com.vanillasource.eliot.eliotc.CompilerProcessor
import scopt.OParser

trait Layer {
  def commandLineParser(): OParser[_, Configuration]

  def activate(): StateT[IO, Configuration, Seq[Class[_ <: Layer]]]

  def initialize(configuration: Configuration): StateT[IO, CompilerProcessor, Unit]
}
