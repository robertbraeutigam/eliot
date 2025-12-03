package com.vanillasource.eliot.eliotc.plugin

import cats.data.StateT
import cats.effect.IO
import com.vanillasource.eliot.eliotc.CompilerProcessor
import scopt.OParser

trait CompilerPlugin {
  def commandLineParser(): OParser[_, Configuration]

  def isSelectedBy(configuration: Configuration): Boolean = false

  def pluginDependencies(configuration: Configuration): Seq[Class[_ <: CompilerPlugin]] = Seq.empty

  def initialize(configuration: Configuration): StateT[IO, CompilerProcessor, Unit]
}
