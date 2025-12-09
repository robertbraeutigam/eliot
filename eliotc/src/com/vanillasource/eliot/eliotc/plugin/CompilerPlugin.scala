package com.vanillasource.eliot.eliotc.plugin

import cats.Applicative
import cats.data.StateT
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerProcessor}
import scopt.OParser

trait CompilerPlugin {
  def commandLineParser(): OParser[_, Configuration]

  def isSelectedBy(configuration: Configuration): Boolean = false

  def pluginDependencies(configuration: Configuration): Seq[Class[_ <: CompilerPlugin]] = Seq.empty

  def configure[F[_]: Applicative](): StateT[F, Configuration, Unit] = StateT.empty

  def initialize[F[_]](configuration: Configuration): StateT[F, CompilerProcessor[F], Unit]

  def run[F[_]: Applicative](configuration: Configuration, compilation: CompilationProcess[F]): F[Unit] =
    Applicative[F].unit
}
