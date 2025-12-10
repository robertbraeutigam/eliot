package com.vanillasource.eliot.eliotc.plugin

import cats.Applicative
import cats.data.StateT
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerProcessor}
import scopt.OParser

import scala.annotation.nowarn

trait CompilerPlugin {
  def commandLineParser(): OParser[?, Configuration]

  def isSelectedBy(@nowarn configuration: Configuration): Boolean = false

  def pluginDependencies(@nowarn configuration: Configuration): Seq[Class[? <: CompilerPlugin]] = Seq.empty

  def configure[F[_]: Applicative](): StateT[F, Configuration, Unit] = StateT.empty

  def initialize[F[_]](@nowarn configuration: Configuration): StateT[F, CompilerProcessor[F], Unit]

  def run[F[_]: Applicative](
      @nowarn configuration: Configuration,
      @nowarn compilation: CompilationProcess[F]
  ): F[Unit] =
    Applicative[F].unit
}
