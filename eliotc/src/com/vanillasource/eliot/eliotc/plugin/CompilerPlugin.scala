package com.vanillasource.eliot.eliotc.plugin

import cats.data.StateT
import cats.effect.IO
import com.vanillasource.eliot.eliotc.processor.{CompilationProcess, CompilerProcessor}
import scopt.OParser

import scala.annotation.unused

trait CompilerPlugin {
  def commandLineParser(): OParser[?, Configuration]

  def isSelectedBy(configuration: Configuration): Boolean = false

  def pluginDependencies(@unused configuration: Configuration): Seq[Class[? <: CompilerPlugin]] = Seq.empty

  def configure(): StateT[IO, Configuration, Unit] = StateT.empty

  def initialize(@unused configuration: Configuration): StateT[IO, CompilerProcessor, Unit]

  def run(@unused configuration: Configuration, @unused compilation: CompilationProcess): IO[Unit] = IO.unit
}
