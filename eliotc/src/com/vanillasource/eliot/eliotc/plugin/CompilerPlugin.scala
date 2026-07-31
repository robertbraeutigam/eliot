package com.vanillasource.eliot.eliotc.plugin

import cats.data.StateT
import cats.effect.IO
import com.vanillasource.eliot.eliotc.processor.{CompilationProcess, CompilerProcessor}
import scopt.{OParser, OParserBuilder}

import scala.annotation.unused

trait CompilerPlugin {
  def commandLineParser(): OParser[?, Configuration] = OParser.builder.programName("eliotc")

  def isSelectedBy(configuration: Configuration): Boolean = false

  def pluginDependencies(@unused configuration: Configuration): Seq[Class[? <: CompilerPlugin]] = Seq.empty

  def configure(): StateT[IO, Configuration, Unit] = StateT.empty

  def initialize(@unused configuration: Configuration): StateT[IO, CompilerProcessor, Unit]

  /** Drive one compilation by demanding whatever this plugin was selected to produce, returning whether that actually
    * came out. A target that declines without erroring (an abort with no errors) produces nothing and must not read as
    * a successful build — so this, not just the error count, is what decides the compiler's exit code. Plugins with no
    * single artefact to produce (a whole-workspace check) always succeed.
    */
  def run(@unused configuration: Configuration, @unused compilation: CompilationProcess): IO[Boolean] = IO.pure(true)
}
