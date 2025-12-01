package com.vanillasource.eliot.eliotc.jvm

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.layer.Configuration.namedKey
import com.vanillasource.eliot.eliotc.layer.{CompilerSystem, Configuration, Layer}
import com.vanillasource.eliot.eliotc.main.Main
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName}
import com.vanillasource.eliot.eliotc.processor.SequentialCompilerProcessors
import scopt.{OParser, OParserBuilder}

import java.io.File

class JvmLayer extends Layer {
  private val cmdLineBuilder: OParserBuilder[Configuration] = OParser.builder[Configuration]
  import cmdLineBuilder.*

  private val mainKey = namedKey[FunctionFQN]("mainFunction")

  override def commandLineParser(): OParser[_, Configuration] = OParser.sequence(
    opt[String]('m', "main-module")
      .text("module that has a suitable main method")
      .action((moduleName, config) => config.set(mainKey, FunctionFQN(ModuleName.parse(moduleName), "main")))
  )

  override def initialize(configuration: Configuration, system: CompilerSystem): IO[Unit] =
    configuration.get(mainKey) match {
      case Some(mainFfqn) =>
        system.registerProcessor(
          SequentialCompilerProcessors(
            Seq(
              JvmClassGenerator(),
              JvmProgramGenerator(mainFfqn, configuration.get(Main.targetPathKey).get)
            )
          )
        )
      case None           => ???
    }
}
