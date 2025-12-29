package com.vanillasource.eliot.eliotc.jvm.plugin

import cats.data.StateT
import cats.effect.IO
import com.vanillasource.eliot.eliotc.base.BasePlugin
import com.vanillasource.eliot.eliotc.jvm.classgen.JvmClassGenerator
import com.vanillasource.eliot.eliotc.jvm.jargen.{GenerateExecutableJar, JvmProgramGenerator}
import com.vanillasource.eliot.eliotc.compiler.Compiler
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName}
import com.vanillasource.eliot.eliotc.plugin.Configuration.namedKey
import com.vanillasource.eliot.eliotc.plugin.{CompilerPlugin, Configuration}
import com.vanillasource.eliot.eliotc.processor.impl.SequentialCompilerProcessors
import com.vanillasource.eliot.eliotc.processor.{CompilationProcess, CompilerProcessor}
import scopt.{OParser, OParserBuilder}

import java.nio.file.Path

class JvmPlugin extends CompilerPlugin {
  private val cmdLineBuilder: OParserBuilder[Configuration] = OParser.builder[Configuration]
  import cmdLineBuilder.*

  private val mainKey = namedKey[FunctionFQN]("mainFunction")

  override def commandLineParser(): OParser[?, Configuration] = OParser.sequence(
    cmd("jvm")
      .text("target the jvm backend")
      .children(
        cmd("exe-jar")
          .text("generate executable jar")
          .children(
            opt[String]('m', "main-module")
              .required()
              .text("module that has a suitable main method")
              .action((moduleName, config) => config.set(mainKey, FunctionFQN(ModuleName.parse(moduleName), "main")))
          )
      )
  )

  override def initialize(configuration: Configuration): StateT[IO, CompilerProcessor, Unit] =
    StateT
      .modify(superProcessor =>
        SequentialCompilerProcessors(
          Seq(
            superProcessor,
            JvmClassGenerator(),
            JvmProgramGenerator(
              configuration.get(Compiler.targetPathKey).get,
              // TODO: This is not clean, just selecting a random path to insert main source into
              configuration.get(BasePlugin.pathKey).flatMap(_.headOption).getOrElse(Path.of("."))
            )
          )
        )
      )

  override def isSelectedBy(configuration: Configuration): Boolean = configuration.contains(mainKey)

  override def pluginDependencies(configuration: Configuration): Seq[Class[? <: CompilerPlugin]] = Seq(
    classOf[BasePlugin]
  )

  override def run(configuration: Configuration, compilation: CompilationProcess): IO[Unit] =
    compilation.getFact(GenerateExecutableJar.Key(configuration.get(mainKey).get)).void
}
