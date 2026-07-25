package com.vanillasource.eliot.eliotc.jvm.plugin

import cats.data.StateT
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.plugin.LangPlugin
import com.vanillasource.eliot.eliotc.stdlib.plugin.StdlibPlugin
import com.vanillasource.eliot.eliotc.jvm.jargen.{
  GenerateExecutableJar,
  JvmProgramGenerator,
  SyntheticMainMount,
  SyntheticMainSourceProcessor
}
import com.vanillasource.eliot.eliotc.compiler.Compiler
import com.vanillasource.eliot.eliotc.compiler.cache.OutputFileStatProcessor
import com.vanillasource.eliot.eliotc.monomorphize.fact.RunBoundaryFunction
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.JvmClassGenerator
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.plugin.Configuration.namedKey
import com.vanillasource.eliot.eliotc.plugin.{CompilerPlugin, Configuration}
import com.vanillasource.eliot.eliotc.processor.common.SequentialCompilerProcessors
import com.vanillasource.eliot.eliotc.processor.{CompilationProcess, CompilerProcessor}
import com.vanillasource.eliot.eliotc.source.scan.PathScanner
import scopt.{OParser, OParserBuilder}

class JvmPlugin extends CompilerPlugin {
  private val cmdLineBuilder: OParserBuilder[Configuration] = OParser.builder[Configuration]
  import cmdLineBuilder.*

  private val mainKey = namedKey[ValueFQN]("mainFunction")

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
              .action((moduleName, config) => config.set(mainKey, ValueFQN(ModuleName.parse(moduleName), QualifiedName("main", Qualifier.Default))))
          )
      )
  )

  /** Mount the synthesized `main.els` entry-point module into the runtime scan pool, and register the platform run
    * boundary `runMain` as a carrier-capture tag (effects-as-channel source (ii),
    * [[com.vanillasource.eliot.eliotc.monomorphize.fact.RunBoundaryFunction]]): the synthesized entry calls
    * `runMain(main)`, whose `io: IO[A]` parameter binds the user `main`'s carrier to `IO`, and this registration lets the
    * lang checker route that capture through the join solver without ever naming the jvm-owned `IO`. All `configure()`s
    * run before any `initialize`, so `LangPlugin` sees both contributions when it builds the pipeline.
    */
  override def configure(): StateT[IO, Configuration, Unit] =
    StateT.modify(configuration =>
      if (configuration.contains(mainKey))
        configuration
          .updatedWith(
            PathScanner.extraRuntimeMountsKey,
            mounts => (mounts.getOrElse(Seq.empty) :+ new SyntheticMainMount).some
          )
          .updatedWith(
            RunBoundaryFunction.configKey,
            boundaries => (boundaries.getOrElse(Set.empty) + SyntheticMainSourceProcessor.runMainVfqn).some
          )
      else configuration
    )

  override def initialize(configuration: Configuration): StateT[IO, CompilerProcessor, Unit] =
    StateT
      .modify(superProcessor =>
        SequentialCompilerProcessors(
          Seq(
            superProcessor,
            OutputFileStatProcessor(),
            JvmClassGenerator(),
            JvmProgramGenerator(configuration.get(Compiler.targetPathKey).get)
          ) ++ configuration
            .get(mainKey)
            .map(SyntheticMainSourceProcessor(_))
            .toSeq
        )
      )

  override def isSelectedBy(configuration: Configuration): Boolean = configuration.contains(mainKey)

  override def pluginDependencies(configuration: Configuration): Seq[Class[? <: CompilerPlugin]] = Seq(
    classOf[LangPlugin],
    classOf[StdlibPlugin]
  )

  override def run(configuration: Configuration, compilation: CompilationProcess): IO[Unit] =
    compilation.getFact(GenerateExecutableJar.Key(configuration.get(mainKey).get)).void
}

object JvmPlugin
