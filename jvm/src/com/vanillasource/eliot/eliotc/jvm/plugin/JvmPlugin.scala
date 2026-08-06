package com.vanillasource.eliot.eliotc.jvm.plugin

import cats.data.StateT
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.compiler.cache.codec.FactKeyCodecs
import com.vanillasource.eliot.eliotc.jvm.codec.JvmFactCodecs
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
import com.vanillasource.eliot.eliotc.row.RunBoundaryFunctions
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.JvmClassGenerator
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.plugin.Configuration.demandScopedKey
import com.vanillasource.eliot.eliotc.plugin.{CompilerPlugin, Configuration}
import com.vanillasource.eliot.eliotc.processor.common.SequentialCompilerProcessors
import com.vanillasource.eliot.eliotc.processor.{CompilationProcess, CompilerProcessor}
import com.vanillasource.eliot.eliotc.source.scan.PathScanner
import scopt.{OParser, OParserBuilder}

class JvmPlugin extends CompilerPlugin {
  private val cmdLineBuilder: OParserBuilder[Configuration] = OParser.builder[Configuration]
  import cmdLineBuilder.*

  // Demand-scoped, not contributing: `-m` selects the monomorphization root, so two examples over the same roots and
  // backend compute identical values for every shared fact and may share one accumulating cache file. The main enters
  // the fact graph only through the synthetic-entry `SourceContent` leaf (SyntheticMainSourceProcessor), which is
  // always regenerated and equality-checked, so a shared cache self-heals per main (see Configuration.demandScopedKey).
  private val mainKey = demandScopedKey[ValueFQN]("mainFunction")

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
    * boundary `runMain` as a carrier capture ([[com.vanillasource.eliot.eliotc.row.RunBoundaryFunctions]], carrier
    * recognition source (ii)): the synthesized entry calls `runMain(main)`, whose `io: IO[A]` parameter hosts the user
    * `main`'s computation, and this registration lets the row elaborator treat that slot as a capture without ever
    * naming the jvm-owned `IO`. All `configure()`s run before any `initialize`, so `LangPlugin` sees both
    * contributions when it builds the pipeline.
    */
  override def configure(): StateT[IO, Configuration, Unit] =
    StateT.modify(configuration =>
      (if (configuration.contains(mainKey))
        configuration
          .updatedWith(
            PathScanner.extraRuntimeMountsKey,
            mounts => (mounts.getOrElse(Seq.empty) :+ new SyntheticMainMount).some
          )
          .updatedWith(
            RunBoundaryFunctions.configKey,
            boundaries => (boundaries.getOrElse(Set.empty) + SyntheticMainSourceProcessor.runMainVfqn).some
          )
      else configuration)
        .updatedWith(
          FactKeyCodecs.configKey,
          codecs => (codecs.getOrElse(Map.empty) ++ JvmFactCodecs.keyCodecs).some
        )
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

  override def run(configuration: Configuration, compilation: CompilationProcess): IO[Boolean] =
    compilation.getFact(GenerateExecutableJar.Key(configuration.get(mainKey).get)).map(_.isDefined)
}

object JvmPlugin
