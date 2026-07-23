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

  /** Mount the synthesized `main.els` entry-point module into the runtime scan pool, and — under the effect-channel
    * flag — contribute this platform's **base effect carrier** (`eliot.jvm.IO`) so the effects-as-channel weaver can
    * assign it (`LangPlugin.baseCarrierKey`; `LangPlugin` is platform-agnostic and never names a carrier itself). All
    * `configure()`s run before any `initialize`, and the effect-channel flag is already parsed here, so `LangPlugin`
    * sees both contributions when it builds the pipeline.
    */
  override def configure(): StateT[IO, Configuration, Unit] =
    StateT.modify(configuration =>
      if (configuration.contains(mainKey))
        withBaseCarrier(
          configuration.updatedWith(
            PathScanner.extraRuntimeMountsKey,
            mounts => (mounts.getOrElse(Seq.empty) :+ new SyntheticMainMount).some
          )
        )
      else configuration
    )

  /** Contribute the jvm base effect carrier and the synthesized entry-point FQN when the effect-channel flag is on; a
    * no-op otherwise (the default carrier path names its carrier by ordinary unification, and its synthetic main runs
    * the carrier in Eliot source, so neither config is needed).
    */
  private def withBaseCarrier(configuration: Configuration): Configuration =
    if (configuration.getOrElse(LangPlugin.effectChannelKey, false))
      configuration
        .set(LangPlugin.baseCarrierKey, JvmPlugin.baseCarrierFQN)
        .set(LangPlugin.entryPointKey, SyntheticMainSourceProcessor.syntheticMainVfqn)
    else configuration

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
            .map(SyntheticMainSourceProcessor(_, configuration.getOrElse(LangPlugin.effectChannelKey, false)))
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

object JvmPlugin {

  /** This platform's base effect carrier as a type-constructor `ValueFQN`: `eliot.jvm.IO`'s `IO` (the `Qualifier.Type`
    * namespace). Contributed to `LangPlugin.baseCarrierKey` under the effect-channel flag (see `configure`). There is no
    * `WellKnownTypes` entry for `IO` — it is a jvm-platform detail, deliberately absent from the platform-agnostic lang
    * layer — so the FQN is spelled here, the one place the backend names its carrier for the weaver.
    */
  val baseCarrierFQN: ValueFQN = ValueFQN(ModuleName(Seq("eliot", "jvm"), "IO"), QualifiedName("IO", Qualifier.Type))
}
