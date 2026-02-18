package com.vanillasource.eliot.eliotc.plugin

import cats.data.StateT
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.abilitycheck.AbilityCheckProcessor
import com.vanillasource.eliot.eliotc.implementation.processor.AbilityImplementationProcessor
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.processor.CoreProcessor
import com.vanillasource.eliot.eliotc.eval.processor.{
  DataTypeEvaluator,
  ExistingNamedValueEvaluator,
  SystemValueEvaluator
}
import com.vanillasource.eliot.eliotc.module.processor.{
  ModuleNamesProcessor,
  ModuleValueProcessor,
  UnifiedModuleNamesProcessor,
  UnifiedModuleValueProcessor
}
import com.vanillasource.eliot.eliotc.monomorphize.processor.MonomorphicTypeCheckProcessor
import com.vanillasource.eliot.eliotc.plugin.LangPlugin.pathKey
import com.vanillasource.eliot.eliotc.plugin.Configuration.namedKey
import com.vanillasource.eliot.eliotc.plugin.{CompilerPlugin, Configuration}
import com.vanillasource.eliot.eliotc.processor.CompilerProcessor
import com.vanillasource.eliot.eliotc.processor.common.SequentialCompilerProcessors
import com.vanillasource.eliot.eliotc.resolve.processor.ValueResolver
import com.vanillasource.eliot.eliotc.source.content.SourceContentReader
import com.vanillasource.eliot.eliotc.source.file.FileContentReader
import com.vanillasource.eliot.eliotc.source.resource.ResourceContentReader
import com.vanillasource.eliot.eliotc.source.scan.PathScanner
import com.vanillasource.eliot.eliotc.source.stat.FileStatProcessor
import com.vanillasource.eliot.eliotc.symbolic.processor.SymbolicTypeCheckProcessor
import com.vanillasource.eliot.eliotc.token.Tokenizer
import com.vanillasource.eliot.eliotc.uncurry.processor.UncurryingProcessor
import com.vanillasource.eliot.eliotc.used.UsedNamesProcessor
import scopt.{OParser, OParserBuilder}

import java.nio.file.Path

class LangPlugin extends CompilerPlugin {
  private val cmdLineBuilder: OParserBuilder[Configuration] = OParser.builder[Configuration]

  import cmdLineBuilder.*

  override def commandLineParser(): OParser[?, Configuration] = OParser.sequence(
    arg[Path]("<path>...")
      .unbounded()
      .required()
      .action((path, config) => config.updatedWith(pathKey, _.getOrElse(Seq.empty).appended(path).some))
      .text("paths of either directories or files to compile")
  )

  override def initialize(configuration: Configuration): StateT[IO, CompilerProcessor, Unit] = {
    StateT
      .modify(superProcessor =>
        SequentialCompilerProcessors(
          Seq(
            superProcessor,
            FileStatProcessor(),
            FileContentReader(),
            ResourceContentReader(),
            SourceContentReader(),
            PathScanner(configuration.getOrElse(pathKey, Seq.empty)),
            Tokenizer(),
            ASTParser(),
            CoreProcessor(),
            SystemValueEvaluator(),
            ExistingNamedValueEvaluator(),
            DataTypeEvaluator(),
            ModuleNamesProcessor(),
            ModuleValueProcessor(),
            UnifiedModuleNamesProcessor(),
            UnifiedModuleValueProcessor(),
            ValueResolver(),
            SymbolicTypeCheckProcessor(),
            AbilityImplementationProcessor(),
            AbilityCheckProcessor(),
            MonomorphicTypeCheckProcessor(),
            UsedNamesProcessor(),
            UncurryingProcessor()
          )
        )
      )
  }
}

object LangPlugin {
  val pathKey: Configuration.Key[Seq[Path]] = namedKey[Seq[Path]]("paths")
}
