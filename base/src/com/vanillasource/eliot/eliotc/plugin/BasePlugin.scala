package com.vanillasource.eliot.eliotc.plugin

import cats.data.StateT
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.core.processor.CoreProcessor
import com.vanillasource.eliot.eliotc.module.processor.{
  ModuleValueProcessor,
  UnifiedModuleValueProcessor,
  ModuleNamesProcessor as ModuleNamesProcessor2,
  UnifiedModuleNamesProcessor as UnifiedModuleNamesProcessor2
}
import com.vanillasource.eliot.eliotc.monomorphize.processor.MonomorphicTypeCheckProcessor
import com.vanillasource.eliot.eliotc.plugin.BasePlugin.pathKey
import com.vanillasource.eliot.eliotc.plugin.Configuration.namedKey
import com.vanillasource.eliot.eliotc.plugin.{CompilerPlugin, Configuration}
import com.vanillasource.eliot.eliotc.processor.CompilerProcessor
import com.vanillasource.eliot.eliotc.processor.common.SequentialCompilerProcessors
import com.vanillasource.eliot.eliotc.resolve2.processor.ValueResolver
import com.vanillasource.eliot.eliotc.source.content.SourceContentReader
import com.vanillasource.eliot.eliotc.source.scan.PathScanner
import com.vanillasource.eliot.eliotc.symbolic.processor.SymbolicTypeCheckProcessor
import com.vanillasource.eliot.eliotc.token.Tokenizer
import com.vanillasource.eliot.eliotc.uncurry2.processor.UncurryingProcessor
import com.vanillasource.eliot.eliotc.used2.UsedNamesProcessor
import scopt.{OParser, OParserBuilder}

import java.nio.file.Path

class BasePlugin extends CompilerPlugin {
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
            SourceContentReader(),
            PathScanner(configuration.getOrElse(pathKey, Seq.empty)),
            Tokenizer(),
            ASTParser(),
            CoreProcessor(),
            ModuleNamesProcessor2(),
            ModuleValueProcessor(),
            UnifiedModuleNamesProcessor2(),
            UnifiedModuleValueProcessor(),
            ValueResolver(),
            SymbolicTypeCheckProcessor(),
            MonomorphicTypeCheckProcessor(),
            UsedNamesProcessor(),
            UncurryingProcessor()
          )
        )
      )
  }
}

object BasePlugin {
  val pathKey: Configuration.Key[Seq[Path]] = namedKey[Seq[Path]]("paths")
}
