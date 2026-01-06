package com.vanillasource.eliot.eliotc.plugin

import cats.data.StateT
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.processor.ASTParser
import com.vanillasource.eliot.eliotc.plugin.BasePlugin.pathKey
import com.vanillasource.eliot.eliotc.plugin.Configuration.namedKey
import com.vanillasource.eliot.eliotc.plugin.{CompilerPlugin, Configuration}
import com.vanillasource.eliot.eliotc.module.processor.{
  ModuleDataProcessor,
  ModuleFunctionProcessor,
  ModuleNamesProcessor,
  UnifiedModuleDataProcessor,
  UnifiedModuleFunctionProcessor,
  UnifiedModuleNamesProcessor
}
import com.vanillasource.eliot.eliotc.processor.common.SequentialCompilerProcessors
import com.vanillasource.eliot.eliotc.processor.CompilerProcessor
import com.vanillasource.eliot.eliotc.resolve.processor.{FunctionResolver, TypeResolver}
import com.vanillasource.eliot.eliotc.source.content.SourceContentReader
import com.vanillasource.eliot.eliotc.source.scan.PathScanner
import com.vanillasource.eliot.eliotc.datafunctions.DataFunctionsProcessor
import com.vanillasource.eliot.eliotc.token.Tokenizer
import com.vanillasource.eliot.eliotc.typesystem.processor.TypeCheckProcessor
import com.vanillasource.eliot.eliotc.used.UsedSymbolsProcessor
import scopt.{OParser, OParserBuilder}

import java.io.File
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
            DataFunctionsProcessor(),
            ModuleFunctionProcessor(),
            ModuleDataProcessor(),
            ModuleNamesProcessor(),
            UnifiedModuleNamesProcessor(),
            UnifiedModuleDataProcessor(),
            UnifiedModuleFunctionProcessor(),
            FunctionResolver(),
            TypeResolver(),
            TypeCheckProcessor(),
            UsedSymbolsProcessor()
          )
        )
      )
  }
}

object BasePlugin {
  val pathKey = namedKey[Seq[Path]]("paths")
}
