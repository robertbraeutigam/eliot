package com.vanillasource.eliot.eliotc.base

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.CompilerProcessor
import com.vanillasource.eliot.eliotc.ast.ASTParser
import com.vanillasource.eliot.eliotc.layer.Configuration.namedKey
import com.vanillasource.eliot.eliotc.layer.{CompilerSystem, Configuration, Layer}
import com.vanillasource.eliot.eliotc.module.processor.ModuleProcessor
import com.vanillasource.eliot.eliotc.processor.SequentialCompilerProcessors
import com.vanillasource.eliot.eliotc.resolve.processor.{FunctionResolver, TypeResolver}
import com.vanillasource.eliot.eliotc.source.content.SourceContentReader
import com.vanillasource.eliot.eliotc.source.resolve.ResolvedSourceContentReader
import com.vanillasource.eliot.eliotc.sugar.DesugarProcessor
import com.vanillasource.eliot.eliotc.token.Tokenizer
import com.vanillasource.eliot.eliotc.typesystem.TypeCheckProcessor
import com.vanillasource.eliot.eliotc.used.UsedSymbolsProcessor
import scopt.{OParser, OParserBuilder}

import java.io.File

class BaseLayer extends Layer {
  private val cmdLineBuilder: OParserBuilder[Configuration] = OParser.builder[Configuration]
  import cmdLineBuilder.*

  private val pathKey = namedKey[Seq[File]]("paths")

  override def commandLineParser(): OParser[_, Configuration] = OParser.sequence(
    arg[File]("<path>...")
      .unbounded()
      .required()
      .action((path, config) => config.updatedWith(pathKey, _.getOrElse(Seq.empty).appended(path).some))
      .text("paths of either directories or files to compile")
  )

  override def initialize(configuration: Configuration, system: CompilerSystem): IO[Unit] =
    system.registerProcessor(
      SequentialCompilerProcessors(
        Seq(
          SourceContentReader(),
          ResolvedSourceContentReader(configuration.getOrElse(pathKey, Seq.empty).map(_.toPath)),
          Tokenizer(),
          ASTParser(),
          DesugarProcessor(),
          ModuleProcessor(),
          FunctionResolver(),
          TypeResolver(),
          TypeCheckProcessor(),
          UsedSymbolsProcessor()
        )
      )
    )
}
