package com.vanillasource.eliot.eliotc.plugin

import cats.data.StateT
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.compiler.cache.UpToDateProcessor
import com.vanillasource.eliot.eliotc.plugin.LangPlugin.pathKey
import com.vanillasource.eliot.eliotc.plugin.Configuration.namedKey
import com.vanillasource.eliot.eliotc.plugin.{CompilerPlugin, Configuration}
import com.vanillasource.eliot.eliotc.processor.CompilerProcessor
import com.vanillasource.eliot.eliotc.processor.common.SequentialCompilerProcessors
import com.vanillasource.eliot.eliotc.source.content.SourceContentReader
import com.vanillasource.eliot.eliotc.source.file.FileContentReader
import com.vanillasource.eliot.eliotc.source.resource.ResourceContentReader
import com.vanillasource.eliot.eliotc.source.scan.PathScanner
import com.vanillasource.eliot.eliotc.source.stat.FileStatProcessor
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
            UpToDateProcessor(),
            FileStatProcessor(),
            FileContentReader(),
            ResourceContentReader(),
            SourceContentReader(),
            PathScanner(configuration.getOrElse(pathKey, Seq.empty))
          ) ++ LangProcessors()
        )
      )
  }
}

object LangPlugin {
  val pathKey: Configuration.Key[Seq[Path]] = namedKey[Seq[Path]]("paths")
}
