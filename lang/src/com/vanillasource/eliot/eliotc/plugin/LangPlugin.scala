package com.vanillasource.eliot.eliotc.plugin

import cats.data.StateT
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.compiler.cache.UpToDateProcessor
import com.vanillasource.eliot.eliotc.monomorphize.fact.ContributedBinding
import com.vanillasource.eliot.eliotc.plugin.LangPlugin.{compilerPathKey, mountFactory, pathKey, runtimePathKey}
import com.vanillasource.eliot.eliotc.plugin.Configuration.namedKey
import com.vanillasource.eliot.eliotc.plugin.{CompilerPlugin, Configuration}
import com.vanillasource.eliot.eliotc.processor.CompilerProcessor
import com.vanillasource.eliot.eliotc.processor.common.SequentialCompilerProcessors
import com.vanillasource.eliot.eliotc.source.content.SourceContentReader
import com.vanillasource.eliot.eliotc.source.file.FileContentReader
import com.vanillasource.eliot.eliotc.source.scan.{FilesystemMount, PathScanner, SourceMount}
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
      .text("paths of either directories or files to compile (the user program; folded into the runtime path)"),
    opt[Path]("compiler-path")
      .unbounded()
      .action((path, config) => config.updatedWith(compilerPathKey, _.getOrElse(Seq.empty).appended(path).some))
      .text(
        "the compiler-platform override overlay: a source root added on top of the runtime path for compile-time " +
          "evaluation, whose definitions supersede the platform's; repeatable"
      ),
    opt[Path]("runtime-path")
      .unbounded()
      .action((path, config) => config.updatedWith(runtimePathKey, _.getOrElse(Seq.empty).appended(path).some))
      .text("a source root scanned for codegen (the base, the target layer, and the user program); repeatable")
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
            SourceContentReader(),
            // The two root lists are the *only* filesystem source of `.els` (CP1.5). `--runtime-path` carries the base +
            // the selected target layer; `--compiler-path` carries the compiler-platform **override overlay** only. The
            // marker no longer selects one exclusive pool: `PathScanner` scans the runtime mounts for a runtime request,
            // but for a compiler request it scans the runtime mounts *and* the override mounts, tagging the override
            // mounts' files so the merge prefers an overlay definition over the platform's (the compiler-as-platform
            // override). So the platform layer is no longer pool-exclusive — the compiler track sees all of it and
            // borrows what it does not override. The positional `<path>` (the user's program) folds into the runtime
            // pool *only* — the compiler scan still reaches it through the runtime union (so user type-level code is
            // checked in both tracks), while keeping the override set exactly the compiler overlay. Roots become mounts
            // through the (substitutable) factory; plugins may contribute extra runtime mounts — both settled in
            // configure(), which completes before any initialize.
            PathScanner(
              configuration.getOrElse(compilerPathKey, Seq.empty).map(mountFactory(configuration)),
              (configuration.getOrElse(runtimePathKey, Seq.empty) ++ configuration.getOrElse(pathKey, Seq.empty))
                .map(mountFactory(configuration)) ++
                configuration.getOrElse(PathScanner.extraRuntimeMountsKey, Seq.empty)
            )
          ) ++ LangProcessors(extraNativeBindingLabels =
            // The native-binding merger built inside LangProcessors must consult every native contributor that other
            // layers registered in their configure() (e.g. stdlib's arithmetic natives). All configure() complete before
            // initialize, so the roster is already final here.
            configuration.getOrElse(ContributedBinding.extraNativeLabelsKey, Set.empty[String]).toSeq
          )
        )
      )
  }
}

object LangPlugin {
  val pathKey: Configuration.Key[Seq[Path]]         = namedKey[Seq[Path]]("paths")
  val compilerPathKey: Configuration.Key[Seq[Path]] = namedKey[Seq[Path]]("compilerPaths")
  val runtimePathKey: Configuration.Key[Seq[Path]]  = namedKey[Seq[Path]]("runtimePaths")

  private def mountFactory(configuration: Configuration): Path => SourceMount =
    configuration.getOrElse(PathScanner.mountFactoryKey, FilesystemMount(_))
}
