package com.vanillasource.eliot.eliotc.plugin

import cats.data.StateT
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.compiler.cache.UpToDateProcessor
import com.vanillasource.eliot.eliotc.monomorphize.fact.ContributedBinding
import com.vanillasource.eliot.eliotc.plugin.LangPlugin.{allRoots, eliotCompilerOverlay, mountFactory, pathKey}
import com.vanillasource.eliot.eliotc.plugin.Configuration.namedKey
import com.vanillasource.eliot.eliotc.plugin.{CompilerPlugin, Configuration}
import com.vanillasource.eliot.eliotc.processor.CompilerProcessor
import com.vanillasource.eliot.eliotc.processor.common.SequentialCompilerProcessors
import com.vanillasource.eliot.eliotc.source.content.SourceContentReader
import com.vanillasource.eliot.eliotc.source.file.FileContentReader
import com.vanillasource.eliot.eliotc.source.scan.{FilesystemMount, PathScanner, PoolModulesProcessor, SourceMount}
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
      .text("paths of either directories or files to compile (the user program)"),
    opt[Path]("path")
      .unbounded()
      .action((path, config) => config.updatedWith(pathKey, _.getOrElse(Seq.empty).appended(path).some))
      .text(
        "an additional source root (a layer's `eliot/` dir). Each root is scanned for the runtime pool; for the " +
          "compile-time pool its sibling `eliot-compiler/` overlay is added on top (override-preferred). Repeatable; " +
          "this is the option form of the positional `<path>`, usable after a subcommand."
      )
  )

  override def initialize(configuration: Configuration): StateT[IO, CompilerProcessor, Unit] = {
    // There is a single list of source roots (`pathKey` — the positional `<path>` program plus every `--path` layer).
    // Both pools are derived from it: every root is a runtime mount, and *for the compiler pool only* each root's
    // sibling `eliot-compiler/` overlay is added on top. `PathScanner` scans the runtime mounts for a runtime request;
    // for a compiler request it scans the runtime mounts *and* the overlay mounts, tagging the overlays so the merge
    // prefers an overlay definition over the platform's (the compiler-as-platform override). So the compiler track sees
    // the whole runtime track and borrows what no layer overrides, while a layer's own `eliot-compiler/` contribution
    // wins; user type-level code is checked in both tracks because the program root is in the same list. Roots become
    // mounts through the (substitutable) factory; plugins may contribute extra runtime mounts — both settled in
    // configure(), which completes before any initialize.
    val compilerMounts = allRoots(configuration).map(root => mountFactory(configuration)(eliotCompilerOverlay(root)))
    val runtimeMounts  = allRoots(configuration).map(mountFactory(configuration)) ++
      configuration.getOrElse(PathScanner.extraRuntimeMountsKey, Seq.empty)
    StateT
      .modify(superProcessor =>
        SequentialCompilerProcessors(
          Seq(
            superProcessor,
            UpToDateProcessor(),
            FileStatProcessor(),
            FileContentReader(),
            SourceContentReader(),
            PathScanner(compilerMounts, runtimeMounts),
            // The whole-pool module enumeration behind the `namedValues` reflection. Mount-dependent, so — like
            // `PathScanner` — it lives here rather than in the mount-free `LangProcessors` list; it is only demanded
            // when a body actually reflects.
            PoolModulesProcessor(compilerMounts, runtimeMounts)
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

  /** Every source root: the positional `<path>` program and every `--path` layer. Both the runtime and compile-time
    * mount pools are derived from this one list (see `initialize` and [[eliotCompilerOverlay]]).
    */
  val pathKey: Configuration.Key[Seq[Path]] = namedKey[Seq[Path]]("paths")

  /** All configured source roots, in order. */
  def allRoots(configuration: Configuration): Seq[Path] = configuration.getOrElse(pathKey, Seq.empty)

  /** A root's **compile-time overlay**: the sibling directory `eliot-compiler/` beside its `eliot/` source root. This
    * is a layer's opt-in compiler-platform contribution; it is scanned only for the compiler pool, where it
    * override-supersedes the borrowed runtime definition of the same name. Most roots (every runtime-only layer, the
    * user program) have no such sibling, in which case the derived mount simply resolves nothing.
    */
  def eliotCompilerOverlay(root: Path): Path = root.resolveSibling("eliot-compiler")

  private def mountFactory(configuration: Configuration): Path => SourceMount =
    configuration.getOrElse(PathScanner.mountFactoryKey, FilesystemMount(_))
}
