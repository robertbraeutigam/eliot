package com.vanillasource.eliot.eliotc.plugin

import cats.data.StateT
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.compiler.cache.UpToDateProcessor
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.ContributedBinding
import com.vanillasource.eliot.eliotc.plugin.LangPlugin.{
  allRoots,
  baseCarrierKey,
  compilerRoots,
  effectChannelKey,
  eliotCompilerOverlay,
  entryPointKey,
  mountFactory,
  pathKey
}
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
      ),
    // Effects-as-channel Phase 3 (docs/effects-as-channel.md §10): the gated new path. When set, the desugar strips
    // open effect rows to their payload and erases the carrier from effect abilities, so the checker is effect-blind;
    // verification and elaboration move downstream (built in later slices). Off by default — the whole current
    // carrier-based path is unchanged unless this flag is given.
    opt[Unit]("effect-channel")
      .action((_, config) => config.set(effectChannelKey, true))
      .text(
        "EXPERIMENTAL: strip effect rows out of the type language (effects-as-channel). The checker becomes " +
          "effect-blind; effect verification and elaboration happen after monomorphization. Off by default."
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
    //
    // A tool that drives the compiler programmatically (the LSP) may additionally set `compilerPathKey` with
    // compile-time overlay roots listed *explicitly* — for a layout whose overlay is not the `eliot-compiler/` sibling
    // of a runtime root, so the sibling derivation cannot find it. Those roots are added to the compiler pool on top of
    // the derived siblings (deduplicated). The CLI never sets this key, so `eliotc` stays fully `--path`-parameterised.
    val compilerRootPaths = (allRoots(configuration).map(eliotCompilerOverlay) ++ compilerRoots(configuration)).distinct
    val compilerMounts    = compilerRootPaths.map(mountFactory(configuration))
    val runtimeMounts     = allRoots(configuration).map(mountFactory(configuration)) ++
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
          ) ++ LangProcessors(
            extraNativeBindingLabels =
              // The native-binding merger built inside LangProcessors must consult every native contributor that other
              // layers registered in their configure() (e.g. stdlib's arithmetic natives). All configure() complete before
              // initialize, so the roster is already final here.
              configuration.getOrElse(ContributedBinding.extraNativeLabelsKey, Set.empty[String]).toSeq,
            effectChannel = configuration.getOrElse(effectChannelKey, false),
            // The platform's runtime base effect carrier (the jvm target's `eliot.jvm.IO`), contributed by that target's
            // plugin in its configure() when the effect-channel flag is on, so the weaver can assign it and resolve
            // effect operations at it. Absent (no effect-channel target, or the flag off) leaves the weave the identity
            // image of each `MonomorphicValue`.
            baseCarrier = configuration.get(baseCarrierKey),
            // The platform's synthesized entry-point value (the jvm target's `main::main`), whose effect-blind body is a
            // bare reference to the user `main`; the weaver wraps its woven `IO[Unit]` reference in the carrier's run
            // boundary so the launcher runs it. Same provenance/lifecycle as `baseCarrier`.
            entryPoint = configuration.get(entryPointKey)
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

  /** The effects-as-channel gated-path flag (`--effect-channel`, docs/effects-as-channel.md §10 Phase 3). When set, the
    * desugar strips open effect rows to their payload and erases the carrier from effect abilities, making the checker
    * effect-blind; effect verification and elaboration move to post-monomorphization processors (grown in later slices).
    * Absent (the default) leaves the entire current carrier-based effect path unchanged.
    */
  val effectChannelKey: Configuration.Key[Boolean] = namedKey[Boolean]("effectChannel")

  /** The platform's runtime **base effect carrier** — the concrete `Suspend`-riding carrier the effects-as-channel
    * weaver assigns as the ambient stack base and resolves effect operations at (the jvm target's `eliot.jvm.IO`). It is
    * a *platform* fact, so it is contributed by the target's plugin (jvm) in its `configure()` — under the
    * [[effectChannelKey]] flag — and read here, exactly the way [[effectChannelKey]] itself is threaded; `LangPlugin`,
    * being platform-agnostic, never names a carrier. Absent (no effect-channel-capable target selected, or the flag off)
    * leaves [[com.vanillasource.eliot.eliotc.monomorphize.channel.WovenValueProcessor]] the identity image of each
    * `MonomorphicValue`.
    */
  val baseCarrierKey: Configuration.Key[ValueFQN] = namedKey[ValueFQN]("baseCarrier")

  /** The platform's synthesized **entry-point** value (the jvm target's `main::main`). Under the effect-channel flag the
    * effect-blind checker types this value's body — a bare reference to the user `main` — as pure `Unit`; the weaver
    * recognises it (by this FQN) and wraps its woven `IO[Unit]` reference in the carrier's run boundary
    * (`<baseCarrier module>::runMain`), so the platform launcher runs the effect instead of receiving a bare `IO`. A
    * platform fact, contributed by the target's plugin (jvm) in its `configure()` under the flag, exactly like
    * [[baseCarrierKey]]. Absent leaves the weaver with no entry to rewrite.
    */
  val entryPointKey: Configuration.Key[ValueFQN] = namedKey[ValueFQN]("entryPoint")

  /** **Explicit** compile-time overlay roots, set programmatically by a driver (the LSP) — *not* a CLI option. Each is
    * scanned for the compiler pool only, override-superseding the borrowed runtime definition of the same name, exactly
    * like a derived `eliot-compiler/` sibling ([[eliotCompilerOverlay]]). This is the escape hatch for a project layout
    * whose overlay is not that sibling (e.g. a Mill-style tree with sources directly under `src/`), where the derivation
    * finds nothing. Empty for the CLI, which keeps deriving siblings from its `--path` runtime roots only.
    */
  val compilerPathKey: Configuration.Key[Seq[Path]] = namedKey[Seq[Path]]("compilerPaths")

  /** All explicit compile-time overlay roots, in order (see [[compilerPathKey]]). */
  def compilerRoots(configuration: Configuration): Seq[Path] = configuration.getOrElse(compilerPathKey, Seq.empty)

  /** A root's **compile-time overlay**: the sibling directory `eliot-compiler/` beside its `eliot/` source root. This
    * is a layer's opt-in compiler-platform contribution; it is scanned only for the compiler pool, where it
    * override-supersedes the borrowed runtime definition of the same name. Most roots (every runtime-only layer, the
    * user program) have no such sibling, in which case the derived mount simply resolves nothing.
    */
  def eliotCompilerOverlay(root: Path): Path = root.resolveSibling("eliot-compiler")

  private def mountFactory(configuration: Configuration): Path => SourceMount =
    configuration.getOrElse(PathScanner.mountFactoryKey, FilesystemMount(_))
}
