package com.vanillasource.eliot.eliotc.compiler

import cats.effect.{IO, Ref}
import cats.effect.std.Mutex
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.compiler.cache.{CacheFingerprint, FactCache, FactCacheData}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.plugin.{CompilerPlugin, Configuration}
import com.vanillasource.eliot.eliotc.processor.CompilerProcessor
import com.vanillasource.eliot.eliotc.processor.common.NullProcessor
import com.vanillasource.eliot.eliotc.visualization.FactVisualizationTracker
import com.vanillasource.eliot.eliotc.visualization.TrackedCompilerProcessor.wrapProcessor

import java.nio.file.Path

/** A resident compilation context. The expensive, run-independent setup — plugin configuration, the processor graph,
  * cache fingerprints — is done once in [[CompilationSession.create]]; each call to [[compileOnce]] then runs a full
  * compilation against the in-memory fact cache and folds the result back in as the next run's `prior`. The on-disk
  * cache is touched only at the edges (seed in `create`, flush in [[persist]]), never per compile — so the rich
  * in-memory facts (typed `SemValue`s with their native closures, dropped on serialization) survive across runs.
  *
  * This is the single seam the CLI and a long-running (LSP) server share: the CLI calls `compileOnce` once and
  * `persist`s; a server loops `compileOnce` and `persist`s on shutdown.
  */
final class CompilationSession private (
    targetPlugin: CompilerPlugin,
    processors: CompilerProcessor,
    val effectiveConfiguration: Configuration,
    targetPath: Path,
    compilerFingerprint: String,
    configFingerprint: String,
    cache: Ref[IO, Option[FactCacheData]],
    compileLock: Mutex[IO]
) extends Logging {

  /** Run one full compilation against the current in-memory cache, then store the resulting cache for the next run.
    *
    * Serialized via the lock so overlapping requests cannot corrupt the hand-off. The cache is updated as the *last*
    * step, so a run cancelled mid-flight leaves the previous good cache untouched — cancel-restart on a newer edit is
    * safe. Failures are never cached (they are not materialised), so a broken fact re-runs next time and re-surfaces its
    * error.
    *
    * @param tracker optional fact-flow visualization; omit in server mode.
    * @return the live generator (queryable for LSP features) plus the run's diagnostics.
    */
  def compileOnce(tracker: Option[FactVisualizationTracker] = None): IO[CompilationResult] =
    compileLock.lock.surround {
      for {
        prior     <- cache.get
        wrapped    = tracker.fold(processors)(t => processors.wrapWith(wrapProcessor(_, t)))
        generator <- IncrementalFactGenerator.create(wrapped, prior)
        _         <- targetPlugin.run(effectiveConfiguration, generator)
        nextCache <- generator.buildCacheData()
        _         <- cache.set(Some(nextCache)) // last effect ⇒ a cancelled run keeps the old cache
        errors    <- generator.currentErrors()
      } yield CompilationResult(generator, errors)
    }

  /** Flush the in-memory cache to disk so the next *process* start is warm. Call from the CLI after its single compile,
    * and from a server on shutdown. Fail-safe: [[FactCache.save]] warns rather than failing.
    */
  def persist(): IO[Unit] =
    cache.get.flatMap(_.traverse_(FactCache.save(targetPath, compilerFingerprint, configFingerprint, _)))
}

object CompilationSession {

  /** One-time setup: let the activated plugins configure each other, collect their processors, compute fingerprints, and
    * seed the in-memory cache from disk. The returned session is then re-runnable.
    */
  def create(
      targetPlugin: CompilerPlugin,
      activatedPlugins: Seq[CompilerPlugin],
      configuration: Configuration,
      args: List[String]
  ): IO[CompilationSession] =
    for {
      effectiveConfig <- activatedPlugins.traverse_(_.configure()).runS(configuration)
      processors      <- activatedPlugins.traverse_(_.initialize(effectiveConfig)).runS(NullProcessor())
      targetPath       = effectiveConfig.get(Compiler.targetPathKey).get
      compilerFp      <- CacheFingerprint.compiler
      configFp         = CacheFingerprint.config(args)
      seeded          <- FactCache.load(targetPath, compilerFp, configFp)
      cache           <- Ref.of[IO, Option[FactCacheData]](seeded)
      lock            <- Mutex[IO]
    } yield new CompilationSession(
      targetPlugin,
      processors,
      effectiveConfig,
      targetPath,
      compilerFp,
      configFp,
      cache,
      lock
    )
}
