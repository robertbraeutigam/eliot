package com.vanillasource.eliot.eliotc.compiler

import cats.effect.{IO, Ref}
import cats.effect.std.Mutex
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.compiler.cache.codec.FactKeyCodecs
import com.vanillasource.eliot.eliotc.compiler.cache.{CacheFingerprint, FactCacheData, IncrementalCacheBackend}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.plugin.{CompilerPlugin, Configuration}
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerProcessor}
import com.vanillasource.eliot.eliotc.processor.common.NullProcessor
import com.vanillasource.eliot.eliotc.statistics.{PhaseTimings, ProcessorStatistics}

import scala.concurrent.duration.FiniteDuration
import com.vanillasource.eliot.eliotc.visualization.{FactVisualizationTracker, TrackedCompilerProcessor}

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
    backend: IncrementalCacheBackend,
    cache: Ref[IO, Option[FactCacheData]],
    compileLock: Mutex[IO],
    phaseTimings: PhaseTimings
) extends Logging {

  /** Run one full compilation against the current in-memory cache, then store the resulting cache for the next run.
    *
    * Serialized via the lock so overlapping requests cannot corrupt the hand-off. The cache is updated as the *last*
    * step, so a run cancelled mid-flight leaves the previous good cache untouched — cancel-restart on a newer edit is
    * safe. Failures are never cached (they are not materialised), so a broken fact re-runs next time and re-surfaces its
    * error.
    *
    * @param tracker optional fact-flow visualization; omit in server mode.
    * @param statistics optional per-processor timing; omit in server mode.
    * @return the live generator (queryable for LSP features) plus the run's diagnostics.
    */
  def compileOnce(
      tracker: Option[FactVisualizationTracker] = None,
      statistics: Option[ProcessorStatistics] = None
  ): IO[CompilationResult] =
    compileLock.lock.surround {
      for {
        prior     <- cache.get
        wrapped   <- IO.delay(wrap(processors, tracker, statistics))
        generator <- IncrementalFactGenerator.create(wrapped, prior, strictAccounting = true)
        produced  <- targetPlugin.run(effectiveConfiguration, generator)
        nextCache <- phaseTimings.time(PhaseTimings.buildCache)(generator.buildCacheData())
        _         <- cache.set(Some(nextCache)) // last effect ⇒ a cancelled run keeps the old cache
        errors    <- generator.currentErrors()
      } yield CompilationResult(generator, errors, produced)
    }

  /** Apply all requested instrumentation in a *single* pass, because `wrapWith` hands each wrapper the leaf processor
    * and both wrappers need the leaf's class name to label it — wrapping a wrapper would name every processor after the
    * instrumentation instead. Timing goes innermost, so the tracking wrapper's own overhead falls inside a measured
    * fact wait rather than being charged to the processor.
    */
  private def wrap(
      processor: CompilerProcessor,
      tracker: Option[FactVisualizationTracker],
      statistics: Option[ProcessorStatistics]
  ): CompilerProcessor =
    if (tracker.isEmpty && statistics.isEmpty) processor
    else {
      val instrumented = processor.wrapWith { leaf =>
        val name  = leaf.getClass.getName
        val timed = statistics.fold(leaf)(_.wrap(leaf, name))

        tracker.fold(timed)(TrackedCompilerProcessor(timed, _, name))
      }

      statistics.fold(instrumented)(_.wrapTree(instrumented))
    }

  /** Flush the in-memory cache to disk so the next *process* start is warm. Call from the CLI after its single compile,
    * and from a server on shutdown. Fail-safe: [[FactCache.save]] warns rather than failing.
    */
  def persist(): IO[Unit] =
    cache.get.flatMap(
      _.traverse_(data => phaseTimings.time(PhaseTimings.cacheSave)(backend.save(data)))
    )

  /** The coarse cache-phase timings (fingerprint, load, build, save) recorded across this session so far, by phase id.
    * `--statistics` reports them as explicit lines so the cache load/store cost — invisible to the per-processor total,
    * since it falls outside the compile window — is accounted rather than lost.
    */
  def phaseSnapshot: IO[Map[String, FiniteDuration]] = phaseTimings.snapshot
}

object CompilationSession {

  /** One-time setup: let the activated plugins configure each other, collect their processors, compute fingerprints, and
    * seed the in-memory cache from disk. The returned session is then re-runnable.
    */
  def create(
      targetPlugin: CompilerPlugin,
      activatedPlugins: Seq[CompilerPlugin],
      configuration: Configuration
  ): IO[CompilationSession] =
    for {
      effectiveConfig <- activatedPlugins.traverse_(_.configure()).runS(configuration)
      processors      <- activatedPlugins.traverse_(_.initialize(effectiveConfig)).runS(NullProcessor())
      targetPath       = effectiveConfig.get(Compiler.targetPathKey).get
      phaseTimings    <- PhaseTimings.create()
      compilerFp      <- phaseTimings.time(PhaseTimings.fingerprint)(CacheFingerprint.compiler)
      configFp         = CacheFingerprint.config(effectiveConfig)
      backend         <- IncrementalCacheBackend.create(
                           targetPath,
                           compilerFp,
                           configFp,
                           effectiveConfig.get(FactKeyCodecs.configKey).getOrElse(Map.empty)
                         )
      seeded          <- phaseTimings.time(PhaseTimings.cacheLoad)(backend.load())
      cache           <- Ref.of[IO, Option[FactCacheData]](seeded)
      lock            <- Mutex[IO]
    } yield new CompilationSession(
      targetPlugin,
      processors,
      effectiveConfig,
      targetPath,
      compilerFp,
      configFp,
      backend,
      cache,
      lock,
      phaseTimings
    )
}
