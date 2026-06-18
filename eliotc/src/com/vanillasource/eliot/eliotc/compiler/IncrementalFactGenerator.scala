package com.vanillasource.eliot.eliotc.compiler

import cats.data.Chain
import cats.effect.{Deferred, IO, IOLocal, Ref}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.compiler.cache.{CacheEntry, DependencyTrackingProcess, FactCache, FactCacheData}
import com.vanillasource.eliot.eliotc.feedback.{CompilerError, Logging}
import com.vanillasource.eliot.eliotc.processor.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor}

/** Cache-aware fact generator implementing incremental compilation by backward, demand-driven validation.
  *
  * It keeps the same concurrent core as the non-incremental generator — a [[Deferred]]-per-key map that computes each
  * fact at most once per run, per-fiber generation, and an [[IOLocal]] ancestor chain for recursion detection — and adds
  * exactly two behaviors:
  *
  *   1. before generating a fact, consult the `prior` snapshot: if the fact has a cached entry and every one of its
  *      recorded direct dependencies still resolves to the value it had last run, accept the cached value without
  *      re-running the processor;
  *   2. while generating a fact, record (via [[DependencyTrackingProcess]]) the facts its processor reads, so the trace
  *      can be persisted for the next run.
  *
  * Leaf facts — those whose recorded dependency set is empty, e.g. a `stat` of a source file — are always regenerated,
  * forming the boundary with the external world. Failures are never cached, so a fact that failed to generate has no
  * prior entry and is regenerated (re-emitting its error) on every run until fixed.
  *
  * With an empty `prior` (cold start) every fact is regenerated, so behavior matches a non-incremental generator plus
  * harmless dependency recording.
  */
final class IncrementalFactGenerator(
    generator: CompilerProcessor,
    prior: Map[CompilerFactKey[?], CacheEntry],
    errors: Ref[IO, Chain[CompilerError]],
    facts: Ref[IO, Map[CompilerFactKey[?], Deferred[IO, Option[CompilerFact]]]],
    directDependencies: Ref[IO, Map[CompilerFactKey[?], Set[CompilerFactKey[?]]]],
    activeKeys: IOLocal[List[CompilerFactKey[?]]]
) extends CompilationProcess
    with Logging {

  override def activeFactKeys: IO[List[CompilerFactKey[?]]] = activeKeys.get

  override def getFact[V <: CompilerFact, K <: CompilerFactKey[V]](key: K): IO[Option[V]] =
    for {
      modifyResult <- modifyAtomicallyFor(key)
      _            <- (activeKeys.update(key :: _) >> resolve(key, modifyResult._1))
                        .handleErrorWith(t => error[IO](s"Resolving (${key.getClass.getName}) $key failed.", t))
                        .flatMap(_ => modifyResult._1.complete(None).void) // safety net; no-op if already completed
                        .start
                        .whenA(modifyResult._2) // only the first requester runs the computation
      result       <- modifyResult._1.get
    } yield result.map(_.asInstanceOf[V])

  override def registerFact(fact: CompilerFact): IO[Unit] =
    modifyAtomicallyFor(fact.key()).flatMap(_._1.complete(Some(fact)).void)

  /** Satisfy a first-time request: accept the cached value if it is still valid, otherwise regenerate.
    *
    * An injected fact (registered directly, never produced by a processor) is always accepted — nothing can regenerate
    * it, and its cached value is authoritative for the build target.
    */
  private def resolve(key: CompilerFactKey[?], deferred: Deferred[IO, Option[CompilerFact]]): IO[Unit] =
    prior.get(key) match {
      case Some(entry) if entry.injected            => acceptPrior(key, entry, deferred)
      case Some(entry) if entry.directDeps.nonEmpty =>
        depsUnchanged(entry.directDeps).flatMap {
          case true  => acceptPrior(key, entry, deferred)
          case false => regenerate(key)
        }
      case _ => regenerate(key) // no prior entry, or a generated leaf (empty deps) ⇒ always run
    }

  /** Every recorded dependency still resolves to the value it had last run. */
  private def depsUnchanged(deps: Set[CompilerFactKey[?]]): IO[Boolean] =
    deps.toList.forallM { dep =>
      getFactUntyped(dep).map {
        case Some(current) => prior.get(dep).exists(p => (p.fact eq current) || p.fact == current)
        case None          => false // dependency no longer producible ⇒ treat as changed
      }
    }

  /** Accept the cached fact and carry its trace forward so it re-persists with the right metadata. An injected fact is
    * intentionally *not* recorded in `directDependencies`, so it is re-classified as injected when the cache is rebuilt
    * (see [[buildCacheData]]).
    */
  private def acceptPrior(
      key: CompilerFactKey[?],
      entry: CacheEntry,
      deferred: Deferred[IO, Option[CompilerFact]]
  ): IO[Unit] =
    directDependencies.update(_.updated(key, entry.directDeps)).unlessA(entry.injected) >>
      deferred.complete(Some(entry.fact)).void

  /** Run the processor, recording (via [[DependencyTrackingProcess]]) the facts it reads as this key's direct
    * dependencies. The processor completes the fact's [[Deferred]] itself via [[registerFact]]; the caller's safety net
    * handles the "produced nothing" case.
    *
    * The key is marked present in `directDependencies` up front (empty), so that even a generated leaf — which makes no
    * `getFact` calls — is recorded as *generated* (not mistaken for an injected fact, see [[buildCacheData]]). Reads then
    * accumulate into the same entry as they happen, before the fact becomes observable.
    */
  private def regenerate(key: CompilerFactKey[?]): IO[Unit] =
    directDependencies.update(deps => deps.updated(key, deps.getOrElse(key, Set.empty))) >>
      generator
        .generate(key)
        .run(new DependencyTrackingProcess(this, key, directDependencies))
        .runS(Chain.empty)
        .fold(identity, identity)
        .flatMap(es => errors.update(_ ++ es))

  private def getFactUntyped(key: CompilerFactKey[?]): IO[Option[CompilerFact]] =
    getFact(key.asInstanceOf[CompilerFactKey[CompilerFact]])

  private def modifyAtomicallyFor(
      key: CompilerFactKey[?]
  ): IO[(Deferred[IO, Option[CompilerFact]], Boolean)] =
    for {
      newValue <- Deferred[IO, Option[CompilerFact]]
      result   <- facts.modify { internalMap =>
                    internalMap.get(key) match
                      case Some(alreadyPresentValue) => (internalMap, (alreadyPresentValue, false))
                      case None                      => (internalMap.updated(key, newValue), (newValue, true))
                  }
    } yield result

  def currentFacts(): IO[Map[CompilerFactKey[?], CompilerFact]] =
    for {
      currentMap <- facts.get
      resolved   <- currentMap.values.toSeq.traverse(_.tryGet.map(_.flatten)).map(_.flatten)
    } yield resolved.map(fact => fact.key() -> fact).toMap

  def currentErrors(): IO[Seq[CompilerError]] = errors.get.map(_.toList)

  /** Build the cache to persist after this run. Only facts that successfully generated (resolved to a value) are
    * included, so failures are never cached and re-surface on the next run.
    *
    * A fact that a processor generated has an entry in `directDependencies` (its recorded reads, possibly empty for a
    * leaf). A fact that was only registered directly — injected (e.g. a dynamic source) — has none, and is marked so it
    * is accepted rather than (unsuccessfully) regenerated next run.
    */
  def buildCacheData(): IO[FactCacheData] =
    for {
      factMap <- currentFacts()
      deps    <- directDependencies.get
    } yield FactCacheData(
      FactCache.CACHE_VERSION,
      factMap.map { case (key, fact) =>
        key -> CacheEntry(fact, deps.getOrElse(key, Set.empty), injected = !deps.contains(key))
      }
    )
}

object IncrementalFactGenerator {
  def create(generator: CompilerProcessor, prior: Option[FactCacheData]): IO[IncrementalFactGenerator] =
    for {
      errors     <- Ref.of[IO, Chain[CompilerError]](Chain.empty)
      facts      <- Ref.of[IO, Map[CompilerFactKey[?], Deferred[IO, Option[CompilerFact]]]](Map.empty)
      deps       <- Ref.of[IO, Map[CompilerFactKey[?], Set[CompilerFactKey[?]]]](Map.empty)
      activeKeys <- IOLocal[List[CompilerFactKey[?]]](Nil)
    } yield new IncrementalFactGenerator(
      generator,
      prior.map(_.entries).getOrElse(Map.empty),
      errors,
      facts,
      deps,
      activeKeys
    )
}
