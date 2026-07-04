package com.vanillasource.eliot.eliotc.compiler

import cats.data.Chain
import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.compiler.cache.{CacheEntry, DependencyTrackingProcess, FactCache, FactCacheData}
import com.vanillasource.eliot.eliotc.feedback.{CompilerError, Logging}
import com.vanillasource.eliot.eliotc.processor.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor}

/** Cache-aware fact generator implementing incremental compilation by backward, demand-driven validation.
  *
  * It keeps the same concurrent core as a plain generator — a [[Deferred]]-per-key map that computes each fact at most
  * once per run, per-fiber generation, and an ancestor chain for recursion detection threaded explicitly through the
  * per-generation [[DependencyTrackingProcess]] (the `ancestors` parameter of [[getFact]]) — and decides, per
  * dependency, whether it changed since last run via [[depUnchanged]]:
  *
  *   - a dependency with a **stored value** (every leaf, and every serializable derived fact) is recomputed and compared
  *     by value — this is the equality cutoff: a changed leaf whose derived value recomputes equal stops propagation;
  *   - a **value-less** dependency (a non-serializable `SemValue`-bearing fact) is validated **structurally** by drilling
  *     through its recorded `directDeps` to the leaves, *without materialising the value*. Its prior edges-only entry is
  *     carried forward when the structural check passes, so it stays drillable next run.
  *
  * Consequently a no-change run materialises no `SemValue` at all (the whole monomorphize layer is skipped), and a
  * value-less fact is regenerated only when a genuinely changed dependent actually reads its value.
  *
  * Leaf facts — those whose recorded dependency set is empty, e.g. a `stat` of a source file — are always regenerated,
  * forming the boundary with the external world. Failures are never cached, so a fact that failed has no prior entry and
  * is regenerated (re-emitting its error) on every run until fixed.
  *
  * With an empty `prior` (cold start) every fact is regenerated, so behavior matches a non-incremental generator plus
  * harmless dependency recording.
  */
final class IncrementalFactGenerator(
    generator: CompilerProcessor,
    prior: Map[CompilerFactKey[?], CacheEntry],
    strictAccounting: Boolean,
    errors: Ref[IO, Chain[CompilerError]],
    facts: Ref[IO, Map[CompilerFactKey[?], Deferred[IO, Option[CompilerFact]]]],
    directDependencies: Ref[IO, Map[CompilerFactKey[?], Set[CompilerFactKey[?]]]],
    producedDuring: Ref[IO, Map[CompilerFactKey[?], CompilerFactKey[?]]],
    injectedKeys: Ref[IO, Set[CompilerFactKey[?]]],
    carriedForward: Ref[IO, Map[CompilerFactKey[?], CacheEntry]],
    unchangedChecks: Ref[IO, Map[CompilerFactKey[?], Deferred[IO, Boolean]]],
    regeneratedCount: Ref[IO, Int]
) extends CompilationProcess
    with Logging {

  override def getFact[V <: CompilerFact, K <: CompilerFactKey[V]](
      key: K,
      ancestors: List[CompilerFactKey[?]]
  ): IO[Option[V]] =
    for {
      modifyResult <- modifyAtomicallyFor(key)
      _            <- resolve(key, modifyResult._1, ancestors)
                        .handleErrorWith(t => error[IO](s"Resolving (${key.getClass.getName}) $key failed.", t))
                        .flatMap(_ => modifyResult._1.complete(None).void) // safety net; no-op if already completed
                        .start
                        .whenA(modifyResult._2) // only the first requester runs the computation
      result       <- modifyResult._1.get
    } yield result.map(_.asInstanceOf[V])

  /** Register a fact, completing its [[Deferred]]. Re-registering the *same* value is a no-op (the push pattern:
    * concurrent generations over one file legitimately push identical sibling facts). Registering a **different** value
    * for an already-completed key is a double-produce — two producers computing conflicting values for one key, or a
    * value arriving after the key already concluded empty — and is reported as an internal error instead of being
    * silently dropped.
    */
  override def registerFact(fact: CompilerFact): IO[Unit] =
    modifyAtomicallyFor(fact.key()).flatMap { case (deferred, _) =>
      deferred.complete(Some(fact)).flatMap { won =>
        deferred.tryGet
          .flatMap {
            case Some(Some(existing)) if (existing eq fact) || existing == fact => IO.unit
            case Some(Some(_))                                                  =>
              errors.update(
                _ :+ CompilerError.global(s"Internal error: fact ${fact.key()} was produced twice with different values.")
              )
            case _                                                              =>
              errors.update(
                _ :+ CompilerError.global(
                  s"Internal error: fact ${fact.key()} was registered after its generation already concluded without it."
                )
              )
          }
          .unlessA(won)
      }
    }

  /** An injected fact is marked so the cache build classifies it as injected (accepted on sight next run) rather than
    * giving it the registering generation's dependency set — an injected fact is not derived from that generation's
    * inputs, and may even be read back by it.
    */
  override def registerInjectedFact(fact: CompilerFact): IO[Unit] =
    injectedKeys.update(_ + fact.key()) >> registerFact(fact)

  /** Satisfy a first-time request: accept the cached value if it is still valid, otherwise regenerate.
    *
    *   - An injected fact (registered directly, never produced by a processor) is always accepted — nothing can
    *     regenerate it, and its cached value is authoritative for the build target.
    *   - A fact with a stored value and recorded dependencies is accepted iff every dependency is unchanged.
    *   - Anything else (a value-less fact whose value is now actually needed, a leaf, or a fact with no prior) is
    *     regenerated.
    */
  private def resolve(
      key: CompilerFactKey[?],
      deferred: Deferred[IO, Option[CompilerFact]],
      ancestors: List[CompilerFactKey[?]]
  ): IO[Unit] =
    prior.get(key) match {
      case Some(entry) if entry.injected                              => deferred.complete(entry.value).void
      case Some(entry) if entry.value.isDefined && entry.directDeps.nonEmpty =>
        entry.directDeps.toList.forallM(depUnchanged).flatMap {
          case true  => acceptPrior(key, entry, deferred)
          case false => regenerate(key, ancestors)
        }
      case _                                                          => regenerate(key, ancestors)
    }

  /** Whether `key`'s value is unchanged since last run, memoized once per run. The validity oracle a parent uses for
    * each of its dependencies; it never materialises a value-less fact (see class doc).
    */
  private def depUnchanged(key: CompilerFactKey[?]): IO[Boolean] =
    for {
      newDeferred <- Deferred[IO, Boolean]
      modify      <- unchangedChecks.modify { checks =>
                       checks.get(key) match
                         case Some(existing) => (checks, (existing, false))
                         case None           => (checks.updated(key, newDeferred), (newDeferred, true))
                     }
      _           <- computeUnchanged(key)
                       .handleError(_ => false) // any failure ⇒ treat as changed (regenerate); fail-safe
                       .flatMap(modify._1.complete)
                       .void
                       .whenA(modify._2)
      result      <- modify._1.get
    } yield result

  private def computeUnchanged(key: CompilerFactKey[?]): IO[Boolean] =
    prior.get(key) match {
      case None                                      => false.pure[IO]   // new / previously failed ⇒ changed
      case Some(entry) if entry.injected             =>                  // accept on sight; carry forward so a run that
        carriedForward.update(_.updated(key, entry)).as(true)           // does not re-register it keeps it in the cache
      case Some(entry) if entry.value.isDefined      =>                  // leaf or serializable derived: recompute & compare
        getFactUntyped(key).map {
          case Some(current) => entry.value.exists(v => (v eq current) || v == current)
          case None          => false // no longer producible ⇒ changed
        }
      case Some(entry) if entry.directDeps.nonEmpty  =>                  // value-less derived: drill structurally
        entry.directDeps.toList
          .forallM(depUnchanged)
          .flatTap(unchanged => carriedForward.update(_.updated(key, entry)).whenA(unchanged))
      case Some(_)                                   => false.pure[IO]   // value-less leaf: cannot validate ⇒ changed
    }

  /** Accept the cached fact and carry its trace forward so it re-persists with the right metadata. */
  private def acceptPrior(
      key: CompilerFactKey[?],
      entry: CacheEntry,
      deferred: Deferred[IO, Option[CompilerFact]]
  ): IO[Unit] =
    directDependencies.update(_.updated(key, entry.directDeps)) >>
      deferred.complete(entry.value).void

  /** Run the processor, recording (via [[DependencyTrackingProcess]]) the facts it reads as this key's direct
    * dependencies. The processor completes the fact's [[Deferred]] itself via [[registerFact]]; the caller's safety net
    * handles the "produced nothing" case.
    *
    * The key is marked present in `directDependencies` up front (empty), so that even a generated leaf — which makes no
    * `getFact` calls — is recorded as *generated* (not mistaken for an injected fact, see [[buildCacheData]]). Reads then
    * accumulate into the same entry as they happen, before the fact becomes observable.
    *
    * The generation must account for its outcome: it ends with the fact registered, at least one error recorded, an
    * explicit abort (the sanctioned *decline*), or a missing dependency read (the failure is attributable upstream). A
    * generation with none of these — a missing producer for the key type, or a processor that silently produced
    * nothing — is reported as an internal error instead of being indistinguishable from a legitimate decline.
    */
  private def regenerate(key: CompilerFactKey[?], ancestors: List[CompilerFactKey[?]]): IO[Unit] =
    for {
      _          <- regeneratedCount.update(_ + 1)
      _          <- directDependencies.update(deps => deps.updated(key, deps.getOrElse(key, Set.empty)))
      sawMissing <- Ref.of[IO, Boolean](false)
      tracking    = new DependencyTrackingProcess(this, key, directDependencies, producedDuring, errors, sawMissing, ancestors)
      outcome    <- generator.generate(key).run(tracking).runS(Chain.empty).value
      es          = outcome.fold(identity, identity)
      _          <- errors.update(_ ++ es)
      _          <- reportUnaccountedOutcome(key, sawMissing).whenA(strictAccounting && outcome.isRight && es.isEmpty)
    } yield ()

  /** The generation neither erred nor declined; unless it registered its fact or read a missing input, nothing accounts
    * for its outcome — report it.
    */
  private def reportUnaccountedOutcome(key: CompilerFactKey[?], sawMissing: Ref[IO, Boolean]): IO[Unit] =
    for {
      missing    <- sawMissing.get
      registered <- facts.get.flatMap(_.get(key).traverse(_.tryGet)).map(_.flatten.flatten.isDefined)
      _          <- errors
                      .update(
                        _ :+ CompilerError.global(
                          s"Internal error: no processor produced a fact, an error, or a decline for $key."
                        )
                      )
                      .whenA(!missing && !registered)
    } yield ()

  /** A validation-path read (recompute-and-compare). It starts a fresh chain (`Nil`): the recursion guard only matters
    * for facts materialised through a processor generation, and the value-less `SemValue`-bearing facts the guard
    * protects are never materialised here — they take the structural-drill branch of [[computeUnchanged]].
    */
  private def getFactUntyped(key: CompilerFactKey[?]): IO[Option[CompilerFact]] =
    getFact(key.asInstanceOf[CompilerFactKey[CompilerFact]], Nil)

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

  /** Build the cache to persist after this run. Two sources are merged:
    *
    *   - facts **materialised** this run (regenerated or accepted) — a fresh entry with their value and recorded
    *     dependencies. Only facts that resolved to a value are included, so failures are never cached and re-surface.
    *   - facts **validated structurally but not materialised** (value-less facts proven unchanged) — their prior
    *     edges-only entry, carried forward so the graph stays drillable. A fact neither materialised nor carried (no
    *     longer reachable, or removed) drops out, so the cache self-prunes.
    *
    * A materialised fact gets its dependency set from one of two places: its own `directDependencies` entry (it was
    * generated), or — for a fact **pushed** by a processor for a key other than the one being generated — the final
    * dependency set of the generation that pushed it (recorded in `producedDuring`): a pushed fact is a function of
    * exactly what its producing generation read, so those edges validate it soundly next run. Only facts registered
    * with neither record — explicitly injected ones ([[registerInjectedFact]]), or direct out-of-generation
    * registrations — are marked injected and accepted from the cache on sight, since no processor can reproduce them.
    */
  def buildCacheData(): IO[FactCacheData] =
    for {
      factMap     <- currentFacts()
      deps        <- directDependencies.get
      pushedBy    <- producedDuring.get
      injected    <- injectedKeys.get
      carried     <- carriedForward.get
      regenerated <- regeneratedCount.get
      _           <- debug[IO](s"Incremental run: regenerated $regenerated fact(s); ${factMap.size} materialised, " +
                       s"${carried.size} validated unchanged without recompute.")
    } yield {
      val fresh = factMap.map { case (key, fact) =>
        val recordedDeps =
          if (injected.contains(key)) None
          else deps.get(key).orElse(pushedBy.get(key).map(producer => deps.getOrElse(producer, Set.empty)))
        key -> CacheEntry(Some(fact), recordedDeps.getOrElse(Set.empty), injected = recordedDeps.isEmpty)
      }
      FactCacheData(FactCache.CACHE_VERSION, fresh ++ carried.view.filterKeys(k => !fresh.contains(k)).toMap)
    }
}

object IncrementalFactGenerator {

  /** @param strictAccounting
    *   when true, a generation that ends with no fact, no error, no explicit abort (decline), and no missing dependency
    *   read is reported as an internal error ("no processor produced a fact, an error, or a decline"). This invariant
    *   only holds for a *complete* processor bundle — a session running all plugins — so it is enabled by
    *   [[CompilationSession]] and off by default for partial bundles (test harnesses that inject source-phase facts
    *   instead of carrying their processors).
    */
  def create(
      generator: CompilerProcessor,
      prior: Option[FactCacheData],
      strictAccounting: Boolean = false
  ): IO[IncrementalFactGenerator] =
    for {
      errors          <- Ref.of[IO, Chain[CompilerError]](Chain.empty)
      facts           <- Ref.of[IO, Map[CompilerFactKey[?], Deferred[IO, Option[CompilerFact]]]](Map.empty)
      deps            <- Ref.of[IO, Map[CompilerFactKey[?], Set[CompilerFactKey[?]]]](Map.empty)
      producedDuring  <- Ref.of[IO, Map[CompilerFactKey[?], CompilerFactKey[?]]](Map.empty)
      injectedKeys    <- Ref.of[IO, Set[CompilerFactKey[?]]](Set.empty)
      carriedForward  <- Ref.of[IO, Map[CompilerFactKey[?], CacheEntry]](Map.empty)
      unchangedChecks <- Ref.of[IO, Map[CompilerFactKey[?], Deferred[IO, Boolean]]](Map.empty)
      regenerated     <- Ref.of[IO, Int](0)
    } yield new IncrementalFactGenerator(
      generator,
      prior.map(_.entries).getOrElse(Map.empty),
      strictAccounting,
      errors,
      facts,
      deps,
      producedDuring,
      injectedKeys,
      carriedForward,
      unchangedChecks,
      regenerated
    )
}
