package com.vanillasource.eliot.eliotc.compiler

import cats.data.Chain
import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.compiler.cache.{CacheEntry, DependencyTrackingProcess, FactCacheData}
import com.vanillasource.eliot.eliotc.feedback.{CompilerError, Logging}
import com.vanillasource.eliot.eliotc.processor.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor}

/** Cache-aware fact generator implementing incremental compilation by backward, demand-driven validation.
  *
  * It keeps the same concurrent core as a plain generator — a [[Deferred]]-per-key map that computes each fact at most
  * once per run, per-fiber generation, and an ancestor chain for recursion detection threaded explicitly through the
  * per-generation [[DependencyTrackingProcess]] (the `ancestors` parameter of [[getFact]]) — and decides, per
  * dependency, whether it changed since last run via [[depUnchanged]]:
  *
  *   - a dependency with a **stored value** (every leaf, and every persistable derived fact) is recomputed and compared
  *     against what the cache holds ([[com.vanillasource.eliot.eliotc.compiler.cache.CacheEntry.matches]]) — this is
  *     the equality cutoff: a changed leaf whose derived value recomputes equal stops propagation. A backend that
  *     stores values by content hash answers it without reading the stored side at all;
  *   - a **derived** dependency is validated **structurally** by drilling through its recorded `directDeps` to the
  *     leaves, *without materialising anything*. A fact is a pure function of its recorded inputs, so dependencies that
  *     all hold mean the fact holds — there is nothing to recompute and nothing to compare. A value-less one has its
  *     edges-only entry carried forward so it stays drillable; a value-bearing one simply stays untouched and is
  *     retained. Only when a dependency has genuinely moved is the fact recomputed and compared.
  *
  * Consequently a no-change run materialises **no fact value at all** beyond the world leaves it must re-read — not a
  * `SemValue`, not a monomorphic value, nothing the build does not actually consume. A fact is regenerated only when a
  * genuinely changed dependent reads it.
  *
  * Leaf facts — those whose recorded dependency set is empty, e.g. a `stat` of a source file — are always regenerated,
  * forming the boundary with the external world. Failures are never cached, so a fact that failed has no prior entry
  * and is regenerated (re-emitting its error) on every run until fixed.
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
    carriedForward: Ref[IO, Map[CompilerFactKey[?], CacheEntry]],
    unchangedChecks: Ref[IO, Map[CompilerFactKey[?], Deferred[IO, Boolean]]],
    regeneratedKeysRef: Ref[IO, Set[CompilerFactKey[?]]]
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
                _ :+ CompilerError
                  .global(s"Internal error: fact ${fact.key()} was produced twice with different values.")
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

  /** Satisfy a first-time request: accept the cached value if it is still valid, otherwise regenerate.
    *
    *   - A fact with a stored value and recorded dependencies is accepted iff every dependency is unchanged.
    *   - Anything else (a value-less fact whose value is now actually needed, a leaf — including a fact that was
    *     registered outside any generation, which has no recorded dependencies — or a fact with no prior) is
    *     regenerated.
    */
  private def resolve(
      key: CompilerFactKey[?],
      deferred: Deferred[IO, Option[CompilerFact]],
      ancestors: List[CompilerFactKey[?]]
  ): IO[Unit] =
    prior.get(key) match {
      case Some(entry) if entry.hasValue && entry.directDeps.nonEmpty =>
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

  /** Whether the prior entry for `key` still stands, decided the cheapest way that is sound for its shape.
    *
    *   - **No prior entry** — new, or previously failed, so changed.
    *   - **A world leaf** (no recorded dependencies) — the only thing that can tell is running it: recompute and
    *     compare against what the cache holds. This is the boundary with the outside world and the one place the
    *     equality cutoff earns its name.
    *   - **A derived fact whose dependencies all hold** — unchanged *by construction*, with nothing recomputed and
    *     nothing materialised. A fact is a pure function of its recorded inputs, which is the assumption
    *     [[acceptPrior]] already rests on; recomputing it here would compare the stored value against itself. A
    *     value-less one is carried forward so it stays drillable, a value-bearing one needs nothing — untouched keys
    *     retain their prior entry ([[buildCacheData]]).
    *   - **A derived fact whose dependencies moved** — recompute and compare if there is a value to compare against.
    *     Equality here is what stops propagation: a changed input whose result recomputes the same invalidates nothing
    *     further. With no value the drill has failed and there is nothing left to try.
    */
  private def computeUnchanged(key: CompilerFactKey[?]): IO[Boolean] =
    prior.get(key) match {
      case None                                    => false.pure[IO]
      case Some(entry) if entry.directDeps.isEmpty => recomputeAndCompare(key, entry)
      case Some(entry)                             =>
        entry.directDeps.toList.forallM(depUnchanged).flatMap {
          case true  => carriedForward.update(_.updated(key, entry)).unlessA(entry.hasValue).as(true)
          case false => recomputeAndCompare(key, entry)
        }
    }

  private def recomputeAndCompare(key: CompilerFactKey[?], entry: CacheEntry): IO[Boolean] =
    if (entry.hasValue)
      getFactUntyped(key).map {
        case Some(current) => entry.matches(current)
        case None          => false // no longer producible ⇒ changed
      }
    else false.pure[IO]

  /** Accept the cached fact and carry its trace forward so it re-persists with the right metadata. This is the one
    * place a stored value is actually read: everything else about an entry — whether it has a value, and whether a
    * recomputed one matches it — is answerable without materialising.
    */
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
    * `getFact` calls — is recorded as *generated* (not mistaken for an injected fact, see [[buildCacheData]]). Reads
    * then accumulate into the same entry as they happen, before the fact becomes observable.
    *
    * The generation must account for its outcome: it ends with the fact registered, at least one error recorded, an
    * explicit abort (the sanctioned *decline*), or a missing dependency read (the failure is attributable upstream). A
    * generation with none of these — a missing producer for the key type, or a processor that silently produced nothing
    * — is reported as an internal error instead of being indistinguishable from a legitimate decline.
    */
  private def regenerate(key: CompilerFactKey[?], ancestors: List[CompilerFactKey[?]]): IO[Unit] =
    for {
      _          <- regeneratedKeysRef.update(_ + key)
      _          <- directDependencies.update(deps => deps.updated(key, deps.getOrElse(key, Set.empty)))
      sawMissing <- Ref.of[IO, Boolean](false)
      tracking    =
        new DependencyTrackingProcess(this, key, directDependencies, producedDuring, errors, sawMissing, ancestors)
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

  /** The keys this run regenerated (ran a processor for from scratch) rather than accepting from the prior cache. On a
    * warm build over an unchanged tree this is exactly the set of world leaves — facts with no recorded dependencies,
    * e.g. every `FileStat`, the `OutputFileStat` digest, and the `UpToDate` anchor constant — which are always
    * regenerated as the boundary with the external world.
    */
  def regeneratedKeys(): IO[Set[CompilerFactKey[?]]] = regeneratedKeysRef.get

  /** The regenerated keys that were **not** world leaves in the prior cache — facts re-run from scratch despite having
    * recorded dependencies (or being newly seen this run). On a warm build over an unchanged tree this MUST be empty:
    * every non-leaf fact validates structurally and is accepted from cache. A non-empty result is a warm-build
    * regression surfacing as needless recomputation — a contributor whose input-less generation lost its `UpToDate`
    * anchor (an edge-less entry read as a leaf), an equality-unstable fact, or a genuinely missing prior — see
    * `docs/incremental-compilation.md` §3 and §8.
    */
  def nonLeafRegenerations(): IO[Set[CompilerFactKey[?]]] =
    regeneratedKeysRef.get.map(_.filterNot(key => prior.get(key).exists(_.directDeps.isEmpty)))

  /** Build the cache to persist after this run. Three sources are merged, later winning on key collision:
    *
    *   - **retained prior** entries — every prior entry for a key this run did **not** touch (never demanded: neither
    *     resolved, materialised, nor drilled). This is what makes the cache *accumulate* rather than replace: facts
    *     reachable only from a *different* configuration (e.g. another example's `main`) survive a build that never
    *     asked for them, so switching back stays warm. Retained entries are never served stale — the next demand
    *     re-validates their dependencies down to the world leaves before accepting them.
    *   - facts **validated structurally but not materialised** (value-less facts proven unchanged) — their prior
    *     edges-only entry, carried forward so the graph stays drillable.
    *   - facts **materialised** this run (regenerated or accepted) — a fresh entry with their value and recorded
    *     dependencies. Only facts that resolved to a value are included, so failures are never cached and re-surface.
    *
    * The retention explicitly **excludes** every key this run resolved (`directDependencies`) or carried, so a key that
    * was regenerated this run and *failed* (or recomputed to a new value) can never keep its stale prior value: its
    * fresh outcome — a new entry, or, for a failure, no entry at all — is authoritative. Only genuinely untouched keys
    * fall through to the prior. This is the one subtlety that keeps accumulation sound: without it, a fact that started
    * failing would be masked by its own stale success next run.
    *
    * A materialised fact gets its dependency set from one of two places: its own `directDependencies` entry (it was
    * generated), or — for a fact **pushed** by a processor for a key other than the one being generated — the final
    * dependency set of the generation that pushed it (recorded in `producedDuring`): a pushed fact is a function of
    * exactly what its producing generation read, so those edges validate it soundly next run. A fact registered with
    * neither record (a direct out-of-generation registration, e.g. a test harness seeding source facts) persists with
    * an empty dependency set — leaf semantics, always regenerated, never accepted blindly.
    */
  def buildCacheData(): IO[FactCacheData] =
    for {
      factMap     <- currentFacts()
      deps        <- directDependencies.get
      pushedBy    <- producedDuring.get
      carried     <- carriedForward.get
      regenerated <- regeneratedKeysRef.get.map(_.size)
      _           <- debug[IO](
                       s"Incremental run: regenerated $regenerated fact(s); ${factMap.size} materialised, " +
                         s"${carried.size} validated unchanged without recompute."
                     )
    } yield {
      val fresh    = factMap.map { case (key, fact) =>
        val recordedDeps = deps.get(key).orElse(pushedBy.get(key).map(producer => deps.getOrElse(producer, Set.empty)))
        key -> CacheEntry(Some(fact), recordedDeps.getOrElse(Set.empty))
      }
      val touched  = factMap.keySet ++ deps.keySet ++ carried.keySet // resolved, materialised, or drilled this run
      val retained = prior.view.filterKeys(k => !touched(k)).toMap   // untouched prior facts accumulate
      FactCacheData(retained ++ carried ++ fresh)
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
      carriedForward  <- Ref.of[IO, Map[CompilerFactKey[?], CacheEntry]](Map.empty)
      unchangedChecks <- Ref.of[IO, Map[CompilerFactKey[?], Deferred[IO, Boolean]]](Map.empty)
      regenerated     <- Ref.of[IO, Set[CompilerFactKey[?]]](Set.empty)
    } yield new IncrementalFactGenerator(
      generator,
      prior.map(_.entries).getOrElse(Map.empty),
      strictAccounting,
      errors,
      facts,
      deps,
      producedDuring,
      carriedForward,
      unchangedChecks,
      regenerated
    )
}
