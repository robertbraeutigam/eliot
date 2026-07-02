package com.vanillasource.eliot.eliotc.monomorphize.unify

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Pattern unification on SemValues with a postponement queue. All operations return a new Unifier instance — no
  * mutation occurs.
  *
  * @param metaStore
  *   The current meta store
  * @param depth
  *   The current binding depth (for fresh variable generation)
  * @param postponed
  *   Queue of postponed unification problems (when a meta's spine is not pattern)
  * @param errors
  *   Accumulated errors with source positions and (when available) the expected and actual semantic values.
  *
  * This is *pure definitional (value) equality* with metavariable solving on top: it forces both sides through the
  * evaluator (= normalises) and compares. There is no notion of "assignability" or directional widening here.
  * Directional coercion (e.g. an `Int[0,5]` used where an `Int[0,10]` is expected) is a separate concern handled
  * outside the unifier by a user-defined `Coerce` ability that the checker inserts in check mode.
  *
  * Per-metavariable metadata lives in [[metaRoles]] — one [[MetaRole]] per meta id (D2), the single map that
  * subsumes the former scattered side-sets. The combinable/candidates machinery supporting the Phase 4 `Combine`
  * ability is carried there: a metavariable in a covariant position (a `match` result, a result-position type
  * parameter) is marked [[MetaRole.Instantiation]] with `combinable = true`; when a *second, definitionally-unequal*
  * type is unified against such a meta, instead of failing the unification (first-candidate-wins) we accumulate it as
  * a candidate constraint and leave the meta solved to its first candidate. The checker later resolves these
  * candidates to their `Combine` join at drain time
  * ([[com.vanillasource.eliot.eliotc.monomorphize.check.Checker.resolveCombines]]). A meta is tainted (`combinable`
  * cleared) the moment it appears in a [[VPi]] domain — a contravariant position where joining would be unsound. When
  * no `Combine` instance applies (or the meta was tainted), the checker re-unifies the candidates strictly, surfacing
  * the ordinary mismatch — so unification stays first-candidate-wins for everything that has no `Combine` instance.
  *
  * @param metaRoles
  *   Per-meta-id role. Absence ⇒ [[MetaRole.Plain]]. The unifier reads/writes the [[MetaRole.Instantiation]] aspects
  *   during unification (combinable, candidates); the checker writes the carrier-kind / abstract-assoc / upper-bound /
  *   combine-resolved aspects through the role-transition methods below.
  */
case class Unifier(
    metaStore: MetaStore,
    depth: Int,
    postponed: List[(SemValue, SemValue, Sourced[String])],
    errors: List[UnifyError],
    metaRoles: Map[Int, MetaRole] = Map.empty
) {

  /** The role of a metavariable, defaulting to [[MetaRole.Plain]] when unclassified. */
  def roleOf(id: Int): MetaRole = metaRoles.getOrElse(id, MetaRole.Plain)

  /** Replace a metavariable's role. */
  def setRole(id: Int, role: MetaRole): Unifier = copy(metaRoles = metaRoles.updated(id, role))

  /** Transform a metavariable's role in place (reading its current value, defaulting to [[MetaRole.Plain]]). */
  def updateRole(id: Int, f: MetaRole => MetaRole): Unifier = setRole(id, f(roleOf(id)))

  /** Whether a metavariable is currently combinable (an un-tainted [[MetaRole.Instantiation]]). */
  def isCombinable(id: Int): Boolean = roleOf(id) match {
    case i: MetaRole.Instantiation => i.combinable
    case _                         => false
  }

  /** A metavariable's accumulated `Combine` candidates (empty unless it is an [[MetaRole.Instantiation]]). */
  def candidatesOf(id: Int): List[(SemValue, Sourced[String])] = roleOf(id) match {
    case i: MetaRole.Instantiation => i.candidates
    case _                         => Nil
  }

  /** Every [[MetaRole.Instantiation]] meta with accumulated candidates that combine resolution has not yet consumed,
    * as `(id, candidates)`. Drives the checker's combine-resolution loop.
    */
  def unresolvedCandidateMetas: List[(Int, List[(SemValue, Sourced[String])])] =
    metaRoles.toList.collect {
      case (id, i: MetaRole.Instantiation) if i.candidates.nonEmpty && !i.combineResolved => (id, i.candidates)
    }

  /** All deferred upper-bound obligations across every instantiation meta, as `(id, expected, context)`. */
  def pendingUpperBounds: List[(MetaId, SemValue, Sourced[String])] =
    metaRoles.toList.collect { case (id, i: MetaRole.Instantiation) => i.upperBounds.map((MetaId(id), _)) }.flatten
      .map { case (id, (expected, context)) => (id, expected, context) }

  /** Every higher-kinded carrier meta (an [[MetaRole.Instantiation]] with a recorded `carrierKind`), as
    * `(id, (expectedKind, context))`. Drives post-drain carrier-kind verification.
    */
  def carrierMetas: List[(Int, (SemValue, Sourced[String]))] =
    metaRoles.toList.collect { case (id, MetaRole.Instantiation(_, _, _, _, Some(carrier))) => (id, carrier) }

  /** The raw ids of all abstract associated-ability-type placeholder metas. These are the metas the post-check
    * finalizer protects from defaulting to `VType`, so a constraint-covered reference can stay abstract.
    */
  def abstractAssocMetaIds: Set[Int] = metaRoles.collect { case (id, _: MetaRole.AbstractAssoc) => id }.toSet

  /** Register a metavariable as combinable (covariant). Called by the checker when it allocates an implicit
    * type-parameter instantiation meta (see [[com.vanillasource.eliot.eliotc.monomorphize.check.Checker.peelLams]]).
    * Promotes a [[MetaRole.Plain]] meta to a combinable [[MetaRole.Instantiation]], preserving any fields already set.
    */
  def markCombinable(id: MetaId): Unifier = updateRole(
    id.value,
    {
      case i: MetaRole.Instantiation => i.copy(combinable = true)
      case _                         => MetaRole.Instantiation(combinable = true)
    }
  )

  /** Record a higher-kinded instantiation meta's expected kind (`[F[_]]` carrier). */
  def recordCarrierKind(id: MetaId, expectedKind: SemValue, context: Sourced[String]): Unifier = updateRole(
    id.value,
    {
      case i: MetaRole.Instantiation => i.copy(carrierKind = Some((expectedKind, context)))
      case _                         => MetaRole.Instantiation(carrierKind = Some((expectedKind, context)))
    }
  )

  /** Record an abstract associated-ability-type standing placeholder. */
  def recordAbstractAssoc(id: MetaId, fqn: ValueFQN): Unifier = setRole(id.value, MetaRole.AbstractAssoc(fqn))

  /** Defer a "result fits expected" obligation on an instantiation meta (discharged after combine resolution). */
  def recordUpperBound(id: MetaId, expected: SemValue, context: Sourced[String]): Unifier = updateRole(
    id.value,
    {
      case i: MetaRole.Instantiation => i.copy(upperBounds = i.upperBounds :+ (expected, context))
      case other                     => other
    }
  )

  /** Mark an instantiation meta's candidates as consumed by combine resolution (resolves at most once). */
  def recordCombineResolved(id: MetaId): Unifier = updateRole(
    id.value,
    {
      case i: MetaRole.Instantiation => i.copy(combineResolved = true)
      case other                     => other
    }
  )

  /** Unify two semantic values, reporting errors with the given context message and source position.
    *
    * Before forcing, intercept a *contribution to an already-solved combinable metavariable* — a concrete type unified
    * against a combinable meta (always the expected/right side, since contributions arrive via `check(arg, ?A)`) that is
    * already solved to a different first candidate. Rather than the ordinary first-candidate-wins mismatch, the new
    * candidate is accumulated for `Combine` resolution at drain time and the unification succeeds. (The first candidate
    * is recorded when the meta is solved in [[solveMeta]].) Everything else — including the result-against-expected
    * direction `?A ~ E`, which stays first-candidate-wins so the checker's check-mode `Coerce` handles it — falls
    * through to [[unifyForced]], keeping `unify` pure definitional equality.
    */
  def unify(l: SemValue, r: SemValue, context: Sourced[String]): Unifier = r match {
    case VMeta(id, Spine.SNil) if isCombinable(id.value) && metaStore.lookup(id).nonEmpty =>
      tryUnifyForced(l, metaStore.lookup(id).get, context) match {
        case UnifyResult.Unified(u)       => u
        case UnifyResult.Contradiction(_) => recordCandidate(id, l, context)
      }
    case _                                                                                =>
      unifyForced(l, r, context)
  }

  /** Speculatively unify `l` against `r` without committing a mismatch (D5), returning an explicit [[UnifyResult]]
    * rather than forcing callers to diff [[errors]] before and after a [[unify]] call. A [[UnifyResult.Unified]]
    * carries the unifier with every solution applied (safe to commit); a [[UnifyResult.Contradiction]] carries the same
    * partial solutions but with the new mismatch errors stripped, leaving the report-or-recover decision to the caller
    * (a check-mode `Coerce` insertion, a `Combine` candidate, or a [[drain]] re-postpone).
    */
  def tryUnify(l: SemValue, r: SemValue, context: Sourced[String]): UnifyResult =
    speculate(unify(l, r, context))

  /** Like [[tryUnify]] but over [[unifyForced]], bypassing the combinable-contribution interception in [[unify]] — used
    * by that interception itself to probe a contribution against a solved combinable meta's first candidate.
    */
  private def tryUnifyForced(l: SemValue, r: SemValue, context: Sourced[String]): UnifyResult =
    speculate(unifyForced(l, r, context))

  /** Classify a unification result `after` (produced from `this`) as [[UnifyResult.Unified]] or
    * [[UnifyResult.Contradiction]] by whether it added any error, stripping the new errors in the latter case so a
    * caller can recover without them. This is the single home of the former scattered `errors.size`-delta check (D5);
    * because `after` only ever *prepends* errors to `this`, restoring `this`'s error list drops exactly the new ones
    * while preserving every metavariable solved along the way.
    */
  private def speculate(after: Unifier): UnifyResult =
    if (after.errors.size == errors.size) UnifyResult.Unified(after)
    else UnifyResult.Contradiction(after.copy(errors = errors))

  private def unifyForced(l: SemValue, r: SemValue, context: Sourced[String]): Unifier = {
    val fl = Evaluator.force(l, metaStore)
    val fr = Evaluator.force(r, metaStore)
    (fl, fr) match {
      case (VType, VType) => this

      case (VConst(g1), VConst(g2)) =>
        if (!groundEquals(g1, g2)) addMismatch(fl, fr, context)
        else this

      case (VPi(d1, c1), VPi(d2, c2)) =>
        // Domains are contravariant: any combinable meta appearing in a domain is tainted (removed from `combinable`),
        // so a meta that flows into a parameter-input position is never joined. This is the soundness gate for Phase 4
        // (see the `useTwice[A](f: A -> Unit, x: A, y: A)` counterexample).
        val (fresh, u1) = freshVar()
        u1.taintMetasIn(d1)
          .taintMetasIn(d2)
          .unify(d1, d2, context)
          .unify(c1(fresh), c2(fresh), context)

      case (VLam(_, c1), VLam(_, c2)) =>
        val (fresh, u1) = freshVar()
        u1.unify(c1(fresh), c2(fresh), context)

      // Eta: VLam vs non-lambda
      case (VLam(_, c), other)        =>
        val (fresh, u1) = freshVar()
        u1.unify(c(fresh), Evaluator.applyValue(other, fresh), context)

      case (other, VLam(_, c))                                =>
        val (fresh, u1) = freshVar()
        u1.unify(Evaluator.applyValue(other, fresh), c(fresh), context)

      // Neutral-neutral: same head, same spine length
      case (VNeutral(h1, sp1), VNeutral(h2, sp2)) if h1 == h2 =>
        unifySpines(fl, fr, sp1, sp2, context)

      // Meta solving (pattern rule)
      case (VMeta(id, spine), rhs)                            =>
        solveMeta(id, spine, rhs, context)

      case (lhs, VMeta(id, spine))                                        =>
        solveMeta(id, spine, lhs, context)

      // VTopDef equality by FQN: same constructor → unify spines pointwise (definitional equality). Differing bounds
      // (e.g. Int[0,5] vs Int[0,10]) are genuinely unequal values and are rejected here; directional widening is the
      // Coerce ability's job in check mode, not the unifier's.
      case (VTopDef(fqn1, _, sp1), VTopDef(fqn2, _, sp2)) if fqn1 == fqn2 =>
        unifySpines(fl, fr, sp1, sp2, context)

      // Stuck native equality by FQN: a native application is *not* injective (`add(1, 3) == add(2, 2)`), so it is
      // never injectivity-decomposed against a meta (see `tryDecomposeApplied`); two stuck natives are definitionally
      // equal only when the same operation is applied to pointwise-equal arguments.
      case (VStuckNative(fqn1, sp1), VStuckNative(fqn2, sp2)) if fqn1 == fqn2 =>
        unifySpines(fl, fr, sp1, sp2, context)

      case _ =>
        addMismatch(fl, fr, context)
    }
  }

  /** Try to solve a meta. Empty spine solves directly. Non-empty spine attempts injectivity-style decomposition against
    * a rigid applied rhs (see [[tryDecomposeApplied]]); failing that, postpones.
    */
  private def solveMeta(id: MetaId, spine: Spine, rhs: SemValue, context: Sourced[String]): Unifier =
    metaStore.lookup(id) match {
      case Some(solved) =>
        // Already solved — unify the solution (with spine applied) against rhs. (A contribution to a solved combinable
        // meta is intercepted earlier, in `unify`, before forcing; by here the meta has already been forced away.)
        val applied = spine.toList.foldLeft(solved)(Evaluator.applyValue)
        unify(applied, rhs, context)
      case None         =>
        rhs match {
          case VMeta(rhsId, _) if rhsId.value == id.value =>
            this // Same unsolved meta — trivially equal, no solve needed
          case _                                          =>
            val spineList = spine.toList
            if (spineList.isEmpty) {
              // Empty spine — solve directly, but first run the occurs-check: solving `?id := rhs` where `id` occurs in
              // `rhs` would build an infinite type (the `x x` / Y-combinator route) and loop `Evaluator.force` once the
              // cyclic meta is in the store. This is precondition #2 of the termination model — reject it instead.
              if (occursIn(id, rhs)) addOccursError(context)
              else {
                // For a combinable meta also record the first candidate, so the checker's combine resolution sees the
                // full candidate set.
                val solvedU = copy(metaStore = metaStore.solve(id, rhs))
                if (isCombinable(id.value)) solvedU.recordCandidate(id, rhs, context) else solvedU
              }
            } else {
              // Non-empty spine — try injectivity decomposition first, otherwise postpone.
              tryDecomposeApplied(id, spineList, rhs, context)
                .getOrElse(copy(postponed = (VMeta(id, spine), rhs, context) :: postponed))
            }
        }
    }

  /** Injectivity-based decomposition of `?id s1..sm ~ H r1..rn`, mirroring what GHC and Scala 3 do for applied types:
    * type constructor application is treated as injective, so the decomposition is unique.
    *
    * Two arity regimes, both sound because the rhs head is rigid (a type constructor or bound variable, never a
    * projection/constant lambda):
    *   - **Equal arity** (`m == n`): `?id := H` (unapplied) plus pointwise spine unification `s_i ~ r_i`.
    *   - **Partial application** (`n > m`): `?id := H r1..r(n-m)` (the head applied to the leading prefix) plus
    *     pointwise unification of the trailing `m` args `s_i ~ r(n-m+i)`. This is what lets a *carrier* meta solve to a
    *     type constructor partially applied to a prefix — `?F[A] ~ AbortCarrier[G, A] ⟹ ?F := AbortCarrier[G]` (the effects
    *     discharge path) — exactly as `?F[A] ~ Box[A] ⟹ ?F := Box` works for the equal-arity case. The solution is
    *     unique: with `H` injective, only `?F = H r1..r(n-m)` makes `?F s1..sm = H r1..rn` hold for all the trailing
    *     args.
    *
    * Fires only when the rhs's head is rigid and `n >= m`:
    *   - [[VTopDef]] with no cached body — data-type constructors are bound this way and never β-reduce. Cached
    *     VTopDefs (user-defined type aliases) would have been unfolded by the ambient [[Evaluator.force]] before
    *     reaching here, so we should not see them; the `None` guard is a belt-and-braces check.
    *   - [[VNeutral]] — rigid bound variables with a spine.
    *
    * `n < m` and non-rigid rhs shapes return `None`, leaving the caller to postpone. Cases like `?A [?B] ~
    * Function [Int, String]` have multiple valid solutions and correctly postpone. A [[VStuckNative]] head
    * (`add(x, y)`, `min(x, y)`, …) is deliberately **not** in the rigid set: a native application is non-injective, so
    * decomposing `?F [a] ~ add(x, y)` to `?F := add` would be unsound — it falls through to `None` and postpones,
    * leaving the meta for `renormalize`/defaulting once the arguments concretise.
    */
  private def tryDecomposeApplied(
      id: MetaId,
      metaSpine: List[SemValue],
      rhs: SemValue,
      context: Sourced[String]
  ): Option[Unifier] = rhs match {
    case VTopDef(fqn, None, rhsSpine) =>
      decomposeSpines(id, metaSpine, rhsSpine, prefix => VTopDef(fqn, None, prefix), context)
    case VNeutral(head, rhsSpine)     =>
      decomposeSpines(id, metaSpine, rhsSpine, prefix => VNeutral(head, prefix), context)
    case _                            => None
  }

  /** Decompose `?id metaSpine ~ head rhsSpine` by injectivity. `rebuildHead` reattaches the leading prefix of
    * `rhsSpine` (everything beyond the trailing `metaSpine.length` args) to the rigid head, which becomes `?id`'s
    * solution; the trailing args unify pointwise against `metaSpine`. Returns `None` when the rhs is under-applied
    * relative to the meta (`n < m`), which has no injective solution.
    */
  private def decomposeSpines(
      id: MetaId,
      metaSpine: List[SemValue],
      rhsSpine: Spine,
      rebuildHead: Spine => SemValue,
      context: Sourced[String]
  ): Option[Unifier] = {
    val rhsList = rhsSpine.toList
    if (rhsList.length < metaSpine.length) None
    else {
      val (prefix, suffix) = rhsList.splitAt(rhsList.length - metaSpine.length)
      val headWithPrefix   = rebuildHead(prefix.foldLeft(Spine.SNil: Spine)(_ :+ _))
      // Occurs-check the injective solution too: `?F[A] ~ AbortCarrier[?F, A]` would solve `?F := AbortCarrier[?F]` (infinite).
      // Returning None here postpones rather than committing the cycle, so it surfaces as an unresolved type, never a
      // silently-accepted recursive one.
      if (occursIn(id, headWithPrefix)) None
      else {
        val solvedMeta = copy(metaStore = metaStore.solve(id, headWithPrefix))
        Some(metaSpine.zip(suffix).foldLeft(solvedMeta) { case (u, (l, r)) => u.unify(l, r, context) })
      }
    }
  }

  /** Drain the postponement queue, re-attempting postponed unifications. Repeats until stable. */
  def drain(): Unifier = {
    @scala.annotation.tailrec
    def loop(u: Unifier): Unifier = {
      val current = u.postponed
      if (current.isEmpty) u
      else {
        val (result, changed) = current.foldLeft((u.copy(postponed = Nil), false)) {
          case ((acc, anyChanged), (l, r, ctx)) =>
            acc.tryUnify(l, r, ctx) match {
              case UnifyResult.Contradiction(after) =>
                // Could not discharge — keep any solutions, drop the error, and re-postpone (no progress).
                (after.copy(postponed = (l, r, ctx) :: after.postponed), anyChanged)
              case UnifyResult.Unified(after)       =>
                // A constraint re-postponed inside unify (e.g. a non-pattern spine) is not real progress.
                if (after.postponed.length > acc.postponed.length) (after, anyChanged)
                else (after, true)
            }
        }
        if (changed) loop(result) else result
      }
    }
    loop(this)
  }

  private def unifySpines(l: SemValue, r: SemValue, sp1: Spine, sp2: Spine, context: Sourced[String]): Unifier = {
    val l1 = sp1.toList
    val l2 = sp2.toList
    if (l1.length == l2.length) l1.zip(l2).foldLeft(this) { case (u, (a, b)) => u.unify(a, b, context) }
    else addMismatch(l, r, context)
  }

  /** Append a candidate type (with its source context) to a combinable meta's accumulated constraints. Only an
    * [[MetaRole.Instantiation]] meta accumulates candidates; any other role is left unchanged (callers guard on
    * [[isCombinable]], so this is defensive).
    */
  private def recordCandidate(id: MetaId, rhs: SemValue, context: Sourced[String]): Unifier = updateRole(
    id.value,
    {
      case i: MetaRole.Instantiation => i.copy(candidates = i.candidates :+ (rhs, context))
      case other                     => other
    }
  )

  /** Taint every metavariable occurring in `sv`: clear `combinable` on each one's [[MetaRole.Instantiation]] (its
    * candidates and other fields are preserved, so a tainted meta still falls to strict re-unification). Used on
    * [[VPi]] domains.
    */
  private def taintMetasIn(sv: SemValue): Unifier =
    metasOf(sv).foldLeft(this) { (u, id) =>
      u.updateRole(
        id,
        {
          case i: MetaRole.Instantiation => i.copy(combinable = false)
          case other                     => other
        }
      )
    }

  /** Collect the ids of all metavariables occurring anywhere in `sv` — including inside spines and under binders.
    * Binders are entered by applying a fresh rigid variable, mirroring [[unify]]'s treatment.
    *
    * Crucially this does **not** follow a metavariable's *solution*: a combinable meta that has already been solved to
    * its first candidate is still recorded by id. This is what lets a contravariant use taint a combinable meta even
    * when an earlier (covariant-looking) argument already pinned it — e.g. `useTwice[A](x: A, y: A, f: A -> Unit)`,
    * where `f`'s domain reaches the meta only after `x`/`y` have solved it. Other shapes are forced so type aliases and
    * cached definitions are seen through.
    */
  private def metasOf(sv: SemValue): Set[Int] = sv match {
    case VMeta(id, spine) => spine.toList.flatMap(metasOf).toSet + id.value
    case _                =>
      Evaluator.force(sv, metaStore) match {
        case VMeta(id, spine)                  => spine.toList.flatMap(metasOf).toSet + id.value
        case VTopDef(_, _, spine)              => spine.toList.flatMap(metasOf).toSet
        case VStuckNative(_, spine)            => spine.toList.flatMap(metasOf).toSet
        case VNeutral(_, spine)                => spine.toList.flatMap(metasOf).toSet
        case VPi(domain, codomain)             =>
          val (fresh, _) = freshVar()
          metasOf(domain) ++ metasOf(codomain(fresh))
        case VLam(_, closure)                  =>
          val (fresh, _) = freshVar()
          metasOf(closure(fresh))
        case VConst(_) | VType | VNative(_, _) => Set.empty
      }
  }

  /** Occurs-check: does the metavariable `id` appear anywhere in `value` (under spines and binders)? Forces through
    * already-solved metas, so an *indirect* cycle — `?id := … ?other …` where `?other` is itself solved to something
    * mentioning `?id` — is also caught. This is the soundness gate of precondition #2 (no inferred infinite type): the
    * caller refuses to solve `?id := value` when this holds, so `Evaluator.force` can never enter a cyclic meta.
    *
    * Distinct from [[metasOf]], which deliberately does *not* follow solutions (it taints by id, on purpose). Forcing
    * here is safe because `id` is unsolved at every call site, and the store holds no cycle (this very check maintains
    * that invariant).
    */
  private def occursIn(id: MetaId, value: SemValue): Boolean =
    Evaluator.force(value, metaStore) match {
      case VMeta(other, spine)    => other.value == id.value || spine.toList.exists(occursIn(id, _))
      case VTopDef(_, _, spine)   => spine.toList.exists(occursIn(id, _))
      case VStuckNative(_, spine) => spine.toList.exists(occursIn(id, _))
      case VNeutral(_, spine)     => spine.toList.exists(occursIn(id, _))
      case VPi(domain, codomain)  =>
        val (fresh, _) = freshVar()
        occursIn(id, domain) || occursIn(id, codomain(fresh))
      case VLam(_, closure)       =>
        val (fresh, _) = freshVar()
        occursIn(id, closure(fresh))
      case VNative(paramType, _)  => occursIn(id, paramType)
      case VConst(_) | VType      => false
    }

  /** Record an "infinite type" rejection from the occurs-check, reusing the source position of `context`. */
  private[monomorphize] def addOccursError(context: Sourced[String]): Unifier =
    copy(errors = UnifyError(Sourced(context.uri, context.range, "Cannot construct infinite type."), None, None) :: errors)

  private def freshVar(): (SemValue, Unifier) = {
    val v = VNeutral(NeutralHead.Fresh(NeutralHead.Origin.Unify, depth), Spine.SNil)
    (v, copy(depth = depth + 1))
  }

  /** Record a type-mismatch error. By convention the left value is the actual (inferred) type and the right is the
    * expected type, matching the argument order of [[unify]] as called by the checker.
    */
  private[monomorphize] def addMismatch(actual: SemValue, expected: SemValue, context: Sourced[String]): Unifier =
    copy(errors = UnifyError(context, Some(expected), Some(actual)) :: errors)

  private[monomorphize] def addError(context: Sourced[String]): Unifier =
    copy(errors = UnifyError(context, None, None) :: errors)

  /** Structural equality for ground values. */
  private def groundEquals(g1: GroundValue, g2: GroundValue): Boolean = (g1, g2) match {
    case (GroundValue.Type, GroundValue.Type)                                   => true
    case (GroundValue.Direct(v1, t1), GroundValue.Direct(v2, t2))               => v1 == v2 && groundEquals(t1, t2)
    case (GroundValue.Structure(n1, a1, t1), GroundValue.Structure(n2, a2, t2)) =>
      n1 == n2 &&
      a1.length == a2.length &&
      groundEquals(t1, t2) &&
      a1.zip(a2).forall { case (l, r) => groundEquals(l, r) }
    case _                                                                      => false
  }
}

object Unifier {
  def create(metaStore: MetaStore, depth: Int): Unifier =
    Unifier(metaStore, depth, Nil, Nil)
}
