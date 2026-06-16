package com.vanillasource.eliot.eliotc.monomorphize.unify

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
  * `combinable`/`candidates` support the Phase 4 `Combine` ability without breaking the purity above. A metavariable
  * that sits in a covariant position (a `match` result, a result-position type parameter) is registered as
  * `combinable`; when a *second, definitionally-unequal* type is unified against such a meta, instead of failing the
  * unification (first-candidate-wins) we accumulate it as a candidate constraint and leave the meta solved to its first
  * candidate. The checker later resolves these candidates to their `Combine` join at drain time
  * ([[com.vanillasource.eliot.eliotc.monomorphize.check.Checker.resolveCombines]]). A meta is *un*-registered (tainted)
  * the moment it appears in a [[VPi]] domain — a contravariant position where joining would be unsound. When no
  * `Combine` instance applies (or the meta was tainted), the checker re-unifies the candidates strictly, surfacing the
  * ordinary mismatch — so unification stays first-candidate-wins for everything that has no `Combine` instance.
  *
  * @param combinable
  *   Raw ids of metavariables eligible for `Combine`-based multi-candidate resolution (covariant positions only).
  * @param candidates
  *   For each combinable meta, the types unified against it (with their source contexts), in arrival order.
  */
case class Unifier(
    metaStore: MetaStore,
    depth: Int,
    postponed: List[(SemValue, SemValue, Sourced[String])],
    errors: List[UnifyError],
    combinable: Set[Int] = Set.empty,
    candidates: Map[Int, List[(SemValue, Sourced[String])]] = Map.empty
) {

  /** Register a metavariable as combinable (covariant). Called by the checker when it allocates an implicit
    * type-parameter instantiation meta (see [[com.vanillasource.eliot.eliotc.monomorphize.check.Checker.peelLams]]).
    */
  def markCombinable(id: MetaId): Unifier = copy(combinable = combinable + id.value)

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
    case VMeta(id, Spine.SNil) if combinable.contains(id.value) && metaStore.lookup(id).nonEmpty =>
      val solution = metaStore.lookup(id).get
      val trial    = unifyForced(l, solution, context)
      if (trial.errors.size == errors.size) trial
      else recordCandidate(id, l, context)
    case _                                                                                       =>
      unifyForced(l, r, context)
  }

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
              // Empty spine — solve directly. For a combinable meta also record the first candidate, so the checker's
              // combine resolution sees the full candidate set.
              val solvedU = copy(metaStore = metaStore.solve(id, rhs))
              if (combinable.contains(id.value)) solvedU.recordCandidate(id, rhs, context) else solvedU
            } else {
              // Non-empty spine — try injectivity decomposition first, otherwise postpone.
              tryDecomposeApplied(id, spineList, rhs, context)
                .getOrElse(copy(postponed = (VMeta(id, spine), rhs, context) :: postponed))
            }
        }
    }

  /** Injectivity-based decomposition of `?id s1..sn ~ H r1..rn`, mirroring what GHC and Scala 3 do for applied types:
    * type constructor application is treated as injective, so the decomposition into `?id := H` (unapplied) plus
    * pointwise spine unification is unique.
    *
    * Fires only when the rhs's head is rigid and the arities match:
    *   - [[VTopDef]] with no cached body — data-type constructors are bound this way and never β-reduce. Cached
    *     VTopDefs (user-defined type aliases) would have been unfolded by the ambient [[Evaluator.force]] before
    *     reaching here, so we should not see them; the `None` guard is a belt-and-braces check.
    *   - [[VNeutral]] — rigid bound variables with a spine.
    *
    * Arity mismatches and non-rigid rhs shapes return `None`, leaving the caller to postpone. Cases like `?A [?B] ~
    * Function [Int, String]` have multiple valid solutions and correctly postpone.
    */
  private def tryDecomposeApplied(
      id: MetaId,
      metaSpine: List[SemValue],
      rhs: SemValue,
      context: Sourced[String]
  ): Option[Unifier] = rhs match {
    case VTopDef(fqn, None, rhsSpine) =>
      decomposeSpines(id, metaSpine, rhsSpine, VTopDef(fqn, None, Spine.SNil), context)
    case VNeutral(head, rhsSpine)     =>
      decomposeSpines(id, metaSpine, rhsSpine, VNeutral(head, Spine.SNil), context)
    case _                            => None
  }

  private def decomposeSpines(
      id: MetaId,
      metaSpine: List[SemValue],
      rhsSpine: Spine,
      bareHead: SemValue,
      context: Sourced[String]
  ): Option[Unifier] = {
    val rhsList = rhsSpine.toList
    if (rhsList.length != metaSpine.length) None
    else {
      val solvedMeta = copy(metaStore = metaStore.solve(id, bareHead))
      Some(metaSpine.zip(rhsList).foldLeft(solvedMeta) { case (u, (l, r)) => u.unify(l, r, context) })
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
            val beforeErrors    = acc.errors.length
            val beforePostponed = acc.postponed.length
            val after           = acc.unify(l, r, ctx)
            if (after.errors.length > beforeErrors) {
              // New error — remove it and re-postpone
              (
                after.copy(
                  errors = after.errors.drop(after.errors.length - beforeErrors),
                  postponed = (l, r, ctx) :: after.postponed
                ),
                anyChanged
              )
            } else if (after.postponed.length > beforePostponed) {
              // Constraint was re-postponed inside unify (e.g., non-pattern spine) — no real progress
              (after, anyChanged)
            } else {
              (after, true)
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

  /** Append a candidate type (with its source context) to a combinable meta's accumulated constraints. */
  private def recordCandidate(id: MetaId, rhs: SemValue, context: Sourced[String]): Unifier =
    copy(candidates = candidates.updated(id.value, candidates.getOrElse(id.value, Nil) :+ (rhs, context)))

  /** Remove every metavariable occurring in `sv` from the `combinable` set (taint). Used on [[VPi]] domains. */
  private def taintMetasIn(sv: SemValue): Unifier =
    copy(combinable = combinable -- metasOf(sv))

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

  private def freshVar(): (SemValue, Unifier) = {
    val v = VNeutral(NeutralHead.VVar(depth, s"$$unify$depth"), Spine.SNil)
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
