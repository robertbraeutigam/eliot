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
  */
case class Unifier(
    metaStore: MetaStore,
    depth: Int,
    postponed: List[(SemValue, SemValue, Sourced[String])],
    errors: List[UnifyError]
) {

  /** Unify two semantic values, reporting errors with the given context message and source position. */
  def unify(l: SemValue, r: SemValue, context: Sourced[String]): Unifier = {
    val fl = Evaluator.force(l, metaStore)
    val fr = Evaluator.force(r, metaStore)
    (fl, fr) match {
      case (VType, VType) => this

      case (VConst(g1), VConst(g2)) =>
        if (!groundEquals(g1, g2)) addMismatch(fl, fr, context)
        else this

      case (VPi(d1, c1), VPi(d2, c2)) =>
        val (fresh, u1) = freshVar()
        u1.unify(d1, d2, context).unify(c1(fresh), c2(fresh), context)

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

      // VTopDef equality by FQN
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
        // Already solved — unify the solution (with spine applied) against rhs
        val applied = spine.toList.foldLeft(solved)(Evaluator.applyValue)
        unify(applied, rhs, context)
      case None         =>
        rhs match {
          case VMeta(rhsId, _) if rhsId.value == id.value =>
            this // Same unsolved meta — trivially equal, no solve needed
          case _                                          =>
            val spineList = spine.toList
            if (spineList.isEmpty) {
              // Empty spine — solve directly
              copy(metaStore = metaStore.solve(id, rhs))
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
