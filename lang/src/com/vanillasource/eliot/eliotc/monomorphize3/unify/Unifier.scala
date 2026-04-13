package com.vanillasource.eliot.eliotc.monomorphize3.unify

import com.vanillasource.eliot.eliotc.monomorphize3.domain.*
import com.vanillasource.eliot.eliotc.monomorphize3.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize3.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize3.fact.GroundValue
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Pattern unification on SemValues with a postponement queue. Handles structural unification, meta solving, and
  * eta-expansion.
  *
  * @param metaStore
  *   The current meta store (mutable state passed through)
  * @param depth
  *   The current binding depth (for fresh variable generation)
  * @param postponed
  *   Queue of postponed unification problems (when a meta's spine is not pattern)
  * @param errors
  *   Accumulated error messages with source positions
  */
class Unifier(
    var metaStore: MetaStore,
    var depth: Int,
    var postponed: List[(SemValue, SemValue, Sourced[String])],
    var errors: List[Sourced[String]]
) {

  /** Unify two semantic values, reporting errors with the given context message and source position. */
  def unify(l: SemValue, r: SemValue, context: Sourced[String]): Unit = {
    val fl = Evaluator.force(l, metaStore)
    val fr = Evaluator.force(r, metaStore)
    (fl, fr) match {
      case (VType, VType) => ()

      case (VConst(g1), VConst(g2)) =>
        if (!groundEquals(g1, g2)) {
          errors = context :: errors
        }

      case (VPi(d1, c1), VPi(d2, c2)) =>
        unify(d1, d2, context)
        val fresh = freshVar()
        unify(c1(fresh), c2(fresh), context)

      case (VLam(_, c1), VLam(_, c2)) =>
        val fresh = freshVar()
        unify(c1(fresh), c2(fresh), context)

      // Eta: VLam vs non-lambda
      case (VLam(_, c), other)        =>
        val fresh = freshVar()
        unify(c(fresh), Evaluator.applyValue(other, fresh), context)

      case (other, VLam(_, c))                                      =>
        val fresh = freshVar()
        unify(Evaluator.applyValue(other, fresh), c(fresh), context)

      // Neutral-neutral: same head, same spine length
      case (VNeutral(h1, sp1, _), VNeutral(h2, sp2, _)) if h1 == h2 =>
        val l1 = sp1.toList
        val l2 = sp2.toList
        if (l1.length == l2.length) {
          l1.zip(l2).foreach { (a, b) => unify(a, b, context) }
        } else {
          errors = context :: errors
        }

      // Meta solving (pattern rule)
      case (VMeta(id, spine, _), rhs)                               =>
        solveMeta(id, spine, rhs, context)

      case (lhs, VMeta(id, spine, _))                                     =>
        solveMeta(id, spine, lhs, context)

      // VTopDef equality by FQN
      case (VTopDef(fqn1, _, sp1), VTopDef(fqn2, _, sp2)) if fqn1 == fqn2 =>
        val l1 = sp1.toList
        val l2 = sp2.toList
        if (l1.length == l2.length) {
          l1.zip(l2).foreach { (a, b) => unify(a, b, context) }
        } else {
          errors = context :: errors
        }

      case _ =>
        errors = context :: errors
    }
  }

  /** Try to solve a meta via the pattern rule. If the spine is distinct variables, solve directly. Otherwise postpone.
    */
  private def solveMeta(id: MetaId, spine: Spine, rhs: SemValue, context: Sourced[String]): Unit =
    metaStore.lookup(id) match {
      case Some(solved) =>
        // Already solved — unify the solution (with spine applied) against rhs
        val applied = spine.toList.foldLeft(solved)(Evaluator.applyValue)
        unify(applied, rhs, context)
      case None         =>
        // Unsolved — solve directly (simplified: no occurs check or pattern spine check yet)
        metaStore = metaStore.solve(id, rhs)
    }

  /** Drain the postponement queue, re-attempting postponed unifications. Repeats until stable. */
  def drain(): Unit = {
    var changed = true
    while (changed) {
      changed = false
      val current = postponed
      postponed = Nil
      current.foreach { (l, r, ctx) =>
        val beforeErrors = errors.length
        unify(l, r, ctx)
        if (errors.length > beforeErrors) {
          // New error — remove it and re-postpone
          errors = errors.drop(errors.length - beforeErrors)
          postponed = (l, r, ctx) :: postponed
        } else {
          changed = true
        }
      }
    }
  }

  private def freshVar(): SemValue = {
    val v = VNeutral(NeutralHead.VVar(depth, s"$$unify$depth"), Spine.SNil, VType)
    depth += 1
    v
  }

  /** Structural equality for ground values. */
  private def groundEquals(g1: GroundValue, g2: GroundValue): Boolean = (g1, g2) match {
    case (GroundValue.Type, GroundValue.Type)                           => true
    case (GroundValue.Direct(v1, t1), GroundValue.Direct(v2, t2))       => v1 == v2 && groundEquals(t1, t2)
    case (GroundValue.Structure(f1, t1), GroundValue.Structure(f2, t2)) =>
      f1.keySet == f2.keySet &&
      groundEquals(t1, t2) &&
      f1.keys.forall(k => groundEquals(f1(k), f2(k)))
    case _                                                              => false
  }
}

object Unifier {
  def create(metaStore: MetaStore, depth: Int): Unifier =
    new Unifier(metaStore, depth, Nil, Nil)
}
