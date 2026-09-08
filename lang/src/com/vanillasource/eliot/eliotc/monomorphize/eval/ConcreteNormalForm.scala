package com.vanillasource.eliot.eliotc.monomorphize.eval

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue

/** Concreteness and structural equality of **normal forms** — the compiler's one notion of definitional equality read
  * as a plain predicate, for the natives that must *decide* an equality during evaluation: the `Eq[Type]::equals` leaf
  * and the frame keys of the effect intrinsics ([[com.vanillasource.eliot.eliotc.monomorphize.processor.EffectIntrinsics]]).
  *
  * Pure and side-effect free, mirroring `Unifier.groundEquals`: it never goes through the
  * [[com.vanillasource.eliot.eliotc.monomorphize.unify.Unifier]], whose equality solves metas as a side effect (an
  * equality *test* must not mutate the meta store). It is also **fail-safe**: [[equal]] answers only for two
  * [[isConcrete]] normal forms, so a caller can stay stuck on a not-yet-concrete argument rather than compare a
  * placeholder equal.
  */
object ConcreteNormalForm {

  /** A fully concrete normal form: `Type`, a ground constant, or a body-less constructor applied to concrete arguments.
    * Anything else (a metavariable, a neutral parameter, a lambda/native, a function type) is not concrete.
    */
  def isConcrete(v: SemValue): Boolean = v match {
    case VType                      => true
    case VConst(_)                  => true
    case VTopDef(_, None, spine, _) => spine.toList.forall(isConcrete)
    case _                          => false
  }

  /** Structural equality of two concrete normal forms (assumes both are [[isConcrete]]): same head and all arguments
    * equal, recursing through the spine. Distinct concrete kinds (e.g. `Type` vs a constructor) are unequal.
    */
  def equal(a: SemValue, b: SemValue): Boolean = (a, b) match {
    case (VType, VType)                                       => true
    case (VConst(g1), VConst(g2))                             => groundEquals(g1, g2)
    case (VTopDef(f1, None, s1, _), VTopDef(f2, None, s2, _)) =>
      val l1 = s1.toList
      val l2 = s2.toList
      f1 === f2 && l1.length === l2.length && l1.zip(l2).forall { case (x, y) => equal(x, y) }
    case _                                                    => false
  }

  /** Structural equality for ground values, mirroring `Unifier.groundEquals` — the bottom of [[equal]] for the ground
    * constants carried by a `VConst` (an integer bound, a string, a `Bool`), including their type field.
    */
  private def groundEquals(g1: GroundValue, g2: GroundValue): Boolean = (g1, g2) match {
    case (GroundValue.Type, GroundValue.Type)                                   => true
    case (GroundValue.Direct(v1, t1), GroundValue.Direct(v2, t2))               => v1 == v2 && groundEquals(t1, t2)
    case (GroundValue.Structure(n1, a1, t1), GroundValue.Structure(n2, a2, t2)) =>
      n1 === n2 && a1.length === a2.length && groundEquals(t1, t2) &&
      a1.zip(a2).forall { case (l, r) => groundEquals(l, r) }
    case _                                                                      => false
  }
}
