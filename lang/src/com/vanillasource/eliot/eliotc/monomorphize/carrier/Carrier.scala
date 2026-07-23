package com.vanillasource.eliot.eliotc.monomorphize.carrier

import com.vanillasource.eliot.eliotc.module.fact.{ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*

/** The **uniform-carrier** value of a runtime term judgment (docs/effects-as-channel.md §2–§3), productionised from the
  * U2 foundation spike ([[com.vanillasource.eliot.eliotc.monomorphize.spike.UniformCarrierSpike]]) onto the real
  * [[SemValue]] domain. This is the U3a foundation the checker wiring consumes; it is **not yet wired into the
  * [[com.vanillasource.eliot.eliotc.monomorphize.check.Checker]]**, so the default compiler path stays byte-identical.
  *
  * Under uniform carriers every runtime term's checked type is carrier-headed with the carrier *outermost by
  * construction* — `Id[String]` for a pure string, `IO[String]` for `readLine`, `Id[List[String]]` for a pure list.
  * Carrierhood is therefore *positional*, never a "is this type a carrier?" query on an arbitrary type — the endgame the
  * [[com.vanillasource.eliot.eliotc.monomorphize.check.EffectLifter]] treadmill could not win. A [[Carrier]] is one of:
  *
  *   - [[Carrier.Bottom]] — the pure carrier `Id` ([[WellKnownTypes.idFQN]]), the **lattice bottom** *everywhere*
  *     (never a concrete carrier value — collapsing this is what keeps a pure arm of a conditional from committing an
  *     ambient meta; see [[CarrierJoin]]);
  *   - [[Carrier.Con]] — a concrete carrier constructor (`IO`, `StateCarrier[S, G]`, …), keeping its leading stack
  *     *prefix* (`StateCarrier[S, G]` keeps `[S, G]`) so a multi-layer discharge stack round-trips; two `Con`s are the
  *     *same* carrier iff their [[ValueFQN]]s match (the prefix, like the spike's, does not enter the join comparison);
  *   - [[Carrier.Var]] — an unsolved carrier metavariable (a callee's carrier binder, the value's ambient), still
  *     bottom until [[CarrierJoin.join]]ed.
  */
sealed trait Carrier

object Carrier {

  /** The pure carrier `Id` — the lattice bottom. A judgment headed by [[WellKnownTypes.idFQN]], and any carrier meta
    * solved to it, normalises to this; it never solves another carrier meta (join treats it as "no contribution").
    */
  case object Bottom extends Carrier

  /** A concrete effect carrier constructor applied to its leading stack `prefix` (empty for a flat carrier like `IO`,
    * `[S, G]` for `StateCarrier[S, G]`). Carrier identity for the join lattice is the [[fqn]] alone.
    */
  case class Con(fqn: ValueFQN, prefix: List[SemValue]) extends Carrier

  /** An unsolved carrier metavariable — bottom until a non-`Id` contribution joins into it. */
  case class Var(id: MetaId) extends Carrier

  /** Split a carrier-headed runtime judgment `C[T]` into its carrier and payload — **positional and total**, because
    * every runtime term is carrier-headed by construction (the elaboration invariant). A multi-argument carrier stack
    * (`StateCarrier[S, G, A]`) keeps the leading prefix as the carrier and the *last* argument as the payload, exactly
    * as [[com.vanillasource.eliot.eliotc.monomorphize.check.EffectLifter.effectCarrierSplit]] does today.
    *
    * Unlike the spike's `split` this needs **no** carrier-constructor set: it does not ask "is this head a carrier?",
    * it peels whatever outermost head elaboration put there — `Id` becomes [[Bottom]], any other head becomes its
    * carrier. A bare head (no spine) or a type-of-types [[VType]] is *not* a runtime judgment; throwing there encodes
    * the invariant (a violation is an elaboration bug, never a shape to recognise).
    */
  def split(judgment: SemValue): (Carrier, SemValue) = judgment match {
    case VTopDef(fqn, _, Spine.SApp(prefix, payload)) => (ofConstructor(fqn, prefix.toList), payload)
    case VMeta(id, Spine.SApp(_, payload))            => (Var(id), payload)
    case other                                        =>
      throw new IllegalArgumentException(s"not a carrier-headed runtime judgment: $other")
  }

  /** Interpret a bare *carrier-position* value (a carrier meta's stored solution — an unapplied carrier constructor or
    * another meta) as a [[Carrier]]. Unlike [[split]] there is no payload: the value *is* the carrier head.
    */
  def ofHead(carrierValue: SemValue): Carrier = carrierValue match {
    case VTopDef(fqn, _, spine) => ofConstructor(fqn, spine.toList)
    case VMeta(id, _)           => Var(id)
    case other                  => throw new IllegalArgumentException(s"not a carrier head: $other")
  }

  private def ofConstructor(fqn: ValueFQN, prefix: List[SemValue]): Carrier =
    if (fqn == WellKnownTypes.idFQN) Bottom else Con(fqn, prefix)

  /** Reify a [[Carrier]] back to the *unapplied* carrier-constructor [[SemValue]] stored for a solved carrier meta:
    * [[Bottom]] ⤳ unapplied `Id`, [[Con]] ⤳ its constructor applied to its stack prefix (not the payload), [[Var]] ⤳
    * the bare meta. Inverse of [[ofHead]].
    */
  def toSemValue(carrier: Carrier): SemValue = carrier match {
    case Bottom            => VTopDef(WellKnownTypes.idFQN, None, Spine.SNil)
    case Con(fqn, prefix)  => VTopDef(fqn, None, prefix.foldLeft(Spine.SNil: Spine)(_ :+ _))
    case Var(id)           => VMeta(id, Spine.SNil)
  }
}
