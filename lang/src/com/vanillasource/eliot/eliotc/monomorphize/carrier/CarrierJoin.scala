package com.vanillasource.eliot.eliotc.monomorphize.carrier

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.unify.Unifier
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The **join-based carrier solver** (docs/effects-as-channel.md §3), the load-bearing content of the uniform-carrier
  * foundation ("the #1 U2 spike item"). A carrier metavariable is **never** solved by whichever term touches it first —
  * that first-contact commitment is the historical premature-`Id`-commitment bug class (the `catch`-handler and
  * `if(c, None) else Some(x)` failures). Instead carrier metas solve by *join* with `Id` as the lattice bottom:
  *
  *   - `Id` ([[Carrier.Bottom]]) **never solves** a carrier meta — a pure term meeting a flex carrier slot records
  *     nothing and the meta stays liftable;
  *   - a meta touched by exactly one non-`Id` carrier solves to it;
  *   - two *different* non-`Id` carriers meeting one meta are a genuine mismatch (one ambient per signature; inner
  *     discharge stacks are distinct carriers by construction);
  *   - a meta untouched by any non-`Id` carrier at the value's boundary defaults to `Id` ([[finalize]]).
  *
  * This promotes [[com.vanillasource.eliot.eliotc.monomorphize.check.EffectLifter.tryIdDefault]] from a fragile ladder
  * *arm* into the solving *rule itself*, so `if(c, "a") else readLine` works order-independently.
  *
  * =Where the carriers live=
  *
  * Carrier metas are ordinary [[SemValue.MetaId]]s in the shared [[com.vanillasource.eliot.eliotc.monomorphize.domain.MetaStore]]
  * (flagged in [[Unifier.carrierRoles]] once wired in), the guarantee "a carrier meta can never be stolen by a
  * container" being recovered by *routing carrier metas exclusively through this join channel*: the
  * [[UniformLadder]] always [[Carrier.split]]s the carrier head off before any payload unification, so ordinary
  * [[Unifier.unify]] only ever sees payloads. A join writes the *unapplied* carrier constructor
  * ([[Carrier.toSemValue]]) into the store; [[resolve]] follows the store (and normalises `Id` to [[Carrier.Bottom]]).
  *
  * All operations return a new [[Unifier]] (the object is stateless — the state is the threaded unifier), exactly as
  * the rest of the unification machinery does.
  */
object CarrierJoin {

  /** Follow a carrier through the unifier's meta store to its representative: [[Carrier.Bottom]], a [[Carrier.Con]], or
    * a still-unsolved [[Carrier.Var]]. A meta solved to `Id` resolves to [[Carrier.Bottom]] — `Id` is the lattice
    * bottom, never a concrete solution — which is exactly what stops a pure arm from being treated as a real
    * contribution.
    */
  def resolve(unifier: Unifier, carrier: Carrier): Carrier = carrier match {
    case Carrier.Var(id) =>
      unifier.metaStore.lookup(id) match {
        case Some(_) => resolve(unifier, Carrier.ofHead(Evaluator.force(VMeta(id, Spine.SNil), unifier.metaStore)))
        case None    => Carrier.Var(id)
      }
    case other           => other
  }

  /** Join `contribution` into the carrier meta `metaId` (the load-bearing rule above). Not first-contact unification:
    * a [[Carrier.Bottom]] contribution records nothing, so the pure arm of a conditional never commits the meta before
    * an effectful sibling contributes.
    */
  def join(unifier: Unifier, metaId: MetaId, contribution: Carrier, context: Sourced[String]): Unifier =
    resolve(unifier, Carrier.Var(metaId)) match {
      case Carrier.Var(rep) =>
        resolve(unifier, contribution) match {
          case Carrier.Bottom                 => unifier
          // Self-join: the contribution resolves to the *same* representative meta (a carrier joined toward itself — a
          // value's own ambient carrier meeting its declared return, `?F[Unit]` against `?F[Unit]`). Solving `rep := rep`
          // would write a self-referential cycle into the store (`resolve` then loops forever); it is simply a no-op.
          case Carrier.Var(cid) if cid == rep => unifier
          case resolved                       => solve(unifier, rep, resolved)
        }
      case rep @ Carrier.Con(a, _) =>
        resolve(unifier, contribution) match {
          case Carrier.Bottom              => unifier
          case Carrier.Con(b, _) if b == a => unifier
          case Carrier.Con(_, _)           => conflict(unifier, context)
          case Carrier.Var(vid)            => solve(unifier, vid, rep) // keep the resolved stack (prefix included)
        }
      case Carrier.Bottom   => unifier
    }

  /** Feed a carrier contribution `actual` toward an expected carrier `expected` — the carrier half of the ladder's
    * pass-join arm ([[UniformLadder]]). When the expected is itself a carrier meta, `actual` joins into it; when the
    * expected is concrete, the *actual*'s carrier meta (if any) joins toward it; two concretes are compared. Pure join,
    * never first-contact.
    */
  def joinToward(unifier: Unifier, actual: Carrier, expected: Carrier, context: Sourced[String]): Unifier =
    (resolve(unifier, actual), resolve(unifier, expected)) match {
      case (contribution, Carrier.Var(gid))               => join(unifier, gid, contribution, context)
      case (Carrier.Var(aid), concrete)                   => join(unifier, aid, concrete, context)
      case (Carrier.Bottom, _)                            => unifier
      case (Carrier.Con(a, _), Carrier.Con(b, _)) if a == b => unifier
      case (Carrier.Con(_, _), Carrier.Con(_, _))         => conflict(unifier, context)
      case (_, Carrier.Bottom)                            => unifier
    }

  /** Default every carrier meta untouched by a non-`Id` contribution to `Id` — the boundary rule. After this, an
    * unsolved carrier meta resolves to [[Carrier.Bottom]] (so a deferred pure-lift over it materialises to nothing).
    */
  def finalize(unifier: Unifier, carrierMetaIds: Iterable[MetaId]): Unifier =
    carrierMetaIds.foldLeft(unifier) { (u, id) =>
      resolve(u, Carrier.Var(id)) match {
        case Carrier.Var(rep) => solve(u, rep, Carrier.Bottom)
        case _                => u
      }
    }

  private def solve(unifier: Unifier, id: MetaId, carrier: Carrier): Unifier =
    unifier.copy(metaStore = unifier.metaStore.solve(id, Carrier.toSemValue(carrier)))

  private def conflict(unifier: Unifier, context: Sourced[String]): Unifier =
    unifier.addError(context.as("Conflicting effect carriers."))
}
