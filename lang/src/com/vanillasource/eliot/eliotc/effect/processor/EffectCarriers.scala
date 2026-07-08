package com.vanillasource.eliot.eliotc.effect.processor

import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.{
  ParameterReference,
  SignatureView,
  asArrow,
  spine
}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue.ResolvedAbilityConstraint
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN

/** Identifying effect carriers. An effect carrier is a value's higher-kinded generic binder (`F` in `[F[_] ~ E...]`,
  * the shape the M1 `{E...}` sugar produces) that an effectful result rides in (`F[String]`, `IO[Unit]`). The
  * predicates here are the structural tests the rest of the pass uses to find carriers, to read off the effects they
  * declare, and to decide whether a type is effectful (carrier-headed).
  */
object EffectCarriers {

  /** A generic binder is a carrier iff its kind is an arrow (`Type -> Type`, i.e. higher-kinded). */
  def isHktBinder(binder: SignatureView.Binder): Boolean =
    binder.parameterType.exists(pt => asArrow(pt.value.signature).isDefined)

  /** The higher-kinded binder names of a signature — the binders an effectful result can ride (`F` in `F[Unit]`). This
    * is the *callee* notion of a carrier: an ability method's return rides its ability's binder (`printLine : F[Unit]`)
    * even though that binder carries no constraint on the method itself, so no constraint is required here. A *value's
    * own* ambient effect carriers are the further-filtered subset whose binder is ability-constrained — see the
    * `paramConstraints` filter in [[EffectCheckProcessor]], which excludes a bare generic `C[_, _]`.
    */
  def carrierBinders(view: SignatureView): Set[String] =
    view.binders.filter(isHktBinder).map(_.name.value).toSet

  /** The user-facing effects a value *declares*: the ability FQNs constrained on its `carriers`, with the internal
    * machinery abilities (`Effect`/`Suspend`) removed — those are inserted by the compiler, never declared as
    * effects. This is both a callee's propagated effect set and the declared set the subset check honours.
    */
  def declaredEffects(
      carriers: Set[String],
      paramConstraints: Map[String, Seq[ResolvedAbilityConstraint]]
  ): Set[AbilityFQN] =
    carriers
      .flatMap(c => paramConstraints.getOrElse(c, Seq.empty).map(_.abilityFQN))
      .filterNot(a => EffectMachinery.isMachineryAbility(a.abilityName))

  /** The carrier binder heading a type expression, if it is headed by one of `carrier` (e.g. `F` for `F[String]`).
    * `None` for any other head. This is the effect-provenance primitive: a value parameter whose type is headed by an
    * ability-constrained carrier binder carries that binder's declared effects (discharge-aware accounting, Step 3).
    */
  def carrierHead(tpe: OperatorResolvedExpression, carrier: Set[String]): Option[String] =
    spine(tpe)._1 match {
      case ParameterReference(n) if carrier.contains(n.value) => Some(n.value)
      case _                                                  => None
    }

  /** Whether a type expression is headed by one of `carrier` (a carrier-typed value, e.g. `F[String]`). */
  def carrierHeaded(tpe: OperatorResolvedExpression, carrier: Set[String]): Boolean =
    carrierHead(tpe, carrier).isDefined
}
