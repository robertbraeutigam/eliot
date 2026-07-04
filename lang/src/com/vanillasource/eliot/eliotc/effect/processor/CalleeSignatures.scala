package com.vanillasource.eliot.eliotc.effect.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.{SignatureView, ValueReference}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Reads a callee's operator-resolved signature into the [[CalleeSignatures.CalleeInfo]] the effect *accounting* needs
  * ([[EffectUsageCollector]]): its value-parameter types, its return type, the names of its higher-kinded (carrier)
  * generic binders, and the user-facing effects it performs. An unresolvable callee is treated as a zero-parameter
  * pure value (conservative; monomorphization remains the backstop). The former bind/branch-position half (which
  * argument slots sequence an effectful argument) is gone — that decision is type-directed elaboration in the NbE
  * checker ([[com.vanillasource.eliot.eliotc.monomorphize.check.EffectLifter]]).
  */
class CalleeSignatures {
  import CalleeSignatures.*

  def infoFor(fqn: Sourced[ValueFQN], platform: Platform): CompilerIO[CalleeInfo] =
    getFactIfProduced(OperatorResolvedValue.Key(fqn.value, platform)).map {
      case Some(orv) =>
        val view           = SignatureView.of(orv.typeStack.as(orv.typeStack.value.signature))
        val carrierBinders = EffectCarriers.carrierBinders(view)
        CalleeInfo(
          view.parameters.map(_.value),
          view.returnType.value,
          carrierBinders,
          effectAbilitiesOf(fqn.value, orv, carrierBinders)
        )
      case None      =>
        CalleeInfo(Seq.empty, ValueReference(fqn.as(WellKnownTypes.typeFQN)), Set.empty, Set.empty)
    }

  /** The user-facing effects performing this callee contributes to the *caller's* effect set (Decision 6, propagation):
    *   - an ability method (`printLine`, `log`, `get`) performs its *owning* ability (read off the FQN's
    *     [[Qualifier.Ability]]), e.g. `Console`/`Log`/`Dep`;
    *   - an ordinary `{E...}` function propagates the effects declared on its own carrier binder(s).
    *
    * The internal machinery abilities (`Effect`/`Suspend`) are excluded — they are inserted by the compiler
    * and never named by users, so a hand-written or auto-inserted `flatMap`/`pure`/`suspend` does not pollute the set.
    */
  private def effectAbilitiesOf(
      fqn: ValueFQN,
      orv: OperatorResolvedValue,
      carrierBinders: Set[String]
  ): Set[AbilityFQN] =
    EffectMachinery.abilityNameOf(fqn) match {
      // An ability method performs its owning ability — unless it is internal machinery (Effect/Suspend).
      case Some(abilityName) =>
        if (EffectMachinery.isMachineryAbility(abilityName)) Set.empty
        else Set(AbilityFQN(fqn.moduleName, abilityName))
      // An ordinary `{E...}` function propagates the (non-machinery) effects declared on its own carrier binder(s).
      case None              => EffectCarriers.declaredEffects(carrierBinders, orv.paramConstraints)
    }
}

object CalleeSignatures {

  /** A callee's signature reduced to what the effect accounting reads, plus the user-facing effects it performs. */
  case class CalleeInfo(
      valueParamTypes: Seq[OperatorResolvedExpression],
      returnType: OperatorResolvedExpression,
      carrierBinders: Set[String],
      effectAbilities: Set[AbilityFQN]
  ) {

    /** Whether this callee applied to `appliedCount` value arguments yields an effectful result: it must be fully
      * applied and its result type headed by one of the callee's own carrier binders.
      */
    def resultEffectful(appliedCount: Int): Boolean =
      appliedCount >= valueParamTypes.size && EffectCarriers.carrierHeaded(returnType, carrierBinders)
  }
}
