package com.vanillasource.eliot.eliotc.effect.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.{
  ParameterReference,
  SignatureView,
  ValueReference,
  isFunctionReference,
  spine
}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Reads a callee's operator-resolved signature into the [[CalleeSignatures.CalleeInfo]] the body auto-lift needs: its
  * value-parameter types, its return type, the names of its higher-kinded (carrier) generic binders, and the
  * user-facing effects it performs. An unresolvable callee is treated as a zero-parameter pure value (conservative;
  * monomorphization remains the backstop).
  */
class CalleeSignatures {
  import CalleeSignatures.*

  def infoFor(fqn: Sourced[ValueFQN], platform: Platform): CompilerIO[CalleeInfo] =
    getFact(OperatorResolvedValue.Key(fqn.value, platform)).map {
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
    * The internal machinery abilities (`Effect`/`Sync`) are excluded — they are inserted by the compiler
    * and never named by users, so a hand-written or auto-inserted `flatMap`/`pure`/`sync` does not pollute the set.
    */
  private def effectAbilitiesOf(
      fqn: ValueFQN,
      orv: OperatorResolvedValue,
      carrierBinders: Set[String]
  ): Set[AbilityFQN] =
    EffectMachinery.abilityNameOf(fqn) match {
      // An ability method performs its owning ability — unless it is internal machinery (Effect/Sync).
      case Some(abilityName) =>
        if (EffectMachinery.isMachineryAbility(abilityName)) Set.empty
        else Set(AbilityFQN(fqn.moduleName, abilityName))
      // An ordinary `{E...}` function propagates the (non-machinery) effects declared on its own carrier binder(s).
      case None              => EffectCarriers.declaredEffects(carrierBinders, orv.paramConstraints)
    }
}

object CalleeSignatures {

  /** A callee's signature reduced to what the auto-lift reads, plus the user-facing effects it performs. */
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

    /** Whether argument position `pos` auto-binds an effectful argument. It does iff the callee's parameter there is a
      * *pure value* position: an **un-applied** type (`args.isEmpty`) that is neither function-typed nor one of the
      * callee's own higher-kinded carrier binders — i.e. a concrete scalar (`String`, `Unit`) or a bare type variable
      * (`second : A`).
      *
      * Everything else is a *storage* position that takes the effectful action directly, unbound: a function-typed
      * parameter (a continuation), and any *applied* type constructor — whether it mentions a carrier (`fa : F[A]`,
      * `p : AbortCarrier[G, A]`, a discharge/carrier slot) or not (`x : Id[A]`). So `andThen(printLine(..), abort)` binds
      * `abort` into the bare `A`, but `runId(runAbort(p))` passes the carrier value into `Id[A]` unbound.
      */
    def isBindPosition(pos: Int): Boolean =
      valueParamTypes.lift(pos).exists { paramType =>
        val (head, args) = spine(paramType)
        args.isEmpty && (head match {
          case ref: ValueReference   => !isFunctionReference(ref)
          case ParameterReference(n) => !carrierBinders.contains(n.value)
          case _                     => true
        })
      }

    /** Whether argument position `pos` is an *eliminator branch* — a value parameter whose type is (structurally) the
      * callee's own return type, e.g. `ifNone: B` / `whenTrue: A` of `foldOption[A,B](.., B, ..): B` / `fold[A](..,
      * A, A): A`. Such a position does not *consume* a value; it *produces* the result, so it carries whatever the
      * result carries. When the result is instantiated to a carrier (a guard's `{Throw[String]}` return), an effectful
      * argument here is a *branch value* (the chosen outcome), not an action to sequence — so [[buildArguments]]
      * leaves it unbound and marks the eliminator call effectful instead. Recognised purely structurally (both the
      * parameter and the return are the *same* bare generic binder), so it fires only for genuine eliminators and never
      * for a concrete-typed value parameter.
      */
    def isBranchPosition(pos: Int): Boolean =
      valueParamTypes.lift(pos).exists { paramType =>
        (spine(paramType), spine(returnType)) match {
          case ((ParameterReference(pn), pArgs), (ParameterReference(rn), rArgs)) =>
            pArgs.isEmpty && rArgs.isEmpty && pn.value === rn.value
          case _                                                                  => false
        }
      }
  }
}
