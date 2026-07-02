package com.vanillasource.eliot.eliotc.effect.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.{
  FunctionLiteral,
  ValueReference,
  applyChain
}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The internal effect machinery the effect auto-lift inserts and recognises. The user never imports or names it: a
  * `flatMap`/`pure`/`map` is referenced here by fully-qualified name (`eliot.effect.Effect`) and monomorphization pins
  * the carrier and erases the whole tower.
  *
  * It plays two roles for the rewrite:
  *   - *construction* — [[pureWrap]] lifts a pure body into the carrier and [[sequence]] binds an effectful action into
  *     a continuation (`Effect.flatMap` when the continuation is itself effectful, `Effect.map` when it is pure);
  *   - *recognition* — [[isMachineryAbility]] tells the rest of the pass that an `Effect`/`Sync` call is compiler
  *     machinery, so it neither counts as a user-facing effect (`effectAbilitiesOf`) nor is auto-bound a second time
  *     (`isAuthorMachineryCall`).
  */
object EffectMachinery {
  private val effectModule = ModuleName(ModuleName.effectPackage, "Effect")

  private val flatMapFQN: ValueFQN = ValueFQN(effectModule, QualifiedName("flatMap", Qualifier.Ability("Effect")))
  private val pureFQN: ValueFQN    = ValueFQN(effectModule, QualifiedName("pure", Qualifier.Ability("Effect")))
  private val mapFQN: ValueFQN     = ValueFQN(effectModule, QualifiedName("map", Qualifier.Ability("Effect")))

  /** The abilities the compiler inserts and recognises but the user never names. */
  private val machineryAbilities: Set[String] = Set("Effect", "Sync")

  /** The internal effect machinery, never a user-facing effect: a `flatMap`/`pure`/`map`/`sync` call (hand-written or
    * inserted by this phase) must not be counted as "using an effect" by the declared-effect check.
    */
  def isMachineryAbility(abilityName: String): Boolean = machineryAbilities.contains(abilityName)

  /** The ability a value reference belongs to, if it is an ability method (`printLine` → `Console`, `flatMap` → `Effect`);
    * `None` for an ordinary (non-ability) value. Lets callers ask "which ability does this call name?" without
    * re-matching on [[Qualifier]].
    */
  def abilityNameOf(fqn: ValueFQN): Option[String] =
    fqn.name.qualifier match {
      case Qualifier.Ability(name) => Some(name)
      case _                       => None
    }

  /** Lift a pure expression into the carrier with `Effect.pure`. */
  def pureWrap(expr: Sourced[OperatorResolvedExpression]): Sourced[OperatorResolvedExpression] =
    expr.as(applyChain(expr.as(ValueReference(expr.as(pureFQN))), Seq(expr)))

  /** Sequence `action` into `continuation`, binding the action's result to `name`. Uses `Effect.flatMap` when the
    * continuation is itself effectful and `Effect.map` when it is pure (lifting the pure continuation into the
    * carrier).
    */
  def sequence(
      action: Sourced[OperatorResolvedExpression],
      name: String,
      continuation: Sourced[OperatorResolvedExpression],
      continuationEffectful: Boolean
  ): Sourced[OperatorResolvedExpression] = {
    val lambda     = action.as(FunctionLiteral(action.as(name), None, continuation))
    val combinator = action.as(ValueReference(action.as(if (continuationEffectful) flatMapFQN else mapFQN)))
    action.as(applyChain(combinator, Seq(action, lambda)))
  }
}
