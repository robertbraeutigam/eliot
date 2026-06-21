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

/** The internal monadic machinery the effect auto-lift inserts and recognises. The user never imports or names it: a
  * `flatMap`/`pure`/`map` is referenced here by fully-qualified name (`eliot.lang.Monad`/`eliot.lang.Applicative`) and
  * monomorphization pins the carrier and erases the whole tower.
  *
  * It plays two roles for the rewrite:
  *   - *construction* — [[pureWrap]] lifts a pure body into the carrier and [[sequence]] binds an effectful action into
  *     a continuation (`Monad.flatMap` when the continuation is itself effectful, `Applicative.map` when it is pure);
  *   - *recognition* — [[isMachineryAbility]] tells the rest of the pass that a `Monad`/`Applicative`/`Sync` call is
  *     compiler machinery, so it neither counts as a user-facing effect (`effectAbilitiesOf`) nor is auto-bound a
  *     second time (`isAuthorMachineryCall`).
  */
object EffectMachinery {
  private val monadModule       = ModuleName(ModuleName.defaultSystemPackage, "Monad")
  private val applicativeModule = ModuleName(ModuleName.defaultSystemPackage, "Applicative")

  private val flatMapFQN: ValueFQN = ValueFQN(monadModule, QualifiedName("flatMap", Qualifier.Ability("Monad")))
  private val pureFQN: ValueFQN    = ValueFQN(monadModule, QualifiedName("pure", Qualifier.Ability("Monad")))
  private val mapFQN: ValueFQN     = ValueFQN(applicativeModule, QualifiedName("map", Qualifier.Ability("Applicative")))

  /** The internal effect machinery, never a user-facing effect: a `flatMap`/`pure`/`map`/`sync` call (hand-written or
    * inserted by this phase) must not be counted as "using an effect" by the declared-effect check.
    */
  def isMachineryAbility(abilityName: String): Boolean =
    abilityName == "Monad" || abilityName == "Applicative" || abilityName == "Sync"

  /** Lift a pure expression into the carrier with `Monad.pure`. */
  def pureWrap(expr: Sourced[OperatorResolvedExpression]): Sourced[OperatorResolvedExpression] =
    expr.as(applyChain(expr.as(ValueReference(expr.as(pureFQN))), Seq(expr)))

  /** Sequence `action` into `continuation`, binding the action's result to `name`. Uses `Monad.flatMap` when the
    * continuation is itself effectful and `Applicative.map` when it is pure (lifting the pure continuation into the
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
