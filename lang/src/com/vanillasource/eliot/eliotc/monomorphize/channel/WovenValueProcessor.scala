package com.vanillasource.eliot.eliotc.monomorphize.channel

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementation
import com.vanillasource.eliot.eliotc.effect.processor.EffectMachinery
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicExpression, MonomorphicValue}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The effects-as-channel **weaver** (docs/effects-as-channel.md §6), first slice: under `--effect-channel` with a
  * platform-supplied base carrier, take an effect-blind [[MonomorphicValue]] and produce its woven form
  * ([[WovenValue]]) — the direct-style → monadic elaboration the checker does today, now over concrete post-mono terms.
  *
  * This slice does **carrier assignment + effect-operation resolution** for the Suspend-riding base carrier:
  *   - every abstract *user effect-operation* reference in the body (`Console::printLine`, `Qualifier.Ability`,
  *     machinery excluded — left abstract by the effect-blind desugar/resolver) is resolved to its concrete carrier
  *     instance method at the base carrier, exactly as `PostDrainQuoter.resolveIfAbility` does on the carrier path:
  *     query `AbilityImplementation` at the ability-level carrier argument and emit
  *     `MonomorphicValueReference(implFqn, implTypeArgs)` (e.g. `Console[F~Suspend]::printLine` at `[IO]`);
  *   - an effectful value's signature is wrapped in the carrier (`Unit` ⤳ `IO[Unit]`), so the woven form is the
  *     runnable carrier-headed value the platform entry point expects. The carrier machinery (`Effect`/`Suspend`) and
  *     the effect *instances* keep their carriers (they are not erased), so their carrier-tower bodies monomorphize
  *     normally and need no weaving — only top-level user operations do.
  *
  * Deferred to later slices (documented so the gaps are visible, not silent): `flatMap`/`pure` insertion where an
  * effectful sub-term meets a pure position (a nested effectful argument or a block); precise carrier-headed node types
  * on the woven body (kept as the effect-blind payload types here); control-effect carrier stacks (`weave key = mono
  * key × stack`); and the multi-parameter effect abilities (`State[S, F]`), which this slice's single-carrier-argument
  * query does not yet resolve (they are left abstract rather than mis-resolved).
  *
  * Off the flag, or when no base carrier is configured, the weave is the **identity** image of the `MonomorphicValue`
  * (the carrier path is unchanged).
  */
class WovenValueProcessor(effectChannel: Boolean = false, baseCarrier: Option[ValueFQN] = None)
    extends TransformationProcessor[MonomorphicValue.Key, WovenValue.Key](key =>
      MonomorphicValue.Key(key.vfqn, key.typeArguments)
    )
    with Logging {

  override protected def generateFromKeyAndFact(
      key: WovenValue.Key,
      mv: MonomorphicValue
  ): CompilerIO[WovenValue] =
    baseCarrier.filter(_ => effectChannel) match {
      case Some(carrier) => weave(mv, carrier)
      case None          => WovenValue(mv.vfqn, mv.typeArguments, mv.name, mv.signature, mv.runtime).pure[CompilerIO]
    }

  private def weave(mv: MonomorphicValue, carrier: ValueFQN): CompilerIO[WovenValue] =
    for {
      wovenBody <- mv.runtime.traverse(body => weaveExpression(body.value, carrier).map(body.as))
      effectful <- isEffectful(mv.vfqn)
      signature  = if (effectful) carrierApplied(carrier, mv.signature) else mv.signature
    } yield WovenValue(mv.vfqn, mv.typeArguments, mv.name, signature, wovenBody)

  /** Rewrite one expression: resolve an abstract user effect-operation reference to its concrete carrier instance
    * method at `carrier`, descending through applications and lambdas. Every other node is preserved (its children still
    * descended). An operation that does not resolve at the single carrier argument (a multi-parameter effect ability, a
    * not-yet-instantiated instance) is left abstract — a later slice, never a silent mis-weave.
    */
  private def weaveExpression(
      expr: MonomorphicExpression.Expression,
      carrier: ValueFQN
  ): CompilerIO[MonomorphicExpression.Expression] = expr match {
    case ref @ MonomorphicExpression.MonomorphicValueReference(vfqn, _) if isUserEffectOperation(vfqn.value) =>
      resolveOperation(vfqn.value, carrier).map {
        case Some((implFqn, implTypeArgs)) => MonomorphicExpression.MonomorphicValueReference(vfqn.as(implFqn), implTypeArgs)
        case None                          => ref
      }
    case MonomorphicExpression.FunctionApplication(target, argument)                                        =>
      (weaveNode(target, carrier), weaveNode(argument, carrier)).mapN(MonomorphicExpression.FunctionApplication.apply)
    case MonomorphicExpression.FunctionLiteral(name, parameterType, body)                                   =>
      weaveNode(body, carrier).map(MonomorphicExpression.FunctionLiteral(name, parameterType, _))
    case other                                                                                              => other.pure[CompilerIO]
  }

  private def weaveNode(
      node: Sourced[MonomorphicExpression],
      carrier: ValueFQN
  ): CompilerIO[Sourced[MonomorphicExpression]] =
    weaveExpression(node.value.expression, carrier).map(woven =>
      node.as(MonomorphicExpression(node.value.expressionType, woven))
    )

  /** Whether a reference is an abstract *user* effect operation — an `Qualifier.Ability` method whose ability is not the
    * carrier machinery. These are exactly the references the effect-blind desugar/resolver left unresolved.
    */
  private def isUserEffectOperation(vfqn: ValueFQN): Boolean =
    EffectMachinery.abilityNameOf(vfqn).exists(name => !EffectMachinery.isMachineryAbility(name))

  /** Resolve a user effect operation at the base carrier: query `AbilityImplementation` at the single ability-level
    * carrier argument (the carrier type constructor), returning the concrete impl method FQN and its impl type
    * arguments — the impl marker's pattern binders bound to ground (e.g. `[IO]` for `Console[F]`). `None` when the
    * operation does not resolve at one carrier argument (a later slice).
    */
  private def resolveOperation(vfqn: ValueFQN, carrier: ValueFQN): CompilerIO[Option[(ValueFQN, Seq[GroundValue])]] =
    getFactIfProduced(AbilityImplementation.Key(vfqn, Seq(carrierConstructor(carrier)), Platform.Runtime))
      .map(_.flatMap(_.resolution.resolved))

  /** Whether the value performs any user effect — read off its declared channel row (`OperatorResolvedValue.effectRow`).
    * An effectful value's woven signature is carrier-headed; a pure value's is unchanged.
    */
  private def isEffectful(vfqn: ValueFQN): CompilerIO[Boolean] =
    getFactIfProduced(OperatorResolvedValue.Key(vfqn, Platform.Runtime))
      .map(_.exists(orv => EffectAccountingProcessor.channelDeclaredEffects(orv.effectRow).nonEmpty))

  /** The carrier type constructor unapplied (`IO`), the ability-level argument the effect instance's carrier binder is
    * matched against.
    */
  private def carrierConstructor(carrier: ValueFQN): GroundValue =
    GroundValue.Structure(carrier, Seq.empty, GroundValue.Type)

  /** The carrier applied to a payload (`IO[Unit]`) — the woven signature of an effectful value. */
  private def carrierApplied(carrier: ValueFQN, payload: GroundValue): GroundValue =
    GroundValue.Structure(carrier, Seq(payload), GroundValue.Type)
}
