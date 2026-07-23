package com.vanillasource.eliot.eliotc.monomorphize.channel

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.EffectRow
import com.vanillasource.eliot.eliotc.effect.processor.EffectMachinery
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.{MonomorphicExpression, MonomorphicValue}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue.ResolvedAbilityConstraint
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

/** The effects-as-channel **effect accounting** processor (docs/effects-as-channel.md §5) — the *real* effect
  * verification path under `--effect-channel`, a post-monomorphization rider on [[MonomorphicValue]] built on the same
  * template as [[RefinementChannelProcessor]]. It computes each mono'd value's **derived effect row** by a bottom-up
  * walk of its checked body and requires `derived ⊆ declared`, with a source-anchored, effect-vocabulary diagnostic —
  * replacing the in-checker Phase-2 shadow ([[com.vanillasource.eliot.eliotc.monomorphize.check.EffectResidualChecker]],
  * deleted at Phase 4). With the checker effect-blind, the ground truth is purely syntactic:
  *
  *   - an **effect-ability method** reference (`Console::printLine`, left abstract by the effect-blind desugar/resolver,
  *     its `Qualifier.Ability` intact) contributes its owning ability — machinery abilities (`Effect`/`Suspend`)
  *     excluded, first-order abilities never appearing here (they are resolved to concrete impls, so they carry no
  *     `Qualifier.Ability`);
  *   - an **ordinary callee** contributes its *declared* row, read from the callee's channel metadata
  *     (`OperatorResolvedValue.effectRow`); so an effect propagates from callee to caller through the same union.
  *
  * `Inf` is an ordinary entry and rides the union like any effect, so the totality story is unchanged. The fact is only
  * produced once `derived ⊆ declared` holds; an undeclared effect is reported at the value and the accounting **declines
  * (aborts)** — the fail-safe that keeps a leaking value out of codegen.
  *
  * Scope note (this slice): the transparent-parameter expansion (`Effect`-marked callback positions), reify/discharge
  * subtraction, and the carrier-machinery-impl exception (§0/§11) are later slices; a foundation program (direct effect
  * operations + ordinary-callee propagation) is fully and soundly accounted by the two rules above. Off the flag the
  * processor is inert (an empty accounting) — the carrier path verifies inside the checker.
  */
class EffectAccountingProcessor(effectChannel: Boolean = false)
    extends TransformationProcessor[MonomorphicValue.Key, EffectAccounting.Key](key =>
      MonomorphicValue.Key(key.vfqn, key.typeArguments)
    )
    with Logging {

  override protected def generateFromKeyAndFact(
      key: EffectAccounting.Key,
      mv: MonomorphicValue
  ): CompilerIO[EffectAccounting] =
    if (!effectChannel) EffectAccounting(key.vfqn, key.typeArguments, Set.empty).pure[CompilerIO]
    else
      for {
        derived   <- derivedRow(mv)
        declared  <- declaredEffectsOf(mv.vfqn)
        undeclared = derived.diff(declared)
        _         <- if (undeclared.isEmpty) ().pure[CompilerIO] else reportUndeclared(mv, undeclared)
      } yield EffectAccounting(key.vfqn, key.typeArguments, derived)

  /** The value's derived row: the union, over every value reference in the body, of that reference's contributed
    * effects. Empty for a body-less value.
    */
  private def derivedRow(mv: MonomorphicValue): CompilerIO[Set[AbilityFQN]] =
    mv.runtime.fold(Set.empty[AbilityFQN].pure[CompilerIO]) { body =>
      collectReferences(body.value).toList.foldLeftM(Set.empty[AbilityFQN])((acc, ref) =>
        contributedEffects(ref).map(acc ++ _)
      )
    }

  /** The effects one reference contributes: its owning ability if it is an effect-ability method (machinery excluded),
    * else the callee's declared row.
    */
  private def contributedEffects(ref: ValueFQN): CompilerIO[Set[AbilityFQN]] =
    EffectMachinery.abilityNameOf(ref) match {
      case Some(name) if EffectMachinery.isMachineryAbility(name) => Set.empty[AbilityFQN].pure[CompilerIO]
      case Some(name)                                            => Set(AbilityFQN(ref.moduleName, name)).pure[CompilerIO]
      case None                                                  => declaredEffectsOf(ref)
    }

  /** A value's declared effect row, read from its channel metadata (`OperatorResolvedValue.effectRow`, on the runtime
    * track). Empty when the value declares no effects (or its front-end fact is not available).
    */
  private def declaredEffectsOf(vfqn: ValueFQN): CompilerIO[Set[AbilityFQN]] =
    getFactIfProduced(OperatorResolvedValue.Key(vfqn, Platform.Runtime)).map {
      case Some(orv) => EffectAccountingProcessor.channelDeclaredEffects(orv.effectRow)
      case None      => Set.empty
    }

  /** Every value reference in a monomorphic body, in traversal order (parameter references and literals excluded). */
  private def collectReferences(expr: MonomorphicExpression.Expression): Seq[ValueFQN] = expr match {
    case MonomorphicExpression.MonomorphicValueReference(vfqn, _) => Seq(vfqn.value)
    case MonomorphicExpression.FunctionApplication(target, arg)  =>
      collectReferences(target.value.expression) ++ collectReferences(arg.value.expression)
    case MonomorphicExpression.FunctionLiteral(_, _, body)       => collectReferences(body.value.expression)
    case _                                                       => Seq.empty
  }

  private def reportUndeclared(mv: MonomorphicValue, undeclared: Set[AbilityFQN]): CompilerIO[Unit] = {
    val names   = undeclared.toSeq.map(_.abilityName).sorted
    val word    = if (names.sizeIs == 1) "effect" else "effects"
    val pronoun = if (names.sizeIs == 1) "it" else "them"
    compilerAbort[Unit](
      mv.name.as(
        s"This value performs the $word ${names.map(n => s"'$n'").mkString(", ")} but does not declare $pronoun; " +
          s"add $pronoun to its { ... } effect set."
      )
    )
  }
}

object EffectAccountingProcessor {

  /** The declared effect abilities read straight from a value's effect channel: the union of its open-row entries over
    * every position (return + effect-transparent parameters), with the machinery abilities (`Effect`/`Suspend`) removed.
    * Pure, so it is the unit-testable core of the channel-declared computation (the durable successor to Phase 2's
    * `EffectResidualChecker.channelDeclaredEffects`).
    */
  private[channel] def channelDeclaredEffects(effectRow: EffectRow[ResolvedAbilityConstraint]): Set[AbilityFQN] =
    (effectRow.returnEffects ++ effectRow.parameterEffects.flatMap(_.effects))
      .map(_.abilityFQN)
      .filterNot(a => EffectMachinery.isMachineryAbility(a.abilityName))
      .toSet
}
