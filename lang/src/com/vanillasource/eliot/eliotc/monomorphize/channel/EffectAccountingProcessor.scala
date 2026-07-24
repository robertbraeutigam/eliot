package com.vanillasource.eliot.eliotc.monomorphize.channel

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.EffectRow
import com.vanillasource.eliot.eliotc.effect.processor.{EffectCarriers, EffectMachinery}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicExpression, MonomorphicValue}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue.ResolvedAbilityConstraint
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

/** The effects-as-channel **effect accounting** processor (docs/effects-as-channel.md §5) — the post-monomorphization
  * effect verifier, a rider on [[MonomorphicValue]] built on the same template as [[RefinementChannelProcessor]]. It
  * computes each mono'd value's **derived effect row** by a bottom-up walk of its checked body and requires
  * `derived ⊆ declared`, with a source-anchored, effect-vocabulary diagnostic. The mono body reaches it (on the carrier
  * path) with every ability reference already resolved to its concrete implementation, so the derivation reads:
  *
  *   - an **effect-ability method** reference (`Console::printLine`, resolved to `Qualifier.AbilityImplementation`)
  *     contributes its owning ability, discriminated from a first-order impl (`Show`/`Eq`/`==`) by the ability marker's
  *     higher-kinded carrier binder (machinery abilities `Effect`/`Suspend` excluded) — see [[contributedEffects]];
  *   - an **ordinary callee** contributes its *declared* row, read from the callee's own ambient carrier-binder ability
  *     constraints (the single source of truth shared with the residual checker); so an effect propagates from callee to
  *     caller through the same union.
  *
  * `Inf` is an ordinary entry and rides the union like any effect. The fact is only produced once `derived ⊆ declared`
  * holds; an undeclared effect is reported at the value and the accounting **declines (aborts)**.
  *
  * '''Status (U4-b / Bundle A, docs/effects-as-channel.md §0/§10).''' Verification still lives in
  * [[com.vanillasource.eliot.eliotc.monomorphize.check.EffectResidualChecker]]; this processor is **not yet wired as a
  * codegen precondition** (`effectChannel` gates it inert off the flag) because the row-based derivation here cannot yet
  * replicate the residual checker's **run/discharge subtraction**: walking the *fully monomorphic* body it has lost the
  * ambient-vs-concrete carrier distinction the checker still has (a `{Console}` value run on `IO`, and a discharged
  * `raise` on an inner transformer carrier, both read as bare effect ops here), so wiring it whole-base over-counts
  * (finding: the synthetic `main` runs `Console` on `IO` yet declares nothing). U4-c is blocked on that subtraction; see
  * §10. The re-point of [[contributedEffects]] to the resolved-impl (`AbilityImplementation`) view is landed and
  * validated (the user `main` of a Console program accounts as `{Console}` at its carrier-bound key).
  *
  * Scope note: the transparent-parameter expansion (`Effect`-marked callback positions) and reify/discharge subtraction
  * are later slices; the carrier-machinery-impl exception is **gone** — U4-c-0b reads "declared" from the carrier-binder
  * constraints ([[declaredEffectsOf]]), the single source of truth, so a hand-written carrier-generic discharger with no
  * surface `{E}` row accounts correctly by the rule.
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

  /** The effects one reference contributes: its owning ability if it performs an effect, else the callee's declared row.
    *
    * A monomorphic body reaches this processor *after* the checker has resolved every ability reference to its concrete
    * implementation ([[com.vanillasource.eliot.eliotc.monomorphize.check.PostDrainQuoter.resolveIfAbility]]), so an
    * effect operation is a value carrying `Qualifier.AbilityImplementation(abilityName, _)` — not the abstract
    * `Qualifier.Ability` the pre-mono view had. Two subtleties make recognising it more than a qualifier match:
    *   - **first-order abilities also resolve to `AbilityImplementation`** (`Show`/`Eq`/`==`, the synthetic
    *     `PatternMatch`/`TypeMatch`/`Meta` impls), so an *effect* ability must be discriminated by its **carrier**: the
    *     ability *marker* (the `Qualifier.Ability` value named after the ability) has a higher-kinded (`F[_]`) binder iff
    *     the ability is carrier-parametric, i.e. an effect. This is a fact lookup (the same test
    *     `AbilityResolver.isEffectAbilityRef` performed pre-mono), read on the ability marker rather than the impl
    *     marker — a concrete-carrier impl (`implement Inf[IO]`) has *no* HKT binder of its own, only the ability does.
    *   - the contributed [[AbilityFQN]]'s **module** must be the ability's, so `derived` matches `declared` (which
    *     [[declaredEffectsOf]] sources from the carrier-binder constraints, in the ability's module). Effect-ability instances are
    *     colocated with their ability (a carrier-generic `implement[F ~ E] Ability[F]` can only live in the ability's
    *     module; a concrete `implement Inf[IO]` is placed there too), so the impl method's own module *is* the ability's
    *     module — confirmed by looking the ability marker up there.
    *
    * A constraint-covered effect method that the checker left abstract (`Qualifier.Ability`, resolved at the caller's
    * level) is still handled by the first arm; the machinery (`Effect`/`Suspend`) is excluded in both.
    */
  private def contributedEffects(ref: ValueFQN): CompilerIO[Set[AbilityFQN]] =
    ref.name.qualifier match {
      case Qualifier.Ability(name) if EffectMachinery.isMachineryAbility(name)               =>
        Set.empty[AbilityFQN].pure[CompilerIO]
      case Qualifier.Ability(name)                                                           =>
        Set(AbilityFQN(ref.moduleName, name)).pure[CompilerIO]
      case Qualifier.AbilityImplementation(name, _) if !EffectMachinery.isMachineryAbility(name) =>
        isEffectAbility(ref.moduleName, name).map(if (_) Set(AbilityFQN(ref.moduleName, name)) else Set.empty)
      case _                                                                                  => declaredEffectsOf(ref)
    }

  /** Whether the ability named `abilityName` in `moduleName` is a **user effect ability** — carrier-parametric (a
    * higher-kinded `F[_]` binder on its marker) and not machinery. Read off the ability *marker*'s signature (the
    * `Qualifier.Ability` value named after the ability, colocated in the same module as any of its instances), exactly as
    * `AbilityResolver.isEffectAbilityRef` reads it pre-mono. A first-order ability (`Show`/`Eq`) has no such binder, and
    * the synthetic `PatternMatch`/`TypeMatch`/`Meta` impls have no ability marker at all, so both are correctly not
    * counted as effects. Absent marker ⟹ not an effect.
    */
  private def isEffectAbility(moduleName: ModuleName, abilityName: String): CompilerIO[Boolean] = {
    val markerFqn = ValueFQN(moduleName, QualifiedName(abilityName, Qualifier.Ability(abilityName)))
    getFactIfProduced(OperatorResolvedValue.Key(markerFqn, Platform.Runtime)).map {
      case Some(orv) =>
        OperatorResolvedExpression.SignatureView.of(orv.signature).binders.exists(EffectCarriers.isHktBinder)
      case None      => false
    }
  }

  /** A value's declared effect abilities — the ability constraints on its own ambient effect-carrier binders
    * (`carrierBinders ∩ paramConstraints`, machinery excluded), read off its `OperatorResolvedValue` on the runtime
    * track. This is the **single source of truth** for "declared" (U4-c-0b), shared verbatim with
    * [[com.vanillasource.eliot.eliotc.monomorphize.check.EffectResidualChecker]]: surface `{E...}` rows desugar *into*
    * these constraints, and hand-written carrier-generic code (the stdlib dischargers, the lifting instances
    * `implement[S, G ~ Abort] Abort[StateCarrier[S, G]]`) declares its effects *only* this way — so reading the
    * constraints rather than the surface `effectRow` makes those correct by the rule, with no carrier-machinery-impl
    * exception. Empty when the value declares no effects (or its front-end fact is not available).
    */
  private def declaredEffectsOf(vfqn: ValueFQN): CompilerIO[Set[AbilityFQN]] =
    getFactIfProduced(OperatorResolvedValue.Key(vfqn, Platform.Runtime)).map {
      case Some(orv) =>
        val view = OperatorResolvedExpression.SignatureView.of(orv.signature)
        EffectCarriers.declaredEffects(
          EffectCarriers.carrierBinders(view).filter(orv.paramConstraints.contains),
          orv.paramConstraints
        )
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

  /** The declared effect abilities read straight from a value's effect **channel metadata** (`EffectRow`): the union of
    * its open-row entries over every position (return + effect-transparent parameters), machinery (`Effect`/`Suspend`)
    * removed. This is the **rendering-side** row extraction (the LSP's declared-row vocabulary, §4/§5) — *not* a
    * verification input (U4-c-0b): `derived ⊆ declared` reads "declared" from the carrier-binder constraints
    * ([[declaredEffectsOf]]), the single source of truth, so a hand-written discharger with no surface `{E}` row still
    * accounts correctly. Pure, hence unit-testable in isolation.
    */
  private[channel] def channelDeclaredEffects(effectRow: EffectRow[ResolvedAbilityConstraint]): Set[AbilityFQN] =
    (effectRow.returnEffects ++ effectRow.parameterEffects.flatMap(_.effects))
      .map(_.abilityFQN)
      .filterNot(a => EffectMachinery.isMachineryAbility(a.abilityName))
      .toSet

  /** The reference's own **carrier** ground value(s) — the pure input to the ride test (U4-c-0c). Two sources, mutually
    * exclusive by construction:
    *   - a generic effect-ability method / carrier-generic callee (`printLine@[IO]`, a lifting
    *     `raise@[E, StateCarrier[S, G]]`) carries its carrier(s) in its `typeArguments` at the callee's
    *     **carrier-binder positions** (`carrierPositions`, the callee's higher-kinded binder indices, aligned by
    *     `binders.zipWithIndex` — the alignment `establishSignature` and the mono key share). A multi-binder callee
    *     (`Throw[E, G]`) has only its higher-kinded `G` counted, never its plain error binder `E`.
    *   - a **binder-less concrete-carrier impl** (`implement Inf[IO]`, whose impl method is fully concrete and carries no
    *     type argument, so `carrierPositions` is empty) has its fixed carrier read from its signature return head,
    *     supplied here as `concreteImplCarrier`.
    *
    * Empty ⇒ the reference has no carrier and can never ride (a first-order impl `Show[Int]` with no higher-kinded binder
    * and no concrete-carrier arm; a non-effect callee is never asked).
    */
  private[channel] def referenceCarriers(
      refTypeArgs: Seq[GroundValue],
      carrierPositions: Set[Int],
      concreteImplCarrier: Option[GroundValue]
  ): Set[GroundValue] =
    if (carrierPositions.nonEmpty) carrierPositions.flatMap(refTypeArgs.lift)
    else concreteImplCarrier.toSet

  /** Whether a reference **rides** one of the value's own ambient carriers (docs/effects-as-channel.md §5): one of the
    * reference's carrier ground value(s) ([[referenceCarriers]]) equals an ambient carrier by **exact `GroundValue`
    * equality**. Exactness is the load-bearing choice — strictly tighter than a head-level carrier test — and is what
    * makes the whole accounting fall out of one rule:
    *   - a discharged / captured op's carrier is an **inner transformer stack** (`ThrowCarrier[E, IO]`), unequal to the
    *     ambient `IO`, so it does not ride — discharge is structural, with no `-E` annotation;
    *   - it separates **nested same-transformer stacks** (`ThrowCarrier[E2, ThrowCarrier[E1, IO]]` ≠ the ambient
    *     `ThrowCarrier[E1, IO]`), which a head-level `ThrowCarrier == ThrowCarrier` test would wrongly conflate;
    *   - an **empty** ambient set (a pure value, the synthetic entry) never rides — there is **no synthetic-entry
    *     exemption**, it simply has nothing to ride.
    */
  private[channel] def ridesAmbient(
      refTypeArgs: Seq[GroundValue],
      carrierPositions: Set[Int],
      concreteImplCarrier: Option[GroundValue],
      ambient: Set[GroundValue]
  ): Boolean =
    referenceCarriers(refTypeArgs, carrierPositions, concreteImplCarrier).exists(ambient.contains)
}
