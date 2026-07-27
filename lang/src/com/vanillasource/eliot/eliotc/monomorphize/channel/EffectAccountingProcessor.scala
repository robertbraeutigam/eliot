package com.vanillasource.eliot.eliotc.monomorphize.channel

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.EffectRow
import com.vanillasource.eliot.eliotc.effect.processor.{EffectCarriers, EffectMachinery}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{Qualifier, ValueFQN, WellKnownTypes}
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
  * `derived ⊆ declared` for a value with an open effect row ([[verifySubset]]), with a source-anchored,
  * effect-vocabulary diagnostic. The mono body reaches it with every ability reference already resolved to its concrete
  * implementation, and each contribution is **gated by the ride test** ([[contributedEffects]], [[ridesAmbient]]): a
  * reference counts only if it performs its effect on the value's own ambient carrier.
  *
  *   - an **effect-ability method** reference (`Console::printLine`, resolved to `Qualifier.AbilityImplementation`)
  *     contributes its owning ability *iff it rides*; a first-order (`Show`/`Eq`) impl is pure (empty ambient) so it
  *     never rides, and the match-family eliminators (`PatternMatch`/`TypeMatch`) plus the machinery (`Effect`/`Suspend`)
  *     are excluded outright ([[nonEffectAbility]]);
  *   - an **ordinary callee** contributes its *declared* row (its carrier-binder ability constraints, [[openRow]] — the
  *     single source of truth) iff it rides; a discharged/captured/lifted callee (whose carrier is an inner transformer
  *     stack, not the ambient) drops out — discharge is structural.
  *
  * `Inf` is an ordinary entry and rides the union like any effect. The fact is only produced once `derived ⊆ declared`
  * holds; an undeclared effect is reported at the value and the accounting **declines (aborts)**, and
  * [[com.vanillasource.eliot.eliotc.monomorphize.channel.WovenValueProcessor]]'s `getFactOrAbort` precondition turns that
  * abort into a blocked codegen (U4-c-1).
  *
  * '''Status (U4-c-2, docs/effects-as-channel.md §0/§10).''' This is the **sole subset verifier**, wired as a codegen
  * precondition and verifying **unconditionally** (the old pre-mono `EffectResidualChecker` is deleted; its one remaining
  * diagnostic, "declared pure but performs an effect", is the pre-mono
  * [[com.vanillasource.eliot.eliotc.row.processor.RowElaborationProcessor]]'s row verification, because that concerns a
  * value whose mono *fails* and so never reaches accounting — A.11.6). A **concrete-carrier**
  * return (`def main: IO[Unit] = printLine(…)`) has no carrier binder and is exempt from the subset check — its explicitly
  * chosen carrier permits its effects ([[verifySubset]]). The transparent-parameter expansion (`Effect`-marked callback
  * positions) and the reify-legality check are later slices.
  */
class EffectAccountingProcessor
    extends TransformationProcessor[MonomorphicValue.Key, EffectAccounting.Key](key =>
      MonomorphicValue.Key(key.vfqn, key.typeArguments)
    )
    with Logging {

  // effects-as-channel U4-c-2 (docs/effects-as-channel.md §10): accounting is the **sole** subset verifier and verifies
  // **unconditionally** (the pre-mono `EffectResidualChecker` is deleted). `WovenValueProcessor`'s `getFactOrAbort`
  // precondition makes an undeclared effect block codegen.
  override protected def generateFromKeyAndFact(
      key: EffectAccounting.Key,
      mv: MonomorphicValue
  ): CompilerIO[EffectAccounting] =
    for {
      derived <- derivedRow(mv)
      _       <- verifySubset(mv, derived)
    } yield EffectAccounting(key.vfqn, key.typeArguments, derived)

  /** The `derived ⊆ declared` subset check — fired **only for a value with an open effect row** (a constrained carrier
    * binder). A **concrete-carrier** return (`def main: IO[Unit] = printLine(…)`) has no carrier binder, so it declares no
    * row to be a subset of; its explicitly chosen carrier permits its effects and it is **exempt**, exactly as the
    * pre-mono row verification exempts a return that may itself carry. A genuinely pure value whose body performs an
    * effect never reaches here — its mono fails first (the effect cannot resolve on no carrier), and that "declared
    * pure" diagnostic is the row verification's (A.11.6).
    */
  private def verifySubset(mv: MonomorphicValue, derived: Set[AbilityFQN]): CompilerIO[Unit] =
    getFactIfProduced(OperatorResolvedValue.Key(mv.vfqn, Platform.Runtime)).flatMap {
      case Some(orv) =>
        val (carrierNames, declared) = openRow(orv)
        val undeclared               = derived.diff(declared)
        if (carrierNames.isEmpty || undeclared.isEmpty) ().pure[CompilerIO] else reportUndeclared(mv, undeclared)
      case None      => ().pure[CompilerIO]
    }

  /** The value's derived row: the union, over every value reference in the body, of that reference's contributed
    * effects — each **gated by the ride test** against the value's own ambient carriers. Empty for a body-less value.
    */
  private def derivedRow(mv: MonomorphicValue): CompilerIO[Set[AbilityFQN]] =
    mv.runtime.fold(Set.empty[AbilityFQN].pure[CompilerIO]) { body =>
      collectReferences(body.value).toList.foldLeftM(Set.empty[AbilityFQN]) { case (acc, (ref, typeArgs)) =>
        contributedEffects(ref, typeArgs, mv.ambientCarriers).map(acc ++ _)
      }
    }

  /** The effects one reference contributes: its owning ability (for an ability-method reference) or the callee's declared
    * row (for an ordinary callee) — but **only if the reference rides the value's own ambient carrier** (U4-c-0d). The
    * candidate set is gated through [[EffectAccountingProcessor.ridesAmbient]], so a discharged / captured / lifted callee
    * (whose carrier is an inner transformer stack, not the ambient) contributes nothing — discharge is structural.
    *
    * A monomorphic body reaches this processor *after* the checker has resolved every ability reference to its concrete
    * implementation ([[com.vanillasource.eliot.eliotc.monomorphize.check.PostDrainQuoter.resolveIfAbility]]), so an
    * effect operation is a value carrying `Qualifier.AbilityImplementation(abilityName, _)` — not the abstract
    * `Qualifier.Ability` the pre-mono view had. **No effect-vs-first-order marker lookup is needed** (the source of a
    * spurious `Could not find` when a non-colocated / synthetic marker was resolved): the ride test *plus* the
    * [[nonEffectAbility]] exclusion discriminate them. A **first-order** impl (`Show`/`Eq`/`==`, `Combine`, `Arithmetic`)
    * is pure by construction — its method's result type is a fixed non-carrier type, so its own
    * `MonomorphicValue.ambientCarriers` is empty and it can never ride; its owning-ability candidate is simply dropped by
    * the ride test. The **match-family / machinery** eliminators (`PatternMatch`/`TypeMatch`, `Effect`/`Suspend`) are
    * excluded by name up front: they are compiler-inserted structural dispatch, never a user effect, and — unlike a
    * first-order ability — a match eliminator's result type *follows its branches*, so over an effectful `match` it is
    * carrier-headed (non-empty ambient) and would otherwise spuriously ride. The contributed [[AbilityFQN]]'s **module**
    * is `ref.moduleName` — for a true effect impl that is the ability's module (instances are colocated with their
    * ability), so `derived` matches `declared` ([[declaredEffectsOf]], same module).
    *
    * A constraint-covered effect method the checker left abstract (`Qualifier.Ability`, resolved at the caller's level —
    * not seen on a fully-ground runtime mono) rides the value's own ambient carrier **by definition**, so the abstract arm
    * contributes **unconditionally** (there is no resolved callee mono to ride-test).
    */
  private def contributedEffects(
      ref: ValueFQN,
      typeArgs: Seq[GroundValue],
      ambient: Set[GroundValue]
  ): CompilerIO[Set[AbilityFQN]] =
    ref.name.qualifier match {
      case Qualifier.Ability(name) if EffectMachinery.isMachineryAbility(name)     =>
        Set.empty[AbilityFQN].pure[CompilerIO]
      case Qualifier.Ability(name)                                                 =>
        Set(AbilityFQN(ref.moduleName, name)).pure[CompilerIO]
      case Qualifier.AbilityImplementation(name, _) if nonEffectAbility(name)      =>
        Set.empty[AbilityFQN].pure[CompilerIO]
      case Qualifier.AbilityImplementation(name, _)                                =>
        gatedByRide(Set(AbilityFQN(ref.moduleName, name)), ref, typeArgs, ambient)
      case _                                                                       =>
        declaredEffectsOf(ref).flatMap(gatedByRide(_, ref, typeArgs, ambient))
    }

  /** Ability names that are **never** a user effect and must not contribute even if they ride: the compiler machinery
    * (`Effect`/`Suspend`) and the match-family eliminators (`PatternMatch`/`TypeMatch`), all compiler-inserted structural
    * dispatch. First-order abilities (`Show`/`Eq`/…) need no listing — their result type is non-carrier, so their impl's
    * ambient is empty and the ride test drops them; the match eliminators are listed because their result type follows the
    * eliminated branches and so is carrier-headed over an effectful `match` (a non-empty ambient that would ride).
    */
  private def nonEffectAbility(name: String): Boolean =
    EffectMachinery.isMachineryAbility(name) ||
      name == WellKnownTypes.patternMatchAbilityName ||
      name == WellKnownTypes.typeMatchAbilityName

  /** Gate a reference's candidate effects through the ride test: the effects count only if the reference performs them on
    * the value's own ambient carrier. The reference's carriers are the **callee's own forwarded ambient carriers** at the
    * reference's mono key ([[MonomorphicValue.ambientCarriers]], the U4-c-0a writer) — read via `getFactOrAbort`, so a
    * counted-class reference whose callee mono is (unexpectedly) absent **aborts** rather than contributing `Set.empty`:
    * silent-empty is the under-count direction, the one that lets a leak through (§5 check 3, [[feedback_gaps_must_be_failsafe]]).
    * A pure candidate (empty) short-circuits without a fetch. A non-effect impl's callee mono exists but carries an **empty**
    * ambient (it is pure), so the fetch succeeds and the ride simply fails — the natural, lookup-free effect filter.
    */
  private def gatedByRide(
      candidate: Set[AbilityFQN],
      ref: ValueFQN,
      typeArgs: Seq[GroundValue],
      ambient: Set[GroundValue]
  ): CompilerIO[Set[AbilityFQN]] =
    if (candidate.isEmpty) Set.empty[AbilityFQN].pure[CompilerIO]
    else
      getFactOrAbort(MonomorphicValue.Key(ref, typeArgs)).map { callee =>
        if (EffectAccountingProcessor.ridesAmbient(callee.ambientCarriers, ambient)) candidate else Set.empty
      }

  /** A callee's declared effect abilities (the `._2` of its [[openRow]]) — the row [[gatedByRide]] propagates from a
    * callee to its caller. Empty when the value declares no effects, or its front-end fact is unavailable.
    */
  private def declaredEffectsOf(vfqn: ValueFQN): CompilerIO[Set[AbilityFQN]] =
    getFactIfProduced(OperatorResolvedValue.Key(vfqn, Platform.Runtime)).map(_.map(openRow(_)._2).getOrElse(Set.empty))

  /** A value's **open effect row**, read off its `OperatorResolvedValue`: its constrained carrier binders
    * (`carrierBinders ∩ paramConstraints` — empty for a concrete-carrier or pure return, non-empty for an `{E...}` row)
    * and the user-facing effects declared on them (machinery excluded). The single source of truth for "declared"
    * (U4-c-0b): surface rows desugar *into* these constraints, and hand-written carrier-generic code (the stdlib dischargers, the lifting
    * instances `implement[S, G ~ Abort] Abort[StateCarrier[S, G]]`) declares its effects *only* this way. The
    * carrier-binder set is what [[verifySubset]] gates on (an empty set = no row to be a subset of = exempt).
    */
  private def openRow(orv: OperatorResolvedValue): (Set[String], Set[AbilityFQN]) = {
    val carrierNames = EffectCarriers.carrierBinders(OperatorResolvedExpression.SignatureView.of(orv.signature))
      .filter(orv.paramConstraints.contains)
    (carrierNames, EffectCarriers.declaredEffects(carrierNames, orv.paramConstraints))
  }

  /** Every value reference in a monomorphic body **with its ground type arguments**, in traversal order (parameter
    * references and literals excluded). The `typeArguments` are the reference's own mono key — kept (not discarded as
    * `(vfqn, _)`, the naive-wiring gap of §5/pinned finding 8) so [[gatedByRide]] can fetch the callee's forwarded
    * ambient carriers at exactly this reference's instantiation.
    */
  private def collectReferences(expr: MonomorphicExpression.Expression): Seq[(ValueFQN, Seq[GroundValue])] = expr match {
    case MonomorphicExpression.MonomorphicValueReference(vfqn, typeArgs) => Seq((vfqn.value, typeArgs))
    case MonomorphicExpression.FunctionApplication(target, arg)          =>
      collectReferences(target.value.expression) ++ collectReferences(arg.value.expression)
    case MonomorphicExpression.FunctionLiteral(_, _, body)              => collectReferences(body.value.expression)
    case _                                                              => Seq.empty
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

  /** Whether a reference **rides** one of the value-under-check's own ambient carriers (docs/effects-as-channel.md §5):
    * one of the reference's own carrier ground value(s) equals an ambient carrier by **exact `GroundValue` equality**.
    *
    * The `referenceCarriers` are the **callee's own forwarded ambient carriers** at the reference's mono key
    * (`MonomorphicValue.ambientCarriers`, computed once by the U4-c-0a writer) — which is precisely the reference's
    * carrier for every reference class, with no positional reconstruction: a generic effect method / carrier-generic
    * callee (`printLine@[IO]`, `loopForever@[IO]`) forwards its carrier-binder value, and a **binder-less
    * concrete-carrier impl** (`implement Inf[IO]`, whose impl method carries no type argument) forwards the carrier read
    * from its signature return head — both already resolved into the one `ambientCarriers` field. Reading it (rather than
    * re-deriving from `typeArguments` positions) also means the two sides compare identical quotes, so exact equality is
    * reliable.
    *
    * Exactness is the load-bearing choice — strictly tighter than a head-level carrier test:
    *   - a captured / discharged callee's carrier is an **inner transformer stack** (`ThrowCarrier[E, IO]`), unequal to
    *     the ambient `IO`, so it does not ride — capture/discharge is structural, with no `-E` annotation;
    *   - it separates **nested same-transformer stacks** (`ThrowCarrier[E2, ThrowCarrier[E1, IO]]` ≠ the ambient
    *     `ThrowCarrier[E1, IO]`), which a head-level `ThrowCarrier == ThrowCarrier` test would wrongly conflate;
    *   - an **empty** ambient set (a pure value, the synthetic entry) never rides — there is **no synthetic-entry
    *     exemption**, it simply has nothing to ride.
    */
  private[channel] def ridesAmbient(referenceCarriers: Set[GroundValue], ambient: Set[GroundValue]): Boolean =
    referenceCarriers.exists(ambient.contains)
}
