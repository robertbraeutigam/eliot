package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.effect.processor.{EffectCarriers, EffectMachinery}
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.{SignatureView, spine}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

/** The "declared pure but performs an effect" fail-safe — the one effect diagnostic the post-mono
  * [[com.vanillasource.eliot.eliotc.monomorphize.channel.EffectAccountingProcessor]] cannot voice, because it concerns a
  * value whose monomorphization *fails*: a pure, nullary non-carrier return cannot host the effect its body performs, so
  * the carrier never resolves, the drain leaves a committed mismatch, and no `MonomorphicValue` fact is produced (so
  * accounting never runs). Split out of the deleted `EffectResidualChecker` (whose `derived ⊆ declared` subset check
  * accounting now replaces, U4-c-2). Run per value mono from [[TypeStackLoop.runPostDrainResolution]], after the final
  * drain, reading `unifier.errors` while the ambient carrier's identity is intact.
  *
  * A value **with** an ambient effect carrier (an open `{E...}` row) is the subset verifier's job and is skipped here. A
  * value with **no** carrier binder is checked: an *applied* return (`IO[Unit]`, `Pair[..]`) can legitimately host
  * effects and is exempt; a nullary non-carrier return (`String`, `Unit`) whose body-check left a committed mismatch AND
  * whose body performs an effect gets the friendly message, pre-empting the raw return-boundary mismatch. This checker
  * needs neither the unifier's `force` nor the ambient-carrier bookkeeping (a pure value has no ambient to filter
  * against) — only the track platform, to read a callee's declared effects.
  */
class DeclaredPureChecker(platform: Platform) {

  /** Verify a value's effects against a **pure** declaration. A value with an ambient effect carrier is skipped (the
    * subset verifier owns it); a value with none runs the declared-pure fail-safe.
    */
  def check(body: Sourced[SemExpression], resolvedValue: OperatorResolvedValue): CheckIO[Unit] = {
    val view         = SignatureView.of(resolvedValue.signature)
    val carrierNames = EffectCarriers.carrierBinders(view).filter(resolvedValue.paramConstraints.contains)
    if (carrierNames.nonEmpty) pure(()) else checkDeclaredPure(body, resolvedValue, view)
  }

  /** A value with no ambient carrier whose *return type cannot host effects* (a nullary `String`/`Unit`, not an applied
    * `IO[..]`/`Pair[..]`) but whose body performs an effect is reported with a friendly message. Discharge-aware by
    * construction: it fires only when the body-check left a committed mismatch, so a *fully discharged* pure body (whose
    * residual carrier defaulted to `Id` and reconciled with the pure return) has no error and is accepted — while
    * `def helper: String = printLine(readLine)`, whose `Console` cannot default to `Id`, is caught.
    */
  private def checkDeclaredPure(
      body: Sourced[SemExpression],
      resolvedValue: OperatorResolvedValue,
      view: SignatureView
  ): CheckIO[Unit] =
    if (spine(view.returnType.value)._2.nonEmpty) pure(()) // an applied return can legitimately host the effect carrier
    else
      for {
        mismatched <- inspect(_.unifier.errors.nonEmpty)
        effectful  <- if (!mismatched) pure(false) else bodyPerformsEffect(body.value)
        _          <- if (effectful) reportDeclaredPure(resolvedValue) else pure(())
      } yield ()

  /** Whether any reference in the body performs (or propagates) a non-machinery effect — the ambient-independent read
    * (a pure value has no ambient carrier to filter against).
    */
  private def bodyPerformsEffect(expr: SemExpression): CheckIO[Boolean] =
    collectValueRefs(expr).toList.traverse(effectsOf).map(_.exists(_.nonEmpty))

  /** The user-facing effects a reference contributes: an ability method performs its owning ability (read off the FQN's
    * qualifier — pre-quote, effect refs are still `Qualifier.Ability`; machinery excluded); an ordinary value propagates
    * the effects declared on its own carrier binders (read from its signature).
    */
  private def effectsOf(vfqn: ValueFQN): CheckIO[Set[AbilityFQN]] =
    EffectMachinery.abilityNameOf(vfqn) match {
      case Some(name) if EffectMachinery.isMachineryAbility(name) => pure(Set.empty)
      case Some(name)                                             => pure(Set(AbilityFQN(vfqn.moduleName, name)))
      case None                                                   =>
        liftF(getFactIfProduced(OperatorResolvedValue.Key(vfqn, platform))).map {
          case Some(orv) =>
            val view = SignatureView.of(orv.signature)
            EffectCarriers.declaredEffects(
              EffectCarriers.carrierBinders(view).filter(orv.paramConstraints.contains),
              orv.paramConstraints
            )
          case None      => Set.empty
        }
    }

  /** Every value reference in a checked body (parameter references and literals excluded). */
  private def collectValueRefs(expr: SemExpression): Seq[ValueFQN] =
    expr.expression match {
      case SemExpression.ValueReference(vfqn, _)               => Seq(vfqn.value)
      case SemExpression.FunctionApplication(target, argument) =>
        collectValueRefs(target.value) ++ collectValueRefs(argument.value)
      case SemExpression.FunctionLiteral(_, _, body)           => collectValueRefs(body.value)
      case _                                                   => Seq.empty
    }

  private def reportDeclaredPure(resolvedValue: OperatorResolvedValue): CheckIO[Unit] =
    liftF(
      compilerError(
        resolvedValue.name.as(
          "This value performs an effect but is declared pure; declare an effect set with { ... } or return an effect " +
            "carrier."
        )
      ) >> abort[Unit]
    )
}
