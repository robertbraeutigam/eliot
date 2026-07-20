package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.effect.processor.{EffectCarriers, EffectMachinery}
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.{SignatureView, spine}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

/** Exact effect *verification* in the checker — the monomorphize-phase replacement for the now-deleted pre-mono
  * declared-effect phase. It computes the value-under-check's
  * **residual effect set** — the effect abilities demanded on the value's *own ambient carrier* — and requires it to be
  * a subset of what the value declares.
  *
  * The residual is read off the *checked, instantiated* body, so a discharged effect is naturally absent: an effect is
  * counted iff the reference that performs or propagates it has one of its type arguments headed by an ambient carrier.
  *   - An **ability method** (`Console.printLine[F]`) performs its owning ability when its carrier `F` is the ambient.
  *   - An **ordinary value** (`doLog : {Log} Unit`, or a whole `{State}` computation) propagates the effects declared
  *     on its carrier binders when that carrier is instantiated to the ambient. A *discharged* callee has its effect on
  *     an inner transformer carrier (`StateCarrier[S, G]`, not the ambient `G`), so no type argument is ambient-headed
  *     and it drops out — discharge falls out structurally, with no `-E` annotation.
  *
  * `Inf` rides the ambient carrier like any effect, so the same subset test covers it (and it is load-bearing: `Inf[IO]`
  * resolves, so without this check an undeclared `Inf` — direct or propagated through a callee — would compile).
  *
  * A collaborator of [[Checker]] (constructed with `force` plus the track platform for callee-signature reads), invoked
  * from [[TypeStackLoop.runPostDrainResolution]] after ability resolution and the final drain but *before* meta
  * defaulting — so a reference's carrier argument is solved to the ambient carrier (concrete `IO`, or a still abstract
  * carrier meta) while the ambient carrier's identity is intact. Runs only for a value mono, never a signature twin.
  */
class EffectResidualChecker(force: SemValue => CheckIO[SemValue], platform: Platform) {

  /** Verify a value's effects against its declared signature. A value *with* an ambient effect carrier (a `{E...}` row)
    * runs the subset check; a value with **none** (a pure or concrete-carrier return) runs the declared-pure fail-safe.
    */
  def check(body: Sourced[SemExpression], resolvedValue: OperatorResolvedValue): CheckIO[Unit] = {
    val view         = SignatureView.of(resolvedValue.signature)
    val carrierNames = EffectCarriers.carrierBinders(view).filter(resolvedValue.paramConstraints.contains)
    if (carrierNames.nonEmpty) checkSubset(body, resolvedValue, carrierNames)
    else checkDeclaredPure(body, resolvedValue, view)
  }

  /** The subset check: a carrier-polymorphic body's residual effects must be a subset of what it declares. */
  private def checkSubset(
      body: Sourced[SemExpression],
      resolvedValue: OperatorResolvedValue,
      carrierNames: Set[String]
  ): CheckIO[Unit] =
    for {
      ambientHeads <- effectiveAmbientHeads
      residual     <- residualEffects(collectValueRefs(body.value), ambientHeads)
      declared      = EffectCarriers.declaredEffects(carrierNames, resolvedValue.paramConstraints)
      undeclared    = residual.diff(declared)
      _            <- if (undeclared.isEmpty) pure(()) else reportUndeclared(resolvedValue, undeclared)
    } yield ()

  /** The "declared pure but performs effects" fail-safe. A value with no ambient carrier whose *return type cannot host
    * effects* (a nullary `String`/`Unit`, not an applied `IO[..]`/`Pair[..]`) but whose body performs an effect is
    * reported with a friendly message, pre-empting the raw return-boundary mismatch. Discharge-aware by construction:
    * it fires only when the body-check left a committed mismatch, so a *fully discharged* pure body (`sign(f) = if(f,
    * "+") else "-"`, whose residual carrier defaulted to `Id` and reconciled with the pure return) has no error and is
    * accepted — while `def helper: String = printLine(readLine)`, whose `Console` cannot default to `Id`, is caught.
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
    * used by the declared-pure fail-safe (a pure value has no ambient carrier to filter against).
    */
  private def bodyPerformsEffect(expr: SemExpression): CheckIO[Boolean] =
    collectValueRefs(expr).toList.traverse(ref => effectsOf(ref._1.value)).map(_.exists(_.nonEmpty))

  /** The effect abilities demanded on the ambient carrier: each value reference whose contributed effects are non-empty
    * and one of whose type arguments forces to an ambient-carrier head.
    */
  private def residualEffects(
      refs: Seq[(Sourced[ValueFQN], Seq[SemValue])],
      ambientHeads: Set[CheckState.CarrierHead]
  ): CheckIO[Set[AbilityFQN]] =
    refs.toList.foldLeftM(Set.empty[AbilityFQN]) { case (acc, (vfqn, typeArgs)) =>
      for {
        effects <- effectsOf(vfqn.value)
        rides   <- if (effects.isEmpty) pure(false) else ridesAmbient(typeArgs, ambientHeads)
      } yield if (rides) acc ++ effects else acc
    }

  /** The user-facing effects a reference contributes when its carrier rides the ambient: an ability method performs its
    * owning ability (read off the FQN's qualifier, excluding the `Effect`/`Suspend` machinery); an ordinary value
    * propagates the effects declared on its own carrier binders (read from its signature — no discharge subtraction,
    * that is structural in whether the carrier is ambient at all).
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

  /** Whether any of a reference's type arguments forces to a head in the ambient-carrier set. */
  private def ridesAmbient(typeArgs: Seq[SemValue], ambientHeads: Set[CheckState.CarrierHead]): CheckIO[Boolean] =
    typeArgs.toList.traverse(arg => force(arg).map(headOf)).map(_.flatten.exists(ambientHeads.contains))

  /** The ambient-carrier heads, each recorded *meta* head re-forced through the current metastore — so a carrier meta
    * solved to a concrete carrier after it was recorded (`IO`, or the compiler track's pinned `Either[E]`) is matched by
    * its solution's head, not its stale meta id. Mirrors [[EffectLifter]]'s `effectiveAmbientHeads`.
    */
  private def effectiveAmbientHeads: CheckIO[Set[CheckState.CarrierHead]] =
    for {
      heads    <- inspect(_.ambientCarriers)
      reforced <- heads.toList.traverse {
                    case m @ CheckState.CarrierHead.Meta(id) => force(VMeta(MetaId(id), Spine.SNil)).map(headOf(_).getOrElse(m))
                    case concrete                            => pure(concrete)
                  }
    } yield reforced.toSet

  private def headOf(sv: SemValue): Option[CheckState.CarrierHead] = sv match {
    case VTopDef(fqn, _, _) => Some(CheckState.CarrierHead.TopDef(fqn))
    case VMeta(id, _)       => Some(CheckState.CarrierHead.Meta(id.value))
    case _                  => None
  }

  /** Every value reference in a checked body, paired with its type arguments. */
  private def collectValueRefs(expr: SemExpression): Seq[(Sourced[ValueFQN], Seq[SemValue])] =
    expr.expression match {
      case SemExpression.ValueReference(vfqn, typeArgs)        => Seq((vfqn, typeArgs))
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

  private def reportUndeclared(resolvedValue: OperatorResolvedValue, undeclared: Set[AbilityFQN]): CheckIO[Unit] = {
    val names   = undeclared.toSeq.map(_.abilityName).sorted
    val word    = if (names.sizeIs == 1) "effect" else "effects"
    val pronoun = if (names.sizeIs == 1) "it" else "them"
    liftF(
      compilerError(
        resolvedValue.name.as(
          s"This value performs the $word ${names.map(n => s"'$n'").mkString(", ")} but does not declare $pronoun; " +
            s"add $pronoun to its { ... } effect set."
        )
      ) >> abort[Unit]
    )
  }
}
