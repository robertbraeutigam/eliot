package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.effect.processor.{EffectCarriers, EffectMachinery}
import com.vanillasource.eliot.eliotc.monomorphize.check.AbilityResolver.AbilityRef
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.SignatureView
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

/** Exact effect *verification* in the checker — the monomorphize-phase replacement for the pre-mono
  * `DeclaredEffectChecker` (docs/effect-accounting-in-monomorphize.md, Step 1). It computes the value-under-check's
  * **residual effect set** — the effect abilities demanded on the value's *own ambient carrier* — and requires it to be
  * a subset of what the value declares. Unlike the syntactic pre-mono accounting, this reads the *checked, instantiated*
  * body, so a discharged effect (its ability method demanded on an inner transformer carrier, never the ambient one) is
  * simply absent from the residual — discharge falls out structurally, with no `-E` annotation. `Inf` rides the ambient
  * carrier like any effect, so the same subset test covers it.
  *
  * A collaborator of [[Checker]] (constructed with the one primitive it needs, `force`), invoked from
  * [[TypeStackLoop.runPostDrainResolution]] after ability resolution and the final drain but *before* meta
  * defaulting — so an ability reference's carrier argument is solved to the ambient carrier (concrete `IO`, or a still
  * abstract carrier meta) while the ambient carrier's identity is intact.
  *
  * Step 1 is additive: it runs behind the still-active pre-mono gate, so it only ever sees pre-mono-*valid* bodies, on
  * which the residual is a subset of the (pre-mono-accepted) used set and hence of the declared set — a no-op. It is
  * wired to error so a genuine leak surfacing once the gate is flipped (Step 2) is caught here at the definition.
  */
class EffectResidualChecker(force: SemValue => CheckIO[SemValue]) {

  /** Verify that the value's residual effects are declared. A value with no ambient effect carrier (a pure or
    * concrete-carrier value) has nothing to check — mirroring the pre-mono subset check's `carrier.isEmpty` guard.
    */
  def check(abilityRefs: Seq[AbilityRef], resolvedValue: OperatorResolvedValue): CheckIO[Unit] = {
    val view         = SignatureView.of(resolvedValue.signature)
    val carrierNames = EffectCarriers.carrierBinders(view).filter(resolvedValue.paramConstraints.contains)
    if (carrierNames.isEmpty) pure(())
    else
      for {
        ambientHeads <- effectiveAmbientHeads
        residual     <- residualEffects(abilityRefs, ambientHeads)
        declared      = EffectCarriers.declaredEffects(carrierNames, resolvedValue.paramConstraints)
        undeclared    = residual.diff(declared)
        _            <- if (undeclared.isEmpty) pure(()) else reportUndeclared(resolvedValue, undeclared)
      } yield ()
  }

  /** The effect abilities demanded on the ambient carrier: each ability-qualified reference (excluding the internal
    * `Effect`/`Suspend` machinery) one of whose type arguments forces to an ambient-carrier head. A discharged effect's
    * ability method carries an *inner* carrier (`StateCarrier[S, G]`) in that slot, so it does not match and is excluded.
    */
  private def residualEffects(
      abilityRefs: Seq[AbilityRef],
      ambientHeads: Set[CheckState.CarrierHead]
  ): CheckIO[Set[AbilityFQN]] =
    abilityRefs.toList.foldLeftM(Set.empty[AbilityFQN]) { case (acc, (vfqn, typeArgs)) =>
      EffectMachinery.abilityNameOf(vfqn.value) match {
        case Some(name) if !EffectMachinery.isMachineryAbility(name) =>
          ridesAmbient(typeArgs, ambientHeads).map(rides =>
            if (rides) acc + AbilityFQN(vfqn.value.moduleName, name) else acc
          )
        case _                                                       => pure(acc)
      }
    }

  /** Whether any of an ability reference's type arguments forces to a head in the ambient-carrier set. */
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
