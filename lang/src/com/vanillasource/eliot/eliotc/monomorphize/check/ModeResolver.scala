package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.unify.UnifyResult
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The **post-drain mode resolver** (docs/effects-as-rows.md A.8.7) — the one type-informed decision of the
  * effects-as-rows elaboration, made where types are ground: it finishes the desugar's *deferred* positions (a
  * computation at a bare-generic slot, a `let` over a bare-metavariable bound type) from the **solved** meta store at
  * post-drain quiescence, replacing the v2 checker's mid-spine Phase-B decision.
  *
  * During checking, a runtime-track computation meeting a bare-generic slot is **suspended** — recorded as a
  * [[CheckState.ModeObligation]] with *no* unification of its type into the slot, since first-contact unification is
  * itself a mode decision (`pick(readLine, "x")`: eagerly unifying `readLine : ?F[Str]` into `A` commits pass-through
  * before `"x"` can rigidify `A := Str`). At quiescence, everything the rest of the body can determine about the slot
  * metas is determined, and each obligation classifies three ways against the solved store:
  *
  *   - **carrier-headed** domain (an effect carrier by the ambient/role tag, or a still-generic applied metavariable)
  *     ⟹ *pass*: the computation flows through unrun, its type unified into the slot (`choose`'s branches);
  *   - **rigid non-carrier** domain with a *fitting payload* ⟹ *hoist*: the slot wants the payload, so the desugar's
  *     strict-hoist rewrite is spliced into the body and the mono restarts
  *     ([[com.vanillasource.eliot.eliotc.row.RowElaborator.spliceResolvedModes]] — one rulebook, the desugar
  *     finishing its job late). A non-fitting payload falls back to whole-type unification — a canonical carrier
  *     stack *captures* the computation by partial application (the dot-chained discharger), anything else commits
  *     the exact mismatch;
  *   - **still-unsolved** at full quiescence ⟹ the v2 default, kept: the meta riding into the call's result adopts
  *     the computation's carrier (`choose(readLine, readLine)` — pass-through), an absent one sequences (hoist —
  *     `putState(readLine)`'s `S`).
  *
  * The resolver's contract (the A.8.7 guardrails): it **reads** solved metas and **splices** desugar rewrites that
  * trigger a re-check. It never runs inside unification, never retracts a solution, never grows an ordering arm — a
  * shape that would need an obligation resolved *before* the drain can finish soundly is a stop-and-redecide signal,
  * not a license for a mid-flight arm.
  *
  * Operates over [[CheckIO]] like the other post-drain collaborators ([[CalculatedReturnResolver]],
  * [[AbilityResolver]]); driven exclusively by [[TypeStackLoop]]'s post-drain fixpoint, *before* ability resolution in
  * each round, so an adopted carrier can still have its instances resolved in the same round.
  *
  * @param force
  *   Force a SemValue through the current meta store — the checker's `force`.
  * @param doUnify
  *   Unify two semantic values, updating the unifier in the state — the checker's `doUnify`.
  * @param lifter
  *   The effect lifter, for the shared carrier recognition ([[EffectLifter.effectCarrierSplit]]).
  */
class ModeResolver(
    force: SemValue => CheckIO[SemValue],
    doUnify: (SemValue, SemValue, Sourced[String]) => CheckIO[Unit],
    lifter: EffectLifter
) {
  import CheckState.ModeObligation
  import CheckState.ModeObligation.Status

  /** Classify every still-pending obligation whose slot domain the drain has *solved* (a bare-metavariable domain
    * stays pending — only [[defaultUnsolvedObligations]] decides those, at full quiescence). Returns whether any
    * obligation was resolved (progress for the fixpoint). A classification of `Hoist` marks the obligation for the
    * splice ([[pendingHoistTargets]]) without unifying anything — the restarted check owns the rewritten node.
    */
  def resolveDecidedObligations: CheckIO[Boolean] =
    pendingIndices.flatMap(
      _.foldLeftM(false) { (progressed, index) =>
        inspect(_.modeObligations(index)).flatMap { obligation =>
          force(obligation.domain).flatMap {
            case VMeta(_, Spine.SNil) => pure(progressed)
            case forcedDomain         => classifySolved(index, obligation, forcedDomain).as(true)
          }
        }
      }
    )

  /** Classify one obligation against its *solved* domain. A carrier-headed domain (ambient/role tag) and a
    * still-generic applied metavariable pass the computation through — plain unification is correct there, the
    * carrier genuinely rides in. A rigid domain sequences first when the computation's payload speculatively fits
    * (the v2 `sequenceBeforeUnify` discipline — plain unify against a rigid data head would not fail but *steal the
    * carrier meta* by injective decomposition), and otherwise falls back to whole-type unification: a canonical
    * carrier stack captures the computation by partial application (`?F := StateCarrier[S, ?G]`), anything else is a
    * genuine mismatch, committed as one Expected/Actual error.
    */
  private def classifySolved(index: Int, obligation: ModeObligation, forcedDomain: SemValue): CheckIO[Unit] =
    lifter.effectCarrierSplit(forcedDomain).flatMap { domainSplit =>
      val passThrough = domainSplit.isDefined || forcedDomain.isInstanceOf[VMeta]
      if (passThrough) passObligation(index, obligation, forcedDomain)
      else
        payloadFits(obligation.argType, forcedDomain, obligation.argNode).flatMap {
          case true  => setStatus(index, Status.Hoist)
          case false => passObligation(index, obligation, forcedDomain)
        }
    }

  /** Whether the computation's *payload* speculatively fits the solved domain — the hoist test. The payload is split
    * off the carrier first, so the carrier meta can never be consulted (much less stolen) by the probe; a computation
    * whose carrier recognition dissolved by quiescence (no split) has no payload to hoist and passes whole.
    */
  private def payloadFits(
      argType: SemValue,
      forcedDomain: SemValue,
      argNode: Sourced[OperatorResolvedExpression]
  ): CheckIO[Boolean] =
    lifter.effectCarrierSplit(argType).flatMap {
      case None               => pure(false)
      case Some((_, payload)) =>
        inspect(
          _.unifier.tryUnify(payload, forcedDomain, argNode.as("Type mismatch.")) match {
            case UnifyResult.Unified(_)       => true
            case UnifyResult.Contradiction(_) => false
          }
        )
    }

  /** Pass the computation through: unify its carrier-headed type into the slot, committing the exact mismatch as one
    * Expected/Actual error when the whole-type unification contradicts (the v2 `commitMismatch` shape).
    */
  private def passObligation(index: Int, obligation: ModeObligation, forcedDomain: SemValue): CheckIO[Unit] =
    get.flatMap { state =>
      state.unifier.tryUnify(obligation.argType, forcedDomain, obligation.argNode.as("Type mismatch.")) match {
        case UnifyResult.Unified(unified) => modify(_.withUnifier(unified)) >> setStatus(index, Status.Passed)
        case UnifyResult.Contradiction(_) =>
          modify(s =>
            s.withUnifier(
              s.unifier.addMismatch(obligation.argType, forcedDomain, obligation.argNode.as("Type mismatch."))
            )
          ) >> setStatus(index, Status.Passed)
      }
    }

  /** The full-quiescence default for obligations whose slot meta *nothing* solved — v2's Phase-B default, kept: the
    * meta occurring in the call's result adopts the computation's carrier (pass-through — `choose(readLine,
    * readLine)`'s branches instantiate `A` at the carrier, and the enclosing slot decides), a meta absent from the
    * result cannot ride up and the computation must be sequenced (hoist — `putState(readLine)`, whose `S` is stranded
    * inside a type argument otherwise). Returns whether any obligation was *adopted* (progress that may unblock
    * ability resolution); hoists are collected via [[pendingHoistTargets]].
    */
  def defaultUnsolvedObligations: CheckIO[Boolean] =
    pendingIndices.flatMap(
      _.foldLeftM(false) { (progressed, index) =>
        inspect(_.modeObligations(index)).flatMap { obligation =>
          force(obligation.domain).flatMap {
            case meta @ VMeta(id, Spine.SNil) =>
              for {
                ridesUp <- inspect(_.unifier.occursInValue(id, obligation.retType))
                split   <- lifter.effectCarrierSplit(obligation.argType)
                result  <- if (ridesUp || split.isEmpty)
                             doUnify(meta, obligation.argType, obligation.argNode.as("Type mismatch.")) >>
                               setStatus(index, Status.Passed).as(true)
                           else setStatus(index, Status.Hoist).as(progressed)
              } yield result
            case forcedDomain                 => classifySolved(index, obligation, forcedDomain).as(true)
          }
        }
      }
    )

  /** Whether any obligation is marked for hoisting — the fixpoint stops and splices when so. */
  def hasPendingHoists: CheckIO[Boolean] =
    inspect(_.modeObligations.exists(_.status == Status.Hoist))

  /** The argument nodes marked for the strict-hoist splice, in recording (spine, left-to-right) order, each with the
    * desugar's **core rule applied with the mode known**: `true` when the hoisted spine's solved result type is a
    * rigid non-carrier — a *payload* core, which the splice must `pure`-wrap as the chain's innermost continuation
    * (leaving it bare would meet the machinery's carrier codomain on the re-check, where a bare-generic tail is
    * stolen by first-contact unification). A carrier-headed or still-undetermined spine result stays unwrapped — the
    * carrier chains, and an undetermined one is finished by the checker (deferral, as everywhere).
    */
  def pendingHoistTargets: CheckIO[Seq[(Sourced[OperatorResolvedExpression], Boolean)]] =
    inspect(_.modeObligations.filter(_.status == Status.Hoist)).flatMap(
      _.toList
        .traverse { obligation =>
          for {
            forced <- force(obligation.spineType)
            split  <- lifter.effectCarrierSplit(forced)
          } yield (obligation.argNode, split.isEmpty && forced.isInstanceOf[VTopDef])
        }
        .widen[Seq[(Sourced[OperatorResolvedExpression], Boolean)]]
    )

  /** The deferred `let` bindings whose bound type resolved **carrier-headed** by quiescence — the bindings that must
    * become `flatMap(x -> rest, bound)` (the desugar's binding rule, applied late). Consulted only at full quiescence
    * with no hoists pending, so every bound meta is final. A bound type that stayed pure (or a canonical carrier
    * stack — deliberate storage, matching the build-time rule) keeps its plain `let`.
    */
  def pendingLetTargets: CheckIO[Seq[Sourced[OperatorResolvedExpression]]] =
    inspect(_.letObligations).flatMap(
      _.toList
        .traverse(obligation =>
          lifter.effectCarrierSplit(obligation.argType).map(split => Option.when(split.isDefined)(obligation.argNode))
        )
        .map(_.flatten)
    )

  private def pendingIndices: CheckIO[List[Int]] =
    inspect(_.modeObligations.zipWithIndex.collect { case (ob, i) if ob.status == Status.Pending => i }.toList)

  private def setStatus(index: Int, status: Status): CheckIO[Unit] =
    modify(s => s.copy(modeObligations = s.modeObligations.updated(index, s.modeObligations(index).copy(status = status))))
}
