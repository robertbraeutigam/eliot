package com.vanillasource.eliot.eliotc.row.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.row.fact.RowElaboratedValue
import com.vanillasource.eliot.eliotc.row.{RowChecker, RowElaborator}
import com.vanillasource.eliot.eliotc.source.content.Sourced.{compilerAbort, compilerError}
import com.vanillasource.eliot.eliotc.termination.fact.RecursionCheckedValue

import scala.collection.mutable

/** The effects-as-rows **elaboration phase** (`docs/effects-as-rows.md` §3, migration step R5): rewrites every
  * demanded value's direct-style runtime body into explicit monadic core via [[RowElaborator]], so the checker,
  * monomorphization and codegen all consume bodies whose `flatMap`/`pure`/`runId` are already written down.
  *
  * Placed after [[RecursionCheckedValue]] (the recursion gate walks the *user's* reference graph, before any
  * machinery call is spliced in) and before `SaturatedValueProcessor` (whose input is repointed to this fact), so
  * everything from saturation onwards sees elaborated bodies. A value that is not [[RowChecker.checkable]] — body-less,
  * a `@Signature` twin, a type constructor or meta companion — is carried through unchanged, as is a value whose body
  * is already pure (elaboration is the identity on pure code).
  *
  * **The universe is built by demand, not guessed.** Elaboration is decision-free but not context-free: it consults
  * the *declared* signature, effect row and slot modes of every callee it meets, plus one level of type alias inside
  * those signatures (the `=>` alias) and the registered run boundaries. Which names those are cannot be read off the
  * body alone — a slot's classification decides whether a further name is consulted at all. So this processor runs
  * elaboration against a [[RowChecker.Universe]] that *reports* every name it misses, fetches exactly those, and
  * repeats until a round misses nothing new; the last round's universe is complete for this value by construction.
  * Guessing the set instead would silently fall back to the elaborator's unknown-callee approximations — a wrong slot
  * mode changes *when* an effect runs, which no later phase can catch.
  *
  * The fetch is [[getFactIfProduced]] by design: a referenced name legitimately has no [[OperatorResolvedValue]] when
  * it is an effect-ability method resolved by qualifier only, and the elaborator's declared-row fallback handles
  * exactly that case.
  *
  * @param runBoundaryFunctions
  *   The platform-registered run boundaries (jvm's `eliot.jvm::runMain`), threaded from `LangProcessors` out of
  *   [[com.vanillasource.eliot.eliotc.row.RunBoundaryFunctions.configKey]]: a nominal-run return (`def main:
  *   IO[Unit]`) and a run-boundary argument are carrier regions, which is declared information only this registry
  *   holds.
  */
class RowElaborationProcessor(runBoundaryFunctions: Set[ValueFQN] = Set.empty)
    extends TransformationProcessor[RecursionCheckedValue.Key, RowElaboratedValue.Key](key =>
      RecursionCheckedValue.Key(key.vfqn, key.platform)
    ) {

  override protected def generateFromKeyAndFact(
      key: RowElaboratedValue.Key,
      recursionChecked: RecursionCheckedValue
  ): CompilerIO[RowElaboratedValue] = {
    val value = recursionChecked.value
    if (RowChecker.checkable(value)) {
      for {
        universe    <- universeFor(value, key.platform)
        _           <- verifyRow(value, universe).whenA(key.platform == Platform.Runtime)
        elaboration  = RowElaborator.elaborateChecked(value, universe)
        _           <- verifyRowlessSlots(elaboration).whenA(key.platform == Platform.Runtime)
      } yield RowElaboratedValue(value.copy(runtime = elaboration.body.orElse(value.runtime)))
    } else {
      RowElaboratedValue(value).pure[CompilerIO]
    }
  }

  /** The per-definition **row verification** (`docs/effects-as-rows.md` §2/§5): `derived ⊆ declared`, checked on the
    * declared world before elaboration, located at the definition itself. A leak aborts this value — nothing
    * downstream (checker, mono, codegen) runs for it, so the user gets one located, effect-vocabulary error instead
    * of the machinery's downstream symptoms (the cryptic `AbilityResolver` demand for a `State`/`Throw`/`Abort`
    * leak, a mono-time carrier mismatch). The wording matches accounting's, so the two verifiers speak one language.
    *
    * Enforcement is bounded twice, and by what is *undecidable from declarations* rather than by A.8.6's withdrawn
    * uncertainty (A.11.6):
    *
    *   - **coverage** (`unknownCallees` empty): an unknown callee means the derivation may be incomplete, and a
    *     possibly-wrong pre-mono error is worse than deferring to the post-mono
    *     [[com.vanillasource.eliot.eliotc.monomorphize.channel.EffectAccountingProcessor]], which stays wired as the
    *     unconditional ground-truth fail-safe gating codegen;
    *   - **decidability** ([[RowChecker.RowResult.decidable]]): a definition declaring no ambient whose return could
    *     itself be the carrier is the constructor-class shape, which only the instantiation settles.
    *
    * What A.11.6 removed is the *third* bounding: enforcement no longer requires a declared ambient at all, so a
    * pure-returning definition that performs an effect is reported here, in effect vocabulary, at its own
    * definition — the diagnostic the post-mono `DeclaredPureChecker` used to voice after a failed monomorphization.
    */
  private def verifyRow(value: OperatorResolvedValue, universe: RowChecker.Universe): CompilerIO[Unit] =
    RowChecker.checkValue(value.vfqn, universe) match {
      case Some(result) if result.leak.nonEmpty && result.decidable && result.unknownCallees.isEmpty =>
        val names   = result.leak.toSeq.map(_.abilityName).sorted
        val word    = if (names.sizeIs == 1) "effect" else "effects"
        val pronoun = if (names.sizeIs == 1) "it" else "them"
        compilerAbort[Unit](
          value.name.as(
            s"This value performs the $word ${names.map(n => s"'$n'").mkString(", ")} but does not declare $pronoun; " +
              s"add $pronoun to its { ... } effect set."
          )
        )
      case _                                                                                                =>
        ().pure[CompilerIO]
    }

  /** The **rule-4 enforcement** (`docs/effects-as-rows.md` §1 rule 4, A.11.7-T step 2): a computation may not be
    * delivered to a position that declares no effect row. Reported at the offending *argument*, and aborting this
    * value, because a re-routed computation is a miscompile rather than a mistyping — the five `State`-family shapes
    * A.11.7-R measured all ran silently, printing `null` or nothing at all.
    *
    * The fix is always the same shape and the diagnostic says so: call the target **directly**, where its own
    * declared slot (pinned, carrier-typed, or row-declaring) already has the right mode. What the rule takes away is
    * the *dot* spelling of a discharge (`p.runStateToValue(s0)`), not the discharge — and in particular not the infix
    * dischargers, which resolve straight to a direct call.
    */
  private def verifyRowlessSlots(elaboration: RowElaborator.Elaborated): CompilerIO[Unit] =
    elaboration.violations match {
      case Seq()      => ().pure[CompilerIO]
      case violations =>
        violations.traverse_(message => compilerError(message, rowlessSlotHelp)) >> abort[Unit]
    }

  private val rowlessSlotHelp = Seq(
    "A type parameter that declares no effect row is a payload: it can never stand for a computation.",
    "Call the function directly instead of routing the computation through this position, for example",
    "'runStateToPair(initial, program)' rather than 'program.runStateToPair(initial)'.",
    "Infix dischargers are unaffected: 'x catch (err -> ...)' and 'setting else \"default\"' already resolve",
    "to a direct call with the computation at its own declared slot."
  )

  /** The complete declared world for elaborating this one value: fetch what elaboration misses, re-run, repeat.
    * Terminates because each round strictly grows the set of names already attempted, over the finite set of names
    * reachable from the body.
    */
  private def universeFor(value: OperatorResolvedValue, platform: Platform): CompilerIO[RowChecker.Universe] = {
    def loop(
        known: Map[ValueFQN, OperatorResolvedValue],
        attempted: Set[ValueFQN]
    ): CompilerIO[RowChecker.Universe] = {
      val missed    = mutable.Set.empty[ValueFQN]
      val reporting = RowChecker.Universe(known, runBoundaryFunctions, fqn => { missed += fqn; () })
      RowElaborator.elaborate(value, reporting)
      RowChecker.checkValue(value.vfqn, reporting)
      val fresh     = missed.toSet -- attempted
      if (fresh.isEmpty) RowChecker.Universe(known, runBoundaryFunctions).pure[CompilerIO]
      else
        fresh.toSeq
          .traverse(fqn => getFactIfProduced(OperatorResolvedValue.Key(fqn, platform)).map(fqn -> _))
          .flatMap(fetched => loop(known ++ fetched.collect { case (fqn, Some(orv)) => fqn -> orv }, attempted ++ fresh))
    }

    // The value itself is part of its own universe: the verification looks it up, and a self-reference in a signature
    // read must not count as a coverage gap.
    loop(Map(value.vfqn -> value), Set.empty)
  }
}
