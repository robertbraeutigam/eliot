package com.vanillasource.eliot.eliotc.row.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.row.fact.RowElaboratedValue
import com.vanillasource.eliot.eliotc.row.{RowChecker, RowElaborator}
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort
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
  *   The platform-registered run boundaries (jvm's `eliot.jvm::runMain`), threaded from `LangProcessors` exactly as
  *   [[com.vanillasource.eliot.eliotc.monomorphize.processor.RunBoundaryFunctionProcessor]] receives them: a
  *   nominal-run return (`def main: IO[Unit]`) and a run-boundary argument are carrier regions, which is declared
  *   information only this registry holds.
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
        universe <- universeFor(value, key.platform)
        _        <- verifyRow(value, universe).whenA(key.platform == Platform.Runtime)
      } yield RowElaboratedValue(value.copy(runtime = RowElaborator.elaborate(value, universe).orElse(value.runtime)))
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
    * Enforcement is deliberately bounded twice, in the deferral spirit (A.8.6):
    *
    *   - **only for a definition that declares an ambient** (a non-empty declared row / pinned return): its sequenced
    *     contributions ride that ambient by construction, so "what the body does must fit what the signature
    *     declares" is decidable from declarations. A definition with *no* ambient is skipped — whether a
    *     carrier-ability call there is an effect leak or a constructor-class use (`def f: Box[String] = wrap(s)`,
    *     `Container[Box]`) is decided by the *instantiation* (`F := Box` never rides), which pre-mono derivation
    *     cannot see; those stay with the mono-informed checkers (`DeclaredPureChecker`, the `Suspend`-at-`Id`
    *     message) until the A.8.6 resolver era;
    *   - **only under full coverage** (`unknownCallees` empty): an unknown callee means the derivation may be
    *     incomplete, and a possibly-wrong pre-mono error is worse than deferring to the post-mono
    *     [[com.vanillasource.eliot.eliotc.monomorphize.channel.EffectAccountingProcessor]], which stays wired as the
    *     unconditional ground-truth fail-safe gating codegen.
    */
  private def verifyRow(value: OperatorResolvedValue, universe: RowChecker.Universe): CompilerIO[Unit] =
    RowChecker.checkValue(value.vfqn, universe) match {
      case Some(result)
          if result.declared.nonEmpty && result.leak.nonEmpty &&
            result.unknownCallees.isEmpty && result.uncertain.isEmpty =>
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
