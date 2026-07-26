package com.vanillasource.eliot.eliotc.row.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.row.fact.RowElaboratedValue
import com.vanillasource.eliot.eliotc.row.{RowChecker, RowElaborator}
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
      universeFor(value, key.platform).map { universe =>
        RowElaboratedValue(value.copy(runtime = RowElaborator.elaborate(value, universe).orElse(value.runtime)))
      }
    } else {
      RowElaboratedValue(value).pure[CompilerIO]
    }
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
      val missed = mutable.Set.empty[ValueFQN]
      RowElaborator.elaborate(value, RowChecker.Universe(known, runBoundaryFunctions, fqn => { missed += fqn; () }))
      val fresh  = missed.toSet -- attempted
      if (fresh.isEmpty) RowChecker.Universe(known, runBoundaryFunctions).pure[CompilerIO]
      else
        fresh.toSeq
          .traverse(fqn => getFactIfProduced(OperatorResolvedValue.Key(fqn, platform)).map(fqn -> _))
          .flatMap(fetched => loop(known ++ fetched.collect { case (fqn, Some(orv)) => fqn -> orv }, attempted ++ fresh))
    }

    loop(Map.empty, Set.empty)
  }
}
