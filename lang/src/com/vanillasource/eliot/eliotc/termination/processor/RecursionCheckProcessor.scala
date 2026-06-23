package com.vanillasource.eliot.eliotc.termination.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.termination.fact.RecursionCheckedValue

/** The no-recursion gate (termination M1): runs [[RecursionChecker]] on every value the program demands and certifies
  * it free of body-level recursion as a [[RecursionCheckedValue]].
  *
  * It is a standalone phase in the value chain, placed after `OperatorResolverProcessor` (so application structure is
  * final) and before `EffectDesugaringProcessor` (whose sole input is repointed to this fact). A recursive value has a
  * [[com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError]] reported against it, which trips
  * `registerFactIfClear` so the [[RecursionCheckedValue]] is never registered — and because effect desugaring (and
  * therefore saturation, monomorphization and codegen) reads only this fact, the recursive value never gets any
  * further. The certified value is carried through untouched; the signature, body and every other field are unchanged.
  */
class RecursionCheckProcessor
    extends TransformationProcessor[OperatorResolvedValue.Key, RecursionCheckedValue.Key](key =>
      OperatorResolvedValue.Key(key.vfqn)
    ) {

  private lazy val recursionChecker = new RecursionChecker

  override protected def generateFromKeyAndFact(
      key: RecursionCheckedValue.Key,
      value: OperatorResolvedValue
  ): CompilerIO[RecursionCheckedValue] =
    recursionChecker.check(value).as(RecursionCheckedValue(value))
}
