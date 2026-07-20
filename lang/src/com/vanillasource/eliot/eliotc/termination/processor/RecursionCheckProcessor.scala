package com.vanillasource.eliot.eliotc.termination.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.namedvalues.fact.NamedValuesRewrittenValue
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.termination.fact.RecursionCheckedValue

/** The no-recursion gate (termination M1): runs [[RecursionChecker]] on every value the program demands and certifies
  * it free of body-level recursion as a [[RecursionCheckedValue]].
  *
  * It is a standalone phase in the value chain, placed after `NamedValuesRewriteProcessor` (so the `namedValues`
  * reflection has already expanded into the reference graph this checker walks) and before
  * `SaturatedValueProcessor` (whose sole input is this fact — the former pre-mono effect gate that once sat between
  * them was retired when effect verification moved into the monomorphize phase). A recursive value has a
  * [[com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError]] reported against it, which trips
  * `registerFactIfClear` so the [[RecursionCheckedValue]] is never registered — and because saturation (and therefore
  * monomorphization and codegen) reads only this fact, the recursive value never gets any further. The certified value
  * is carried through untouched; the signature, body and every other field are unchanged.
  */
class RecursionCheckProcessor
    extends TransformationProcessor[NamedValuesRewrittenValue.Key, RecursionCheckedValue.Key](key =>
      NamedValuesRewrittenValue.Key(key.vfqn, key.platform)
    ) {

  private lazy val recursionChecker = new RecursionChecker

  override protected def generateFromKeyAndFact(
      key: RecursionCheckedValue.Key,
      rewritten: NamedValuesRewrittenValue
  ): CompilerIO[RecursionCheckedValue] =
    recursionChecker.check(rewritten.value, key.platform).as(RecursionCheckedValue(rewritten.value))
}
