package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.monomorphize.fact.{BindingContribution, ContributedBinding}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue

/** Shared base for the two *user-category* body suppliers — [[UserValueNativesProcessor]] (the runtime layer) and
  * [[CompilerNativesProcessor]] (the compiler platform). Each reads the value's own [[SaturatedValue]] (under its
  * platform `marker`) and contributes a [[com.vanillasource.eliot.eliotc.monomorphize.fact.BindingContribution.Body]]
  * for a body-ful value, or `None` for a body-less one (whose checking implementation is a native, never an empty user
  * binding that would shadow a reducing native — the `add` bug).
  *
  * Crucially these suppliers do **not** resolve dependencies: they read only [[SaturatedValue]], never [[NativeBinding]].
  * The transitive dependency closure is done by the [[BindingMergerProcessor]] for the *selected* contribution, so a
  * supplier never reads back the merged fact it feeds — the [[NativeBinding]] recursion stays the merger's single,
  * self-contained concern.
  */
abstract class BodyContributorProcessor(protected val label: String, marker: Platform)
    extends TransformationProcessor[SaturatedValue.Key, ContributedBinding.Key](key =>
      SaturatedValue.Key(key.vfqn, marker)
    ) {

  override protected def generateFromKeyAndFact(
      key: ContributedBinding.Key,
      fact: SaturatedValue
  ): CompilerIO[ContributedBinding] =
    ContributedBinding(
      key.vfqn,
      label,
      Option.when(fact.value.runtime.isDefined)(BindingContribution.Body(fact))
    ).pure[CompilerIO]
}
