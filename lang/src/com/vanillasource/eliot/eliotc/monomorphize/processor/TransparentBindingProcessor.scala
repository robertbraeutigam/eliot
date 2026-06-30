package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.monomorphize.fact.TransparentBinding
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue

/** Emits [[TransparentBinding]] facts: the post-checking counterpart of [[UserValueNativesProcessor]] that caches
  * `opaque` bodies so representation lowering (Phase 3) can unfold them.
  *
  * Builds its binding through the shared [[BindingClosure]], selecting
  * [[com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue.runtime]] (keep `opaque`) instead of
  * [[com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue.checkingRuntime]] (drop `opaque`) for the
  * value's own body. A body-less runtime native (e.g. `nativeWiden`, the jvm integer leaf ops) yields an
  * FQN-preserving stuck `VTopDef` here so representation lowering can read it back into codegen. Non-opaque definitions
  * therefore have identical bindings here and in [[com.vanillasource.eliot.eliotc.monomorphize.fact.NativeBinding]].
  *
  * This is a single-owner fact, never read by the [[BindingMergerProcessor]] — so reading [[NativeBinding]] for its
  * dependencies (via [[BindingClosure]]) is a plain downstream read, not part of the merger's cycle.
  */
class TransparentBindingProcessor
    extends TransformationProcessor[SaturatedValue.Key, TransparentBinding.Key](key => SaturatedValue.Key(key.vfqn)) {

  override protected def generateFromKeyAndFact(
      key: TransparentBinding.Key,
      fact: SaturatedValue
  ): CompilerIO[TransparentBinding] =
    BindingClosure.buildBinding(fact, _.runtime).map(TransparentBinding(key.vfqn, _))
}
