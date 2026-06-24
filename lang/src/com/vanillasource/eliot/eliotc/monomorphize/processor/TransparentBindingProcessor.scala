package com.vanillasource.eliot.eliotc.monomorphize.processor

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.fact.TransparentBinding
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Emits [[TransparentBinding]] facts: the post-checking counterpart of [[UserValueNativesProcessor]] that caches
  * `opaque` bodies so representation lowering (Phase 3) can unfold them.
  *
  * Identical to [[UserValueNativesProcessor]] except it selects [[OperatorResolvedValue.runtime]] (keep `opaque`)
  * instead of [[OperatorResolvedValue.checkingRuntime]] (drop `opaque`) for the value's own body. Non-opaque
  * definitions therefore have identical bindings here and in [[NativeBinding]].
  */
class TransparentBindingProcessor
    extends BindingProcessor[TransparentBinding.Key](key => SaturatedValue.Key(key.vfqn)) {

  // Lowering: a body-less runtime native (e.g. `nativeWiden`, the jvm integer leaf ops) needs an FQN-preserving stuck
  // `VTopDef` here so representation lowering can read it back into codegen. See BindingProcessor.
  override protected def bindsBodylessValues: Boolean = true

  override protected def selfBody(fact: OperatorResolvedValue): Option[Sourced[OperatorResolvedExpression]] =
    fact.runtime

  override protected def buildFact(vfqn: ValueFQN, semValue: SemValue): TransparentBinding =
    TransparentBinding(vfqn, semValue)
}
