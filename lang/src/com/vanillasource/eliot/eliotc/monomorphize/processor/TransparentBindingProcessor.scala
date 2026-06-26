package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.fact.TransparentBinding
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Emits [[TransparentBinding]] facts: the post-checking counterpart of [[UserValueNativesProcessor]] that caches
  * `opaque` bodies so representation lowering (Phase 3) can unfold them.
  *
  * Identical to [[UserValueNativesProcessor]] except it selects [[OperatorResolvedValue.runtime]] (keep `opaque`)
  * instead of [[OperatorResolvedValue.checkingRuntime]] (drop `opaque`) for the value's own body, and binds body-less
  * values (the runtime leaves) rather than declining them. Non-opaque definitions therefore have identical bindings
  * here and in [[com.vanillasource.eliot.eliotc.monomorphize.fact.NativeBinding]]. This stays a single-owner fact
  * unaffected by the [[ContributedBinding]] merge (native suppliers never emit a runtime body).
  */
class TransparentBindingProcessor
    extends BindingProcessor[TransparentBinding.Key](key => SaturatedValue.Key(key.vfqn)) {

  // Lowering: a body-less runtime native (e.g. `nativeWiden`, the jvm integer leaf ops) needs an FQN-preserving stuck
  // `VTopDef` here so representation lowering can read it back into codegen. See BindingProcessor.
  override protected def bindsBodylessValues: Boolean = true

  override protected def selfBody(fact: OperatorResolvedValue): Option[Sourced[OperatorResolvedExpression]] =
    fact.runtime

  // `bindsBodylessValues = true`, so `binding` is always `Some`; the `None` arm is unreachable here.
  override protected def buildFact(vfqn: ValueFQN, binding: Option[SemValue]): CompilerIO[TransparentBinding] =
    binding match {
      case Some(semValue) => TransparentBinding(vfqn, semValue).pure[CompilerIO]
      case None           => abort
    }
}
