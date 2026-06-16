package com.vanillasource.eliot.eliotc.monomorphize.fact

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** Like [[NativeBinding]], but built *without* the `opaque` guard: an `opaque` definition's body IS cached and therefore
  * unfoldable here. This is the post-checking counterpart of the checker's [[NativeBinding]] — the checker keeps opaque
  * bodies stuck so distinct type arguments stay distinct ([[com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue.checkingRuntime]]),
  * whereas representation lowering (Phase 3) needs to unfold them to reach the machine representation
  * (`opaque type Int[MIN, MAX] = <Jvm* repr>`). Non-opaque definitions have identical bindings here and in
  * [[NativeBinding]].
  *
  * @param vfqn
  *   The fully qualified name of the value
  * @param semValue
  *   The semantic value for NbE evaluation, with opaque bodies unfoldable
  */
case class TransparentBinding(
    vfqn: ValueFQN,
    semValue: SemValue
) extends CompilerFact {
  override def key(): CompilerFactKey[TransparentBinding] =
    TransparentBinding.Key(vfqn)
}

object TransparentBinding {
  case class Key(vfqn: ValueFQN) extends CompilerFactKey[TransparentBinding]
}
