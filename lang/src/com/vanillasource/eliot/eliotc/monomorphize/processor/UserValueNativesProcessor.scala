package com.vanillasource.eliot.eliotc.monomorphize.processor

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.fact.NativeBinding
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Emits [[NativeBinding]] facts for user-defined values: the binding used during type checking. Each value becomes a
  * `VTopDef` with a lazy thunk that evaluates the value's body on demand.
  *
  * An `opaque` definition is treated as a stuck, identity-based reference during checking: its body is NOT cached (via
  * [[OperatorResolvedValue.checkingRuntime]]), so the evaluator never unfolds it — it stays a `VTopDef(fqn, None, ...)`,
  * exactly like a body-less native/type constructor. The body remains in [[OperatorResolvedValue.runtime]] for later
  * phases (representation lowering, via
  * [[com.vanillasource.eliot.eliotc.monomorphize.fact.TransparentBinding]]) to unfold. This keeps a platform type with a
  * body — like `opaque type Int[MIN, MAX] = <repr>` — distinct per type argument, so range assignability stays sound
  * instead of the type collapsing to its body.
  */
class UserValueNativesProcessor
    extends BindingProcessor[NativeBinding.Key](key => OperatorResolvedValue.Key(key.vfqn)) {

  override protected def selfBody(fact: OperatorResolvedValue): Option[Sourced[OperatorResolvedExpression]] =
    fact.checkingRuntime

  override protected def buildFact(vfqn: ValueFQN, semValue: SemValue): NativeBinding =
    NativeBinding(vfqn, semValue)
}
