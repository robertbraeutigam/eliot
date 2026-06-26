package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.fact.ContributedBinding
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The user supplier: emits the total [[ContributedBinding]] under [[ContributedBinding.userLabel]] for the binding the
  * NbE checker would build from a value's own (checking) body. Each body-ful value contributes a `VTopDef` carrying a
  * lazy thunk that evaluates the body on demand; a body-less value contributes `None` (its checking implementation is a
  * native — or, for a runtime-only function, the evaluator's stuck `VNeutral` fallback — never an empty user `VTopDef`).
  *
  * An `opaque` definition is treated as a stuck, identity-based reference during checking: its body is NOT cached (via
  * [[OperatorResolvedValue.checkingRuntime]]), so the evaluator never unfolds it — it stays a `VTopDef(fqn, None, ...)`,
  * exactly like a body-less native/type constructor. The body remains in [[OperatorResolvedValue.runtime]] for later
  * phases (representation lowering, via
  * [[com.vanillasource.eliot.eliotc.monomorphize.fact.TransparentBinding]]) to unfold. This keeps a platform type with a
  * body — like `opaque type Int[MIN, MAX] = <repr>` — distinct per type argument, so range assignability stays sound
  * instead of the type collapsing to its body.
  *
  * Selected by the [[BindingMergerProcessor]] only when no native supplies the name; native+user coexistence (e.g.
  * `add`'s compile-time native and its runtime body) is benign, the native winning by category precedence.
  */
class UserValueNativesProcessor
    extends BindingProcessor[ContributedBinding.Key](key => SaturatedValue.Key(key.vfqn)) {

  // Only this label's queries belong to the user contributor; short-circuit other labels before fetching SaturatedValue.
  override protected def generateFact(key: ContributedBinding.Key): CompilerIO[Unit] =
    if (key.label === ContributedBinding.userLabel) super.generateFact(key) else abort

  // Checking: a body-less value's implementation is a native (or, for a runtime-only function, the evaluator's stuck
  // VNeutral fallback) — never an empty user binding that would shadow a reducing native. See BindingProcessor.
  override protected def bindsBodylessValues: Boolean = false

  override protected def selfBody(fact: OperatorResolvedValue): Option[Sourced[OperatorResolvedExpression]] =
    fact.checkingRuntime

  override protected def buildFact(vfqn: ValueFQN, binding: Option[SemValue]): CompilerIO[ContributedBinding] =
    ContributedBinding(vfqn, ContributedBinding.userLabel, binding).pure[CompilerIO]
}
