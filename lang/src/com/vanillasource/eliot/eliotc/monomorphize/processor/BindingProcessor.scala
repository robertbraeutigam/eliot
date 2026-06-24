package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.NativeBinding
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerFactKey
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.saturate.fact.{BinderRoles, SaturatedValue}
import com.vanillasource.eliot.eliotc.source.content.Sourced

import scala.reflect.ClassTag

/** Shared base for the two processors that turn a [[SaturatedValue]] into a binding (a `VTopDef` carrying a lazy body
  * thunk) for the NbE evaluator. The checker's [[NativeBinding]] and Phase 3's
  * [[com.vanillasource.eliot.eliotc.monomorphize.fact.TransparentBinding]] differ in exactly one decision — whether an
  * `opaque` definition's body is kept — which subclasses make via [[selfBody]]
  * ([[OperatorResolvedValue.checkingRuntime]] drops it; [[OperatorResolvedValue.runtime]] keeps it).
  *
  * Reads the [[SaturatedValue]] (the same fact the monomorphize checker reads), not the raw [[OperatorResolvedValue]],
  * for two reasons: the saturated signature carries the binder roles ([[SaturatedValue.binderRoles]]) the wrap
  * consumes, and its leading binders line up with the type arguments the checker applies (saturation may prepend
  * binders for omittable `auto` parameters) — keeping the wrap's binder indices aligned (D6).
  *
  * The subtle, error-prone parts live here once: the concurrent generation guard against mutual-recursion deadlocks,
  * and the transitive dependency collection. Dependencies are always resolved via [[NativeBinding]] (the checker's
  * semantics, the bindings the bodies were type-checked against); only the top-level value's own body selection differs
  * between phases.
  */
abstract class BindingProcessor[OutputKey <: CompilerFactKey[?]](
    getInputKey: OutputKey => SaturatedValue.Key
)(using ClassTag[OutputKey])
    extends TransformationProcessor[SaturatedValue.Key, OutputKey](getInputKey) {

  /** The value's own body as this phase sees it: [[OperatorResolvedValue.checkingRuntime]] for the checker (opaque
    * bodies stay stuck), [[OperatorResolvedValue.runtime]] for representation lowering (opaque bodies unfold).
    */
  protected def selfBody(fact: OperatorResolvedValue): Option[Sourced[OperatorResolvedExpression]]

  /** Wrap the computed binding in the phase-specific output fact. */
  protected def buildFact(vfqn: ValueFQN, semValue: SemValue): OutputFact

  /** Thread-safe set of FQNs currently being generated. Prevents mutual recursion deadlocks in collectBindings. */
  private val generating: java.util.Set[ValueFQN] =
    java.util.Collections.newSetFromMap(new java.util.concurrent.ConcurrentHashMap())

  /** Whether this processor binds body-less definitions.
    *
    * The checking-phase NativeBinding producer ([[UserValueNativesProcessor]]) does **not**: a body-less value's
    * checking implementation is a native (one of the native processors) — or, for a runtime-only function the checker
    * never reduces, the evaluator's stuck `VNeutral` fallback — never an empty `VTopDef(_, None)` user binding, which
    * would only shadow a reducing native (the `add` bug). The lowering-phase TransparentBinding producer **does**: a
    * runtime native like `nativeWiden` needs an FQN-preserving stuck `VTopDef` so representation lowering can read it
    * back into codegen.
    *
    * Note the gate is on `runtime` (not `selfBody`/`checkingRuntime`): an `opaque` definition like `Int` HAS a body —
    * it is merely kept stuck during checking — so it is a user value either way.
    */
  protected def bindsBodylessValues: Boolean

  override protected def generateFromKeyAndFact(key: OutputKey, fact: InputFact): CompilerIO[OutputFact] =
    if (!bindsBodylessValues && fact.value.runtime.isEmpty) abort
    else generateForBodyfulValue(key, fact)

  private def generateForBodyfulValue(key: OutputKey, fact: InputFact): CompilerIO[OutputFact] = {
    val vfqn = getInputKey(key).vfqn
    val body = selfBody(fact.value)
    generating.add(vfqn)
    for {
      bodyBindings <- body match {
                        case Some(b) => collectBindings(b.value, vfqn)
                        case None    => Map.empty[ValueFQN, SemValue].pure[CompilerIO]
                      }
    } yield {
      generating.remove(vfqn)
      val semValue = VTopDef(
        vfqn,
        body.map { b =>
          Lazy {
            val evaluator = new Evaluator(ref => bodyBindings.get(ref))
            evaluator.eval(Env.empty, reifyingWrap(b.value, fact))
          }
        },
        Spine.SNil
      )
      buildFact(vfqn, semValue)
    }
  }

  /** Wrap a runtime body in [[FunctionLiteral]] binders for the leading type-stack (generic) parameters it reifies —
    * the ones it references in *value* position — so that applying the value's explicit type arguments substitutes
    * them.
    *
    * A type-stack parameter is a binder of the value's '''signature''' (a leading generic `[N]`), but it is '''not''' a
    * lambda of the '''runtime body''': `def bigOf[V] = V` has signature `(V: BigInteger) -> BigInteger` yet runtime
    * body just `V` (a bare reference, the reification of the erased `V`). Evaluated as-is under the empty env, that `V`
    * becomes a free [[SemValue.VNeutral]], so a later `bigOf[1]` only appends `1` to the neutral's spine instead of
    * reducing to `1` — and any computed type argument flowing through such a value (e.g. `h[subtract(N, bigOf[1])]`)
    * stays a stuck term that fails read-back ("Cannot resolve type."). Wrapping the reified binders as leading lambdas
    * makes the spine application substitute them, exactly as the checker does for the value's own monomorphization via
    * `bindTypeStackParam`.
    *
    * Which binders are reified — and the contiguous prefix to wrap (up to and including the last reified one, so
    * explicit-type-argument threading stays positionally aligned) — is the precomputed [[SaturatedValue.binderRoles]]
    * (D6). When nothing is reified the body is returned unchanged, so ordinary generics (e.g. `id[A](x: A) = x`, whose
    * `A` never appears in value position) keep their existing cached shape and implicit-type-argument application is
    * unaffected.
    */
  private def reifyingWrap(
      body: OperatorResolvedExpression,
      fact: SaturatedValue
  ): OperatorResolvedExpression =
    fact.binderRoles.reifiedPrefixBinders
      .foldRight(fact.value.name.as(body)) { (name, accBody) =>
        name.as(OperatorResolvedExpression.FunctionLiteral(name, None, accBody))
      }
      .value

  /** Collect [[NativeBinding]]s for every ValueReference transitively reachable from `ore`, skipping FQNs that are
    * currently being generated (`generating` set) or already accumulated in this walk — this prevents mutual-recursion
    * deadlocks and duplicate fact fetches.
    */
  private def collectBindings(
      ore: OperatorResolvedExpression,
      selfFqn: ValueFQN
  ): CompilerIO[Map[ValueFQN, SemValue]] =
    OperatorResolvedExpression
      .foldValueReferences[CompilerIO, Map[ValueFQN, SemValue]](ore, Map.empty) { (acc, vfqn) =>
        if (acc.contains(vfqn.value) || vfqn.value == selfFqn || generating.contains(vfqn.value))
          acc.pure[CompilerIO]
        else
          getFact(NativeBinding.Key(vfqn.value)).map {
            case Some(binding) => acc + (vfqn.value -> binding.semValue)
            case None          => acc
          }
      }
}
