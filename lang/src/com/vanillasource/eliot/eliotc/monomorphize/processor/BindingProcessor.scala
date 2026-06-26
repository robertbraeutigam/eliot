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
  * The subtle, error-prone parts live here once: the mutual-recursion guard (via the runtime's active fact-request
  * chain, [[com.vanillasource.eliot.eliotc.processor.CompilerIO.activeFactKeys]] — never per-processor mutable state,
  * so the processor stays a pure function of the facts it reads), and the transitive dependency collection.
  * Dependencies are always resolved via [[NativeBinding]] (the checker's semantics, the bindings the bodies were
  * type-checked against); only the top-level value's own body selection differs between phases.
  */
abstract class BindingProcessor[OutputKey <: CompilerFactKey[?]](
    getInputKey: OutputKey => SaturatedValue.Key
)(using ClassTag[OutputKey])
    extends TransformationProcessor[SaturatedValue.Key, OutputKey](getInputKey) {

  /** The value's own body as this phase sees it: [[OperatorResolvedValue.checkingRuntime]] for the checker (opaque
    * bodies stay stuck), [[OperatorResolvedValue.runtime]] for representation lowering (opaque bodies unfold).
    */
  protected def selfBody(fact: OperatorResolvedValue): Option[Sourced[OperatorResolvedExpression]]

  /** Wrap the computed binding in the phase-specific output fact. `binding` is `None` only when the value is body-less
    * and this phase declines body-less values ([[bindsBodylessValues]] = false) — the user contributor's total `None`
    * answer; a phase that binds body-less values never receives `None` and may treat it as unreachable.
    */
  protected def buildFact(vfqn: ValueFQN, binding: Option[SemValue]): CompilerIO[OutputFact]

  /** Whether this processor binds body-less definitions.
    *
    * The checking-phase user contributor ([[UserValueNativesProcessor]]) does **not**: a body-less value's checking
    * implementation is a native (one of the native processors) — or, for a runtime-only function the checker never
    * reduces, the evaluator's stuck `VNeutral` fallback — never an empty `VTopDef(_, None)` user binding, which would
    * only shadow a reducing native (the `add` bug). So it answers `None` for a body-less value (its total
    * [[com.vanillasource.eliot.eliotc.monomorphize.fact.ContributedBinding]] saying "I do not define this"). The
    * lowering-phase TransparentBinding producer **does**: a runtime native like `nativeWiden` needs an FQN-preserving
    * stuck `VTopDef` so representation lowering can read it back into codegen.
    *
    * Note the gate is on `runtime` (not `selfBody`/`checkingRuntime`): an `opaque` definition like `Int` HAS a body —
    * it is merely kept stuck during checking — so it is a user value either way.
    */
  protected def bindsBodylessValues: Boolean

  override protected def generateFromKeyAndFact(key: OutputKey, fact: InputFact): CompilerIO[OutputFact] =
    bindingFor(key, fact).flatMap(buildFact(getInputKey(key).vfqn, _))

  /** `Some(VTopDef …)` for a body-ful value (and for a body-less value when this phase binds them); `None` for a
    * body-less value this phase declines. The body-less `VTopDef` is a stuck `VTopDef(vfqn, None, SNil)`.
    */
  private def bindingFor(key: OutputKey, fact: InputFact): CompilerIO[Option[SemValue]] =
    if (!bindsBodylessValues && fact.value.runtime.isEmpty) none[SemValue].pure[CompilerIO]
    else buildSemValue(getInputKey(key).vfqn, fact).map(_.some)

  private def buildSemValue(vfqn: ValueFQN, fact: InputFact): CompilerIO[SemValue] = {
    val body = selfBody(fact.value)
    for {
      bodyBindings <- body match {
                        case Some(b) => collectBindings(b.value, vfqn)
                        case None    => Map.empty[ValueFQN, SemValue].pure[CompilerIO]
                      }
    } yield VTopDef(
      vfqn,
      body.map { b =>
        Lazy {
          val evaluator = new Evaluator(ref => bodyBindings.get(ref))
          evaluator.eval(Env.empty, reifyingWrap(b.value, fact))
        }
      },
      Spine.SNil
    )
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

  /** Collect [[NativeBinding]]s for every ValueReference transitively reachable from `ore`, skipping FQNs already
    * accumulated in this walk, the value's own FQN (`selfFqn`), and any FQN whose [[NativeBinding]] is already an
    * ancestor on the active fact-request chain
    * ([[com.vanillasource.eliot.eliotc.processor.CompilerIO.activeFactKeys]]). The ancestor check is what breaks
    * mutual-recursion deadlocks (a covariant `data` cycle, a monad-transformer lift): fetching a [[NativeBinding]] that
    * is currently being generated up this chain would block on its not-yet-completed `Deferred`. Using the runtime's
    * per-fiber chain — rather than a shared mutable "currently generating" set — keeps the scope correct (only *this*
    * request's ancestors are skipped, not values merely generating concurrently in an unrelated chain) and keeps the
    * processor pure.
    */
  private def collectBindings(
      ore: OperatorResolvedExpression,
      selfFqn: ValueFQN
  ): CompilerIO[Map[ValueFQN, SemValue]] =
    OperatorResolvedExpression
      .foldValueReferences[CompilerIO, Map[ValueFQN, SemValue]](ore, Map.empty) { (acc, vfqn) =>
        activeFactKeys.flatMap { ancestors =>
          if (acc.contains(vfqn.value) || vfqn.value == selfFqn || ancestors.contains(NativeBinding.Key(vfqn.value)))
            acc.pure[CompilerIO]
          else
            getFact(NativeBinding.Key(vfqn.value)).map {
              case Some(binding) => acc + (vfqn.value -> binding.semValue)
              case None          => acc
            }
        }
      }
}
