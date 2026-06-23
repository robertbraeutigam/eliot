package com.vanillasource.eliot.eliotc.effect.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.effect.fact.EffectDesugaredValue
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.{FunctionLiteral, SignatureView, arrow, spine}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

/** Effects, Part B — the body auto-lift (the headline of the effects plan, M3).
  *
  * Rewrites a value's runtime body from direct style into ordinary monadic form, so a developer writes
  * `def echo : {Console} Unit = println(readLine)` and the compiler produces `flatMap(readLine, x -> println(x))`. The
  * single rule: an effectful sub-term (one whose static result is headed by an effect carrier) flowing into a *pure
  * value-argument position* is sequenced with the carrier's `Monad.flatMap` (or `Applicative.map`, when the
  * continuation is pure), binding it to a fresh variable; a pure body under an effectful carrier return is wrapped with
  * `Monad.pure`. There is no `lift`: every effectful sub-term shares the one carrier, so there are never layers to
  * cross inside a body.
  *
  * This processor is the per-value orchestrator; the work is split across collaborators:
  *   - [[EffectCarriers]] / [[EffectMachinery]] — pure helpers identifying carriers and constructing/recognising the
  *     internal `Monad`/`Applicative` machinery;
  *   - [[CalleeSignatures]] — reads a callee's signature into what the lift needs (bind positions, effectfulness);
  *   - [[DirectStyleDesugarer]] — the recursive bottom-up rewrite itself;
  *   - [[DeclaredEffectChecker]] — the declared-effects subset (propagation) check.
  *
  * Fail-safe: a value with no effect carrier (a nullary return such as `Unit`) whose body nevertheless performs an
  * effect is rejected here ("performs effects but its return type cannot carry them") rather than miscompiled;
  * monomorphization would also catch it as an `F[A]`-vs-`A` mismatch.
  *
  * Placed after `OperatorResolverProcessor` (so application structure is final) and before `SaturatedValueProcessor`
  * (whose primary input is repointed to this fact); the signature is untouched, so saturate's cross-value reads of
  * other [[OperatorResolvedValue]] signatures stay valid.
  */
class EffectDesugaringProcessor
    extends TransformationProcessor[OperatorResolvedValue.Key, EffectDesugaredValue.Key](key =>
      OperatorResolvedValue.Key(key.vfqn)
    )
    with Logging {

  private lazy val calleeSignatures = new CalleeSignatures
  private lazy val desugarer        = new DirectStyleDesugarer(calleeSignatures)
  private lazy val effectChecker    = new DeclaredEffectChecker
  private lazy val recursionChecker = new RecursionChecker

  override protected def generateFromKeyAndFact(
      key: EffectDesugaredValue.Key,
      value: OperatorResolvedValue
  ): CompilerIO[EffectDesugaredValue] =
    // Gate every compiled value on the no-recursion rule (termination M1) before desugaring: a recursive definition is
    // reported here and its body is left untouched, so the registered error trips `registerFactIfClear` and the value
    // never reaches monomorphization or codegen (where it would silently compile to a self-calling method).
    recursionChecker.isRecursive(value).flatMap {
      case true  => EffectDesugaredValue(value).pure[CompilerIO]
      case false =>
        value.runtime match {
          case None       => EffectDesugaredValue(value).pure[CompilerIO]
          case Some(body) =>
            desugarBody(value, body).map(runtime => EffectDesugaredValue(value.copy(runtime = runtime)))
        }
    }

  /** Auto-lift one value's body against its declared signature, returning the rewritten runtime (or registering a
    * fail-safe error and returning the original, which then will not be registered downstream).
    */
  private def desugarBody(
      value: OperatorResolvedValue,
      body: Sourced[OperatorResolvedExpression]
  ): CompilerIO[Option[Sourced[OperatorResolvedExpression]]] = {
    val view                  = SignatureView.of(value.typeStack.as(value.typeStack.value.signature))
    // The value's own ambient effect carrier(s): a higher-kinded binder that carries an ability constraint — the M1
    // `{E...}` carrier (`[F[_] ~ E...]`) or a hand-written `[F[_] ~ Monad]`. A bare higher-kinded generic (`C[_, _]`
    // in `f[A, B, C[_, _]]`) is NOT a carrier, so a body like `id(c)` is never spuriously `pure`-wrapped.
    val carrier               = EffectCarriers.carrierBinders(view).filter(value.paramConstraints.contains)

    // The number of value parameters is the count of leading lambdas the *body* actually has, NOT the arrow-arity of
    // the declared type: a function-*valued* def (`def f : Function[A, B] = g`) has 0 parameters but a 2-arrow type, so
    // `SignatureView` would over-decompose its return to `B`. The effective return the inner body must produce is the
    // declared type with only the peeled parameters' arrows removed. The body's leading value-parameter lambdas are
    // unannotated (the type stack is the single source of truth), so each peeled parameter's type comes from the view.
    val (paramNames, inner)   = peelParameters(body, view.parameters.size)
    val env                   = paramNames.map(_.value).zip(view.parameters.map(_.value)).toMap
    val effectiveReturn       =
      view.parameters.drop(paramNames.size).foldRight(view.returnType)((dom, cod) => cod.as(arrow(dom, cod)))
    val returnIsCarrierBinder = EffectCarriers.carrierHeaded(effectiveReturn.value, carrier)
    // A nullary return type (`Unit`, `String`) cannot host effects; an applied one (`F[Unit]`, `IO[Unit]`, `Box[A]`)
    // can — only an effectful body in the former case is the "declared pure" error.
    val canHostEffects        = spine(effectiveReturn.value)._2.nonEmpty

    for {
      d          <- desugarer.desugar(inner, env, carrier)
      _          <- effectChecker.verify(value, carrier, d.usedEffects)
      finalInner <-
        if (d.effectful && !canHostEffects)
          compilerError(
            value.name.as(
              "This value performs an effect but is declared pure; declare an effect set with { ... } or return an " +
                "effect carrier."
            )
          ).as(d.expr)
        else if (!d.effectful && returnIsCarrierBinder) EffectMachinery.pureWrap(d.expr).pure[CompilerIO]
        else d.expr.pure[CompilerIO]
    } yield Some(rewrapParameters(body, paramNames, finalInner))
  }

  /** Peel `count` leading value-parameter [[FunctionLiteral]]s off a body, returning their names and the inner body. */
  private def peelParameters(
      body: Sourced[OperatorResolvedExpression],
      count: Int
  ): (Seq[Sourced[String]], Sourced[OperatorResolvedExpression]) =
    if (count <= 0) (Seq.empty, body)
    else
      body.value match {
        case FunctionLiteral(name, _, innerBody) =>
          val (rest, inner) = peelParameters(innerBody, count - 1)
          (name +: rest, inner)
        case _                                   => (Seq.empty, body)
      }

  /** Re-attach the peeled value-parameter lambdas (unannotated, as they were) around a rewritten inner body. */
  private def rewrapParameters(
      original: Sourced[OperatorResolvedExpression],
      paramNames: Seq[Sourced[String]],
      inner: Sourced[OperatorResolvedExpression]
  ): Sourced[OperatorResolvedExpression] =
    paramNames.foldRight(inner)((name, acc) => original.as(FunctionLiteral(name, None, acc)))
}
