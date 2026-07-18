package com.vanillasource.eliot.eliotc.effect.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.effect.fact.EffectCheckedValue
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.{arrow, spine}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import com.vanillasource.eliot.eliotc.termination.fact.RecursionCheckedValue

/** Effects, Part B — the definition-local effect *verification* phase. The body auto-lift itself (rewriting
  * direct-style `printLine(readLine)` into monadic form) is *not* here: it is type-directed elaboration in the NbE
  * checker ([[com.vanillasource.eliot.eliotc.monomorphize.check.EffectLifter]]), where each argument slot is decided at
  * the concrete instantiation (docs/effect-lift-in-checker.md). This phase keeps the two diagnostics that are
  * definition-local and signature-derivable — sequencing never changes *which* effects a body performs:
  *
  *   - the **declared-effects subset check** ([[DeclaredEffectChecker]]): a `{E...}`-carrier body must declare every
  *     user-facing effect it performs — which is also what propagates `Inf` to callers;
  *   - the **"declared pure but result is effectful" fail-safe** ([[purelyDeclaredMessage]]): a value with a
  *     nullary/pure return (`Unit`, `String`) whose body performs a *genuine, undischarged* effect, or whose residual
  *     result rides a *caller-chosen ambient* carrier, is rejected here with a friendly message rather than a raw
  *     monomorphization mismatch. A *fully discharged* residual on a value with no ambient carrier is **not** an error
  *     anymore: it proceeds to the checker, whose pure-boundary Id defaulting
  *     ([[com.vanillasource.eliot.eliotc.monomorphize.check.EffectLifter.tryIdDefault]]) solves the residual carrier to
  *     the identity carrier `Id` and unwraps it with `runId` — so `def sign(f: Bool): String = if(f, "+") else "-"`
  *     compiles. A residual that cannot resolve at `Id` (a mismatching payload) still hard-errors at monomorphization
  *     — deferred, never silent.
  *
  * Both are fed by one [[EffectUsageCollector]] walk (the accounting half of the former desugarer). The body passes
  * through untransformed; a value failing a check never produces its [[EffectCheckedValue]], so it never reaches
  * saturation — fail-safe by construction.
  *
  * Placed after `RecursionCheckProcessor` (its sole input — so only recursion-free values reach the checks; the
  * no-recursion gate is upstream) and before `SaturatedValueProcessor` (whose primary input is keyed to this fact);
  * the value is untouched, so saturate's cross-value reads of other [[OperatorResolvedValue]] signatures stay valid.
  */
class EffectCheckProcessor
    extends TransformationProcessor[RecursionCheckedValue.Key, EffectCheckedValue.Key](key =>
      RecursionCheckedValue.Key(key.vfqn, key.platform)
    )
    with Logging {

  private lazy val calleeSignatures = new CalleeSignatures
  private lazy val collector        = new EffectUsageCollector(calleeSignatures)
  private lazy val effectChecker    = new DeclaredEffectChecker

  override protected def generateFromKeyAndFact(
      key: EffectCheckedValue.Key,
      checked: RecursionCheckedValue
  ): CompilerIO[EffectCheckedValue] = {
    given Platform = checked.value.platform
    val value      = checked.value
    value.runtime
      .traverse_(body => checkBody(value, body))
      .as(EffectCheckedValue(value))
  }

  /** Run the two effect diagnostics over one value's body against its declared signature. An error registered here
    * keeps the fact from registering (`registerFactIfClear`), gating the value out of the downstream pipeline.
    */
  private def checkBody(
      value: OperatorResolvedValue,
      body: Sourced[OperatorResolvedExpression]
  )(using Platform): CompilerIO[Unit] = {
    // Shared preparation: the ambient carrier binders (a higher-kinded binder that carries an ability constraint — the
    // M1 `{E...}` carrier or a hand-written `[F[_] ~ Effect]`; a bare `C[_, _]` is NOT a carrier), the value
    // parameters bound to their types, and the inner body with those parameter lambdas peeled.
    val context         = EffectAccounting.bodyContext(value, body)
    val carrier         = context.carrierEffects.keySet

    // The number of value parameters is the count of leading lambdas the *body* actually has, NOT the arrow-arity of
    // the declared type: a function-*valued* def (`def f : Function[A, B] = g`) has 0 parameters but a 2-arrow type, so
    // `SignatureView` would over-decompose its return to `B`. The effective return the inner body must produce is the
    // declared type with only the peeled parameters' arrows removed.
    val effectiveReturn =
      context.view.parameters
        .drop(context.env.size)
        .foldRight(context.view.returnType)((dom, cod) => cod.as(arrow(dom, cod)))
    // A nullary return type (`Unit`, `String`) cannot host effects; an applied one (`F[Unit]`, `IO[Unit]`, `Box[A]`)
    // can — only an effectful body in the former case is the "declared pure" error.
    val canHostEffects  = spine(effectiveReturn.value)._2.nonEmpty

    for {
      usage <- collector.collect(context.inner, context.env, context.carrierEffects)
      _     <- effectChecker.verify(value, carrier, usage.usedEffects)
      _     <- if (usage.effectful && !canHostEffects && (usage.usedEffects.nonEmpty || carrier.nonEmpty))
                 compilerError(value.name.as(purelyDeclaredMessage(usage)))
               else ().pure[CompilerIO]
    } yield ()
  }

  /** The "declared pure but result is effectful" message, **discharge- and Id-aware** (Step 6, updated by the
    * Id-carrier work). The check fires for an effectful result under a pure return only when the checker's
    * pure-boundary Id defaulting could never legitimize it:
    *
    *   - a body performing a *genuine, undischarged* effect (`usedEffects` non-empty — `printLine(readLine)` under a
    *     `String` return) is told it performs one; whereas
    *   - a value with an *ambient carrier binder* (a `{E...}`-typed parameter — a handler like
    *     `def getOr(x: {Abort} String, d: String): String = x else d`) whose residual result rides that caller-chosen
    *     carrier is told its result rides a carrier the pure return cannot be: the carrier is fixed by each caller, so
    *     it can never default to `Id`, and letting it through would only trade this friendly definition-site error for
    *     an opaque quote error at the use site.
    *
    * A fully-discharged residual on a value with NO ambient carrier (`usedEffects` empty, no carrier binder — a
    * discharged `if(f,"A") else "B"` under a bare `String` return) is deliberately not an error: the checker's Id
    * defaulting ([[com.vanillasource.eliot.eliotc.monomorphize.check.EffectLifter.tryIdDefault]]) solves the residual
    * carrier to the identity carrier and unwraps it with `runId`.
    */
  private def purelyDeclaredMessage(usage: EffectUsageCollector.Usage): String =
    if (usage.usedEffects.nonEmpty)
      "This value performs an effect but is declared pure; declare an effect set with { ... } or return an effect carrier."
    else
      "This value's result rides an effect carrier but its declared return type is pure; return an effect carrier such " +
        "as IO[...] instead."
}
