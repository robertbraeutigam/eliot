package com.vanillasource.eliot.eliotc.effect.fact

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** An [[OperatorResolvedValue]] whose body passed the definition-local effect *checks* — the declared-effects subset
  * check (a `{E...}`-carrier body must declare every user-facing effect it performs, which is also what propagates
  * `Inf`) and the "declared pure but performs effects" fail-safe. The body passes through *untransformed*: the effect
  * auto-lift itself (sequencing an effectful sub-term into `Effect.flatMap`/`map`, lifting a pure body with
  * `Effect.pure`) is type-directed elaboration in the NbE checker
  * ([[com.vanillasource.eliot.eliotc.monomorphize.check.EffectLifter]]), where the concrete instantiation decides each
  * slot exactly (docs/effect-lift-in-checker.md).
  *
  * A value failing a check never produces this fact, so it never reaches saturation or monomorphization — fail-safe by
  * construction. Like [[com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue]] this wraps the operator-resolved
  * value rather than rewriting it in place; `SaturatedValueProcessor` consumes this fact in its place.
  *
  * @param value
  *   The operator-resolved value whose body passed the effect checks, carried through untouched.
  */
case class EffectCheckedValue(value: OperatorResolvedValue) extends CompilerFact {
  override def key(): CompilerFactKey[EffectCheckedValue] = EffectCheckedValue.Key(value.vfqn, value.platform)
}

object EffectCheckedValue {
  case class Key(vfqn: ValueFQN, platform: Platform = Platform.Runtime) extends CompilerFactKey[EffectCheckedValue]
}
