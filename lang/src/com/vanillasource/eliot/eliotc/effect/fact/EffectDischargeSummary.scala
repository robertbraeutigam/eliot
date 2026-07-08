package com.vanillasource.eliot.eliotc.effect.fact

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN

/** The effects a value *discharges* — the discharge-aware effect accounting summary
  * (docs/effect-discharge-accounting.md, Step 3). Unifies the two producers of a discharge:
  *
  *   - **declared** — the negative `{…, -E}` members annotated on the signature (Step 1), carried on
  *     [[com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue.dischargedEffects]]; the only source for an
  *     abstract discharger primitive (`else`, `catch`, `runStateTo…`), which has no body to analyse.
  *   - **inferred** — for a value *with* a body, the effects that entered through its carrier-typed parameters and did
  *     not survive the body (were discharged by some inner discharger). This is what lets a user-defined handler like
  *     `def orDefault(x: {Abort} A, d) = x else d` discharge `Abort` with no annotation.
  *
  * [[com.vanillasource.eliot.eliotc.effect.processor.CalleeSignatures]] reads this fact so a caller subtracts a
  * callee's discharge — declared and inferred alike — from its own effect accounting. Produced once per value by the
  * single [[com.vanillasource.eliot.eliotc.effect.processor.EffectDischargeSummaryProcessor]], which reads back the
  * summaries of the value's own callees; because Eliot has no recursion the runtime-body call graph is a DAG, so this
  * inherent recursion terminates (a payoff of the totality cornerstone) — no per-processor cycle.
  *
  * @param dischargedEffects
  *   The abilities this value discharges, at ability granularity (matching the definition-local check; per-argument
  *   precision is left to monomorphization at the use site).
  */
case class EffectDischargeSummary(
    vfqn: ValueFQN,
    platform: Platform,
    dischargedEffects: Set[AbilityFQN]
) extends CompilerFact {
  override def key(): CompilerFactKey[EffectDischargeSummary] = EffectDischargeSummary.Key(vfqn, platform)
}

object EffectDischargeSummary {
  case class Key(vfqn: ValueFQN, platform: Platform = Platform.Runtime)
      extends CompilerFactKey[EffectDischargeSummary]
}
