package com.vanillasource.eliot.eliotc.effect.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.effect.fact.EffectDischargeSummary
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.termination.fact.RecursionCheckedValue

/** Produces the [[EffectDischargeSummary]] for each value — the discharge-aware effect accounting's inference half
  * (docs/effect-discharge-accounting.md, Step 3). The summary unifies:
  *
  *   - the value's **declared** discharge (its `{…, -E}` negative members, carried on
  *     [[OperatorResolvedValue.dischargedEffects]]) — the only source for a body-less discharger primitive; and
  *   - for a value *with* a body, the **inferred** discharge: run the [[EffectUsageCollector]] over the body and take
  *     the effects that entered through the value's carrier-typed parameters ([[EffectAccounting.paramEffects]]) but did
  *     not survive to the result (`survivingParamEffects`). An effect survives iff some undischarged occurrence remains,
  *     so an effect *not* surviving means every occurrence was discharged inside the body — the value discharges it.
  *
  * The collector's per-call subtraction reads back *callee* summaries via [[CalleeSignatures.infoFor]], so this
  * processor reads [[EffectDischargeSummary]] for the value's own callees. That inherent recursion is the single-owner
  * pattern (one fact, one processor); it terminates because Eliot forbids recursion, so the runtime-body call graph a
  * value's summary walks is a DAG (a rejected recursive callee simply produces no [[RecursionCheckedValue]], hence no
  * summary — read conservatively as no discharge). No `activeFactKeys` guard is therefore needed.
  *
  * Kept **separate from [[EffectCheckProcessor]]** deliberately: the summary is a signature-level property callers need
  * even when the value's own body fails the effect check (e.g. a fully-discharging body under a not-yet-reconciled pure
  * return — Step 6), so it must not be gated on the check passing.
  */
class EffectDischargeSummaryProcessor
    extends TransformationProcessor[RecursionCheckedValue.Key, EffectDischargeSummary.Key](key =>
      RecursionCheckedValue.Key(key.vfqn, key.platform)
    )
    with Logging {

  private lazy val calleeSignatures = new CalleeSignatures
  private lazy val collector        = new EffectUsageCollector(calleeSignatures)

  override protected def generateFromKeyAndFact(
      key: EffectDischargeSummary.Key,
      checked: RecursionCheckedValue
  ): CompilerIO[EffectDischargeSummary] = {
    given Platform = checked.value.platform
    val value      = checked.value
    val declared   = value.dischargedEffects.toSet
    value.runtime match {
      case None       => EffectDischargeSummary(value.vfqn, value.platform, declared).pure[CompilerIO]
      case Some(body) =>
        inferDischarge(value, body).map(inferred =>
          EffectDischargeSummary(value.vfqn, value.platform, declared ++ inferred)
        )
    }
  }

  /** The effects the value discharges by construction of its body: its carrier-typed parameters' effects minus those
    * that survived to the body's result.
    */
  private def inferDischarge(
      value: OperatorResolvedValue,
      body: Sourced[OperatorResolvedExpression]
  )(using Platform): CompilerIO[Set[AbilityFQN]] = {
    val context = EffectAccounting.bodyContext(value, body)
    collector
      .collect(context.inner, context.env, context.carrierEffects)
      .map(usage => EffectAccounting.paramEffects(context) -- usage.survivingParamEffects)
  }
}
