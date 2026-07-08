package com.vanillasource.eliot.eliotc.effect.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.{FunctionLiteral, SignatureView}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Shared body-analysis setup for the effect phase's two collector consumers — the definition-local check
  * ([[EffectCheckProcessor]]) and the discharge-inference summary ([[EffectDischargeSummaryProcessor]]). Both peel a
  * value's leading value-parameter lambdas into an `env`, read its ambient carrier binders' declared effects, and run
  * the [[EffectUsageCollector]] over the inner body; this object holds that common preparation so the two stay in step.
  */
object EffectAccounting {

  /** The prepared collector inputs for one value's body: the signature view, the ambient carrier binders mapped to
    * their declared effects (`carrierEffects`, whose key-set is the value's carrier binder names), the value
    * parameters bound to their declared types (`env`), and the inner body with those parameter lambdas peeled off.
    */
  case class BodyContext(
      view: SignatureView,
      carrierEffects: Map[String, Set[AbilityFQN]],
      env: Map[String, OperatorResolvedExpression],
      inner: Sourced[OperatorResolvedExpression]
  )

  def bodyContext(value: OperatorResolvedValue, body: Sourced[OperatorResolvedExpression]): BodyContext = {
    val view                = SignatureView.of(value.typeStack.as(value.typeStack.value.signature))
    // The value's own ambient effect carrier(s): a higher-kinded binder that carries an ability constraint (the M1
    // `{E...}` carrier or a hand-written `[F[_] ~ Effect]`), each mapped to the (non-machinery) effects it declares.
    val carrierEffects      = EffectCarriers
      .carrierBinders(view)
      .filter(value.paramConstraints.contains)
      .map(binder => binder -> EffectCarriers.declaredEffects(Set(binder), value.paramConstraints))
      .toMap
    val (paramNames, inner) = peelParameters(body, view.parameters.size)
    val env                 = paramNames.map(_.value).zip(view.parameters.map(_.value)).toMap
    BodyContext(view, carrierEffects, env, inner)
  }

  /** The effects that enter the value through its carrier-typed parameters — each parameter headed by an ambient
    * carrier binder contributes that binder's declared effects. The discharge inference subtracts the parameters'
    * effects that *survived* the body from this set.
    */
  def paramEffects(context: BodyContext): Set[AbilityFQN] =
    context.env.values
      .flatMap(tpe => EffectCarriers.carrierHead(tpe, context.carrierEffects.keySet).toSet.flatMap(context.carrierEffects))
      .toSet

  /** Peel `count` leading value-parameter [[FunctionLiteral]]s off a body, returning their names and the inner body. */
  def peelParameters(
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
}
