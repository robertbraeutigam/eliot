package com.vanillasource.eliot.eliotc.effect.processor

import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.{
  ParameterReference,
  SignatureView,
  asArrow,
  spine
}

/** Identifying effect carriers. An effect carrier is a value's higher-kinded generic binder (`F` in `[F[_] ~ E...]`,
  * the shape the M1 `{E...}` sugar produces) that an effectful result rides in (`F[String]`, `IO[Unit]`). The two
  * predicates here are the structural test the rest of the pass uses to find carriers and to decide whether a type is
  * effectful (carrier-headed).
  */
object EffectCarriers {

  /** A generic binder is a carrier iff its kind is an arrow (`Type -> Type`, i.e. higher-kinded). */
  def isHktBinder(binder: SignatureView.Binder): Boolean =
    binder.parameterType.exists(pt => asArrow(pt.value.signature).isDefined)

  /** Whether a type expression is headed by one of `carrier` (a carrier-typed value, e.g. `F[String]`). */
  def carrierHeaded(tpe: OperatorResolvedExpression, carrier: Set[String]): Boolean =
    spine(tpe)._1 match {
      case ParameterReference(n) => carrier.contains(n.value)
      case _                     => false
    }
}
