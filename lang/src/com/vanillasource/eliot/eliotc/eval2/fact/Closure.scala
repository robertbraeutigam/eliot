package com.vanillasource.eliot.eliotc.eval2.fact

import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression

/** A closure capturing an environment and an unevaluated body. The body is re-evaluated
  * when the closure is applied.
  */
case class Closure(env: Env, body: OperatorResolvedExpression)
