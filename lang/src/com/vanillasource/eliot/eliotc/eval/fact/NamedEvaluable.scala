package com.vanillasource.eliot.eliotc.eval.fact

import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.InitialExpressionValue
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

case class NamedEvaluable(
    vfqn: ValueFQN,
    value: InitialExpressionValue
) extends CompilerFact {
  override def key(): CompilerFactKey[NamedEvaluable] = NamedEvaluable.Key(vfqn)
}

object NamedEvaluable {
  case class Key(vfqn: ValueFQN) extends CompilerFactKey[NamedEvaluable]
}
