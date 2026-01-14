package com.vanillasource.eliot.eliotc.resolve2.fact

import com.vanillasource.eliot.eliotc.module2.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.content.Sourced

case class ResolvedValue(
    vfqn: ValueFQN,
    name: Sourced[String],
    typeExpression: Sourced[ExpressionStack],
    value: Option[Sourced[Expression]]
) extends CompilerFact {
  override def key(): CompilerFactKey[ResolvedValue] = ResolvedValue.Key(vfqn)
}

object ResolvedValue {
  case class Key(vfqn: ValueFQN) extends CompilerFactKey[ResolvedValue]
}
