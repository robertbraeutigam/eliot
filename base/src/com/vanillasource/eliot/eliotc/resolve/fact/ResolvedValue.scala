package com.vanillasource.eliot.eliotc.resolve.fact

import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.content.Sourced

case class ResolvedValue(
    vfqn: ValueFQN,
    name: Sourced[String],
    runtime: Option[Sourced[Expression]],
    typeStack: Sourced[TypeStack[Expression]]
) extends CompilerFact {
  override def key(): CompilerFactKey[ResolvedValue] = ResolvedValue.Key(vfqn)
}

object ResolvedValue {
  case class Key(vfqn: ValueFQN) extends CompilerFactKey[ResolvedValue]
}
