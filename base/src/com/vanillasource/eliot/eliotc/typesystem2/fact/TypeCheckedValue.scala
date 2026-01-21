package com.vanillasource.eliot.eliotc.typesystem2.fact

import com.vanillasource.eliot.eliotc.core.fact.ExpressionStack
import com.vanillasource.eliot.eliotc.module2.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.content.Sourced

case class TypeCheckedValue(
    vfqn: ValueFQN,
    name: Sourced[String],
    value: Sourced[ExpressionStack[TypedExpression]]
) extends CompilerFact {
  override def key(): CompilerFactKey[TypeCheckedValue] = TypeCheckedValue.Key(vfqn)
}

object TypeCheckedValue {
  case class Key(vfqn: ValueFQN) extends CompilerFactKey[TypeCheckedValue]
}
