package com.vanillasource.eliot.eliotc.implementation.fact

import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

case class AbilityImplementation(
    abilityValueFQN: ValueFQN,
    typeArguments: Seq[ExpressionValue],
    implementationFQN: ValueFQN
) extends CompilerFact {
  override def key(): CompilerFactKey[AbilityImplementation] =
    AbilityImplementation.Key(abilityValueFQN, typeArguments)
}

object AbilityImplementation {
  case class Key(abilityValueFQN: ValueFQN, typeArguments: Seq[ExpressionValue])
      extends CompilerFactKey[AbilityImplementation]
}
