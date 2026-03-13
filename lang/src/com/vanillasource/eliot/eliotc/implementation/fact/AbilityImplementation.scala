package com.vanillasource.eliot.eliotc.implementation.fact

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.symbolic.fact.SymbolicType

case class AbilityImplementation(
    abilityValueFQN: ValueFQN,
    typeArguments: Seq[SymbolicType],
    implementationFQN: ValueFQN
) extends CompilerFact {
  override def key(): CompilerFactKey[AbilityImplementation] =
    AbilityImplementation.Key(abilityValueFQN, typeArguments)
}

object AbilityImplementation {
  case class Key(abilityValueFQN: ValueFQN, typeArguments: Seq[SymbolicType])
      extends CompilerFactKey[AbilityImplementation]
}
