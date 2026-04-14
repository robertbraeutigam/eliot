package com.vanillasource.eliot.eliotc.ability.fact

import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

case class AbilityImplementation(
    abilityValueFQN: ValueFQN,
    typeArguments: Seq[GroundValue],
    implementationFQN: ValueFQN,
    implementationTypeArgs: Seq[GroundValue] = Seq.empty
) extends CompilerFact {
  override def key(): CompilerFactKey[AbilityImplementation] =
    AbilityImplementation.Key(abilityValueFQN, typeArguments)
}

object AbilityImplementation {
  case class Key(abilityValueFQN: ValueFQN, typeArguments: Seq[GroundValue])
      extends CompilerFactKey[AbilityImplementation]
}
