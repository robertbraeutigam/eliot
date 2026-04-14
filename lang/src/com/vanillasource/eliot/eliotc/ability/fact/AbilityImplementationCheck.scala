package com.vanillasource.eliot.eliotc.ability.fact

import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN

case class AbilityImplementationCheck(abilityFQN: AbilityFQN, typeArguments: Seq[GroundValue]) extends CompilerFact {
  override def key(): CompilerFactKey[AbilityImplementationCheck] =
    AbilityImplementationCheck.Key(abilityFQN, typeArguments)
}

object AbilityImplementationCheck {
  case class Key(abilityFQN: AbilityFQN, typeArguments: Seq[GroundValue])
      extends CompilerFactKey[AbilityImplementationCheck]
}
