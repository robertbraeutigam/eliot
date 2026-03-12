package com.vanillasource.eliot.eliotc.implementation.fact

import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import com.vanillasource.eliot.eliotc.symbolic.types.SymbolicType

case class AbilityImplementationCheck(abilityFQN: AbilityFQN, typeArguments: Seq[SymbolicType])
    extends CompilerFact {
  override def key(): CompilerFactKey[AbilityImplementationCheck] =
    AbilityImplementationCheck.Key(abilityFQN, typeArguments)
}

object AbilityImplementationCheck {
  case class Key(abilityFQN: AbilityFQN, typeArguments: Seq[SymbolicType])
      extends CompilerFactKey[AbilityImplementationCheck]
}
