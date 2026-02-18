package com.vanillasource.eliot.eliotc.implementation.fact

import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN

case class AbilityImplementationCheck(abilityFQN: AbilityFQN, typeArguments: Seq[Value]) extends CompilerFact {
  override def key(): CompilerFactKey[AbilityImplementationCheck] =
    AbilityImplementationCheck.Key(abilityFQN, typeArguments)
}

object AbilityImplementationCheck {
  case class Key(abilityFQN: AbilityFQN, typeArguments: Seq[Value])
      extends CompilerFactKey[AbilityImplementationCheck]
}
