package com.vanillasource.eliot.eliotc.implementation.fact

import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

case class AbilityImplementation(
    abstractFunctionFQN: ValueFQN,
    typeArguments: Seq[Value],
    implementationFQN: ValueFQN
) extends CompilerFact {
  override def key(): CompilerFactKey[AbilityImplementation] =
    AbilityImplementation.Key(abstractFunctionFQN, typeArguments)
}

object AbilityImplementation {
  case class Key(abstractFunctionFQN: ValueFQN, typeArguments: Seq[Value])
      extends CompilerFactKey[AbilityImplementation]
}
