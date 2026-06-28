package com.vanillasource.eliot.eliotc.ability.fact

import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN

/** Completeness/signature check of an ability's implementation, scoped to the same source [[Platform]] as the
  * [[AbilityImplementation]] it gates (effectful-signatures W2a): a compiler-pool-only instance must be checked against
  * the compiler pool's ability methods and markers, not the (instance-free) runtime pool. */
case class AbilityImplementationCheck(
    abilityFQN: AbilityFQN,
    typeArguments: Seq[GroundValue],
    platform: Platform = Platform.Runtime
) extends CompilerFact {
  override def key(): CompilerFactKey[AbilityImplementationCheck] =
    AbilityImplementationCheck.Key(abilityFQN, typeArguments, platform)
}

object AbilityImplementationCheck {
  case class Key(abilityFQN: AbilityFQN, typeArguments: Seq[GroundValue], platform: Platform = Platform.Runtime)
      extends CompilerFactKey[AbilityImplementationCheck]
}
