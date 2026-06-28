package com.vanillasource.eliot.eliotc.ability.fact

import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** The resolved ability implementation for `abilityValueFQN` at `typeArguments`, scoped to a source [[Platform]]: the
  * implementation (and its supporting markers/values) is searched in that platform's source pool. [[Platform.Runtime]]
  * is the default — every existing ability resolves under it — while the **compiler** pool carries the compile-time
  * carrier's `Monad`/`Throw[Either[String]]` instances, reachable only by querying under [[Platform.Compiler]] (the
  * ability-instance analogue of CP3's `CompilerNativesProcessor` for value bodies; effectful-signatures W2a).
  */
case class AbilityImplementation(
    abilityValueFQN: ValueFQN,
    typeArguments: Seq[GroundValue],
    implementationFQN: ValueFQN,
    implementationTypeArgs: Seq[GroundValue] = Seq.empty,
    platform: Platform = Platform.Runtime
) extends CompilerFact {
  override def key(): CompilerFactKey[AbilityImplementation] =
    AbilityImplementation.Key(abilityValueFQN, typeArguments, platform)
}

object AbilityImplementation {
  case class Key(
      abilityValueFQN: ValueFQN,
      typeArguments: Seq[GroundValue],
      platform: Platform = Platform.Runtime
  ) extends CompilerFactKey[AbilityImplementation]
}
