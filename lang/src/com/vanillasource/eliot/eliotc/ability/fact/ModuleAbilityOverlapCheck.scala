package com.vanillasource.eliot.eliotc.ability.fact

import com.vanillasource.eliot.eliotc.module.fact.ModuleName
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** Marker fact produced once per (module, ability-name) pair after verifying that no two implementations of
  * `abilityName` in `moduleName` have structurally-overlapping patterns.
  *
  * The check runs at definition time (i.e. as soon as any call site triggers ability resolution for this ability,
  * before the call's specific type arguments are considered), so overlap errors point at the offending implementations
  * rather than at arbitrary call sites that happen to expose the ambiguity.
  *
  * Cross-module overlap is still detected at call time by the ambiguity check in `AbilityImplementationProcessor`.
  *
  * Scoped to a source [[Platform]] (effectful-signatures W2a) so the per-module overlap scan runs in the same pool as
  * the resolution that triggered it; defaults to [[Platform.Runtime]].
  */
case class ModuleAbilityOverlapCheck(
    moduleName: ModuleName,
    abilityName: String,
    platform: Platform = Platform.Runtime
) extends CompilerFact {
  override def key(): CompilerFactKey[ModuleAbilityOverlapCheck] =
    ModuleAbilityOverlapCheck.Key(moduleName, abilityName, platform)
}

object ModuleAbilityOverlapCheck {
  case class Key(moduleName: ModuleName, abilityName: String, platform: Platform = Platform.Runtime)
      extends CompilerFactKey[ModuleAbilityOverlapCheck]
}
