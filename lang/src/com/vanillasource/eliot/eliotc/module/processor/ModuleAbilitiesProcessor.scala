package com.vanillasource.eliot.eliotc.module.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{
  ModuleAbilities,
  QualifiedName,
  Qualifier,
  UnifiedModuleNames,
  ValueFQN
}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor

/** Builds [[ModuleAbilities]] for a module by decoding the `Qualifier.Ability` (declared ability methods) and
  * `Qualifier.AbilityImplementation` (implementation methods) names in its [[UnifiedModuleNames]] into structured
  * records. The one place that knows the ability-marker name encoding.
  */
class ModuleAbilitiesProcessor extends SingleFactProcessor[ModuleAbilities.Key] {

  override protected def generateSingleFact(key: ModuleAbilities.Key): CompilerIO[ModuleAbilities] =
    getFactOrAbort(UnifiedModuleNames.Key(key.moduleName, key.platform)).map { names =>
      val keys            = names.names.keys.toSeq
      val declaredMethods = keys.collect { case qn @ QualifiedName(_, Qualifier.Ability(abilityName)) =>
        ModuleAbilities.DeclaredMethod(ValueFQN(key.moduleName, qn), abilityName)
      }
      val implementations = keys.collect {
        case qn @ QualifiedName(methodName, Qualifier.AbilityImplementation(abilityName, pattern)) =>
          ModuleAbilities.Impl(ValueFQN(key.moduleName, qn), methodName, abilityName, pattern)
      }
      ModuleAbilities(key.moduleName, declaredMethods, implementations, key.platform)
    }
}
