package com.vanillasource.eliot.eliotc.module.fact

import com.vanillasource.eliot.eliotc.module.fact.QualifiedName
import com.vanillasource.eliot.eliotc.core.fact.NamedValue
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

case class UnifiedModuleValue(
    vfqn: ValueFQN,
    dictionary: Map[QualifiedName, ValueFQN],
    namedValue: NamedValue,
    privateNames: Map[QualifiedName, ValueFQN] = Map.empty,
    platform: Platform = Platform.Runtime
) extends CompilerFact {
  override def key(): CompilerFactKey[UnifiedModuleValue] = UnifiedModuleValue.Key(vfqn, platform)
}

object UnifiedModuleValue {
  case class Key(vfqn: ValueFQN, platform: Platform = Platform.Runtime) extends CompilerFactKey[UnifiedModuleValue]
}
