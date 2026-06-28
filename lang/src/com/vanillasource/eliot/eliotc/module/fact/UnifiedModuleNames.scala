package com.vanillasource.eliot.eliotc.module.fact

import com.vanillasource.eliot.eliotc.ast.fact.Visibility
import com.vanillasource.eliot.eliotc.module.fact.QualifiedName
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

case class UnifiedModuleNames(
    moduleName: ModuleName,
    names: Map[QualifiedName, Visibility],
    platform: Platform = Platform.Runtime
) extends CompilerFact {
  override def key(): CompilerFactKey[UnifiedModuleNames] = UnifiedModuleNames.Key(moduleName, platform)
}

object UnifiedModuleNames {
  case class Key(moduleName: ModuleName, platform: Platform = Platform.Runtime)
      extends CompilerFactKey[UnifiedModuleNames]
}
