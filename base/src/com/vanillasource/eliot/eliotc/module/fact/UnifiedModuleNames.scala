package com.vanillasource.eliot.eliotc.module.fact

import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

case class UnifiedModuleNames(moduleName: ModuleName, names: Set[String]) extends CompilerFact {
  override def key(): CompilerFactKey[UnifiedModuleNames] = UnifiedModuleNames.Key(moduleName)
}

object UnifiedModuleNames {
  case class Key(moduleName: ModuleName) extends CompilerFactKey[UnifiedModuleNames]
}
