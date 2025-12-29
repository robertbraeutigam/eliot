package com.vanillasource.eliot.eliotc.module.fact

import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

import java.io.File

case class UnifiedModuleNames(moduleName: ModuleName, functionNames: Set[String], typeNames: Set[String])
    extends CompilerFact {
  override def key(): CompilerFactKey[UnifiedModuleNames] = UnifiedModuleNames.Key(moduleName)
}

object UnifiedModuleNames {
  case class Key(moduleName: ModuleName) extends CompilerFactKey[UnifiedModuleNames]
}
