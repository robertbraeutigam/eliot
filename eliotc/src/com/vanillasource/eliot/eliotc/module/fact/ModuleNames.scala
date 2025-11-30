package com.vanillasource.eliot.eliotc.module.fact

import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}

case class ModuleNames(moduleName: ModuleName, functionNames: Set[String], typeNames: Set[String])
    extends CompilerFact {
  override def key(): CompilerFactKey[ModuleNames] = ModuleNames.Key(moduleName)
}

object ModuleNames {
  case class Key(moduleName: ModuleName) extends CompilerFactKey[ModuleNames]
}
