package com.vanillasource.eliot.eliotc.module

import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}

case class ModuleFunctionsNames(moduleName: ModuleName, functionNames: Set[String]) extends CompilerFact {
  override def key(): CompilerFactKey = ModuleFunctionsNames.Key(moduleName)
}

object ModuleFunctionsNames {
  case class Key(moduleName: ModuleName) extends CompilerFactKey {
    override type FactType = ModuleFunctionsNames
  }
}
