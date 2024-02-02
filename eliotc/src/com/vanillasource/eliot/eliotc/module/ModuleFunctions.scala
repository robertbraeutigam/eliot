package com.vanillasource.eliot.eliotc.module

import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}

case class ModuleFunctions(moduleName: ModuleName, functionNames: Set[String]) extends CompilerFact {
  override def key(): CompilerFactKey = ModuleFunctions.Key(moduleName)
}

object ModuleFunctions {
  case class Key(moduleName: ModuleName) extends CompilerFactKey {
    override type FactType = ModuleFunctions
  }
}
