package com.vanillasource.eliot.eliotc.module.fact

import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}

import java.io.File

case class ModuleNames(file: File, functionNames: Set[String], typeNames: Set[String]) extends CompilerFact {
  override def key(): CompilerFactKey[ModuleNames] = ModuleNames.Key(file)
}

object ModuleNames {
  case class Key(file: File) extends CompilerFactKey[ModuleNames]
}
