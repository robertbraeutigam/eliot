package com.vanillasource.eliot.eliotc.module.fact

import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

import java.io.File

case class ModuleNames(file: File, names: Set[String]) extends CompilerFact {
  override def key(): CompilerFactKey[ModuleNames] = ModuleNames.Key(file)
}

object ModuleNames {
  case class Key(file: File) extends CompilerFactKey[ModuleNames]
}
