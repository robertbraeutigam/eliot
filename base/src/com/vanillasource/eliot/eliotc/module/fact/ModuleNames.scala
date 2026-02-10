package com.vanillasource.eliot.eliotc.module.fact

import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

import java.io.File
import java.net.URI

case class ModuleNames(uri: URI, names: Set[String]) extends CompilerFact {
  override def key(): CompilerFactKey[ModuleNames] = ModuleNames.Key(uri)
}

object ModuleNames {
  case class Key(uri: URI) extends CompilerFactKey[ModuleNames]
}
