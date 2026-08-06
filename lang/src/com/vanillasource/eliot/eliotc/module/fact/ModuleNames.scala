package com.vanillasource.eliot.eliotc.module.fact

import com.vanillasource.eliot.eliotc.codec.LangFactCodecs

import com.vanillasource.eliot.eliotc.compiler.cache.codec.FactCodec

import com.vanillasource.eliot.eliotc.ast.fact.Visibility
import com.vanillasource.eliot.eliotc.module.fact.QualifiedName
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.content.Sourced

import java.io.File
import java.net.URI

case class ModuleNames(uri: URI, names: Sourced[Map[QualifiedName, Visibility]]) extends CompilerFact {
  override def key(): CompilerFactKey[ModuleNames] = ModuleNames.Key(uri)
}

object ModuleNames {
  case class Key(uri: URI) extends CompilerFactKey[ModuleNames] {
    override def valueCodec: Option[FactCodec[ModuleNames]] = Some(LangFactCodecs.moduleNamesCodec)
  }
}
