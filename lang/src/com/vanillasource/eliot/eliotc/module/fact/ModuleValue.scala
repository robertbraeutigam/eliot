package com.vanillasource.eliot.eliotc.module.fact

import com.vanillasource.eliot.eliotc.codec.LangFactCodecs

import com.vanillasource.eliot.eliotc.compiler.cache.codec.FactCodec

import com.vanillasource.eliot.eliotc.module.fact.QualifiedName
import com.vanillasource.eliot.eliotc.core.fact.NamedValue
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

import java.net.URI

case class ModuleValue(
    uri: URI,
    vfqn: ValueFQN,
    dictionary: Map[QualifiedName, ValueFQN],
    namedValue: NamedValue,
    privateNames: Map[QualifiedName, ValueFQN] = Map.empty,
    platform: Platform = Platform.Runtime
) extends CompilerFact {
  override def key(): CompilerFactKey[ModuleValue] = ModuleValue.Key(uri, vfqn, platform)
}

object ModuleValue {
  case class Key(uri: URI, vfqn: ValueFQN, platform: Platform = Platform.Runtime) extends CompilerFactKey[ModuleValue] {
    override def valueCodec: Option[FactCodec[ModuleValue]] = Some(LangFactCodecs.moduleValueCodec)
  }
}
