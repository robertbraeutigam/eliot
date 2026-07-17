package com.vanillasource.eliot.eliotc.source.scan

import com.vanillasource.eliot.eliotc.module.fact.ModuleName
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** The deduplicated set of every module a platform pool serves — the whole-pool module universe that demand-driven
  * resolution (`resolve` one path at a time) never materializes. Produced by [[PoolModulesProcessor]] from the
  * enumeration of the pool's mounts and keyed by [[Platform]] (the runtime pool for the `namedValues` reflection use;
  * the compiler pool for a compile-time use later).
  */
case class PoolModules(platform: Platform, modules: Set[ModuleName]) extends CompilerFact {
  override def key(): CompilerFactKey[PoolModules] = PoolModules.Key(platform)
}

object PoolModules {
  case class Key(platform: Platform = Platform.Runtime) extends CompilerFactKey[PoolModules]
}
