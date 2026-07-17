package com.vanillasource.eliot.eliotc.namedvalues.fact

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** Every top-level value named `name` in a platform pool, as a deterministically ordered list of fully-qualified
  * names — the reflection index the `namedValues[V](name)` rewrite expands into a builder chain. Keyed on the lexical
  * `name` (available before the checker) rather than on the claimed type `V`: type equality in Eliot is definitional
  * (needs the evaluator), so `V` is a *claim* the emitted chain asserts and the ordinary checker enforces downstream,
  * never a pre-checker filter.
  *
  * `fqns` holds only [[com.vanillasource.eliot.eliotc.module.fact.Qualifier.Default]], public, runtime-role names equal
  * to `name`, sorted by canonical `ValueFQN` string so the result is stable across runs. Layer copies of one module are
  * already merged (the index reads the per-module [[com.vanillasource.eliot.eliotc.module.fact.UnifiedModuleNames]],
  * not per-file names), while distinct modules stay distinct.
  */
case class NamedValuesIndex(name: String, platform: Platform, fqns: Seq[ValueFQN]) extends CompilerFact {
  override def key(): CompilerFactKey[NamedValuesIndex] = NamedValuesIndex.Key(name, platform)
}

object NamedValuesIndex {
  case class Key(name: String, platform: Platform = Platform.Runtime) extends CompilerFactKey[NamedValuesIndex]
}
