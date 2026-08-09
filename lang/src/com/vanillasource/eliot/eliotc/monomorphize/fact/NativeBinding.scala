package com.vanillasource.eliot.eliotc.monomorphize.fact

import com.vanillasource.eliot.eliotc.compiler.cache.codec.FactCodec

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** Maps a value's fully qualified name to its NbE semantic value. Used to inject native built-in values (Function,
  * Type, data constructors, etc.) and user-defined values into the NbE evaluator.
  *
  * Keyed by [[Platform]]: the compiler track ([[Platform.Compiler]]) merges only the compiler-appropriate suppliers
  * (the native + compiler-layer bodies, never the runtime `user` pool), so a compiler-platform reduction can never
  * reach a runtime-only body — the native-leaf boundary. The runtime track ([[Platform.Runtime]], the default) merges
  * the native suppliers over the runtime `user` bodies exactly as before.
  *
  * The fact is *total*: the [[com.vanillasource.eliot.eliotc.monomorphize.processor.BindingMergerProcessor]] answers
  * `semValue = None` for a name no supplier binds rather than declining, so readers request it with `getFactOrAbort`
  * and inspect the payload — a `None` payload is a domain answer ("no binding; evaluation leaves it stuck at the use
  * site"), never a missing fact. Keeping the fact total also removes the cache-poison edge a decline leaves behind (see
  * `docs/retire-optional-fact-reads.md`).
  *
  * @param vfqn
  *   The fully qualified name of the value
  * @param semValue
  *   The semantic value for NbE evaluation, or `None` if no supplier binds `vfqn`
  * @param platform
  *   The source pool this binding was merged for
  */
case class NativeBinding(
    vfqn: ValueFQN,
    semValue: Option[SemValue],
    platform: Platform = Platform.Runtime
) extends CompilerFact {
  override def key(): CompilerFactKey[NativeBinding] =
    NativeBinding.Key(vfqn, platform)
}

object NativeBinding {
  case class Key(vfqn: ValueFQN, platform: Platform = Platform.Runtime) extends CompilerFactKey[NativeBinding] {

    /** Declined: its contribution carries a `SemValue` closure — serializable, never equal (§3.1, §4). */
    override def valueCodec: Option[FactCodec[NativeBinding]] = None
  }
}
