package com.vanillasource.eliot.eliotc.monomorphize.fact

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
  * @param vfqn
  *   The fully qualified name of the value
  * @param semValue
  *   The semantic value for NbE evaluation
  * @param platform
  *   The source pool this binding was merged for
  */
case class NativeBinding(
    vfqn: ValueFQN,
    semValue: SemValue,
    platform: Platform = Platform.Runtime
) extends CompilerFact {
  override def key(): CompilerFactKey[NativeBinding] =
    NativeBinding.Key(vfqn, platform)
}

object NativeBinding {
  case class Key(vfqn: ValueFQN, platform: Platform = Platform.Runtime) extends CompilerFactKey[NativeBinding]
}
