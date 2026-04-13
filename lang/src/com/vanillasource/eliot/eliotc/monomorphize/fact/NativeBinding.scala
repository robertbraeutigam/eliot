package com.vanillasource.eliot.eliotc.monomorphize.fact

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** Maps a value's fully qualified name to its NbE semantic value. Used to inject native built-in values (Function,
  * Type, data constructors, etc.) and user-defined values into the NbE evaluator.
  *
  * @param vfqn
  *   The fully qualified name of the value
  * @param semValue
  *   The semantic value for NbE evaluation
  */
case class NativeBinding(
    vfqn: ValueFQN,
    semValue: SemValue
) extends CompilerFact {
  override def key(): CompilerFactKey[NativeBinding] =
    NativeBinding.Key(vfqn)
}

object NativeBinding {
  case class Key(vfqn: ValueFQN) extends CompilerFactKey[NativeBinding]
}
