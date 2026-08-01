package com.vanillasource.eliot.eliotc.monomorphize.channel

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** A marker that one monomorphic instance has passed the **native meta-declaration check**
  * ([[NativeMetaDeclarationProcessor]]): if it is a native (body-less) value whose ground return type carries non-`Unit`
  * refinement meta, it declares a `^Meta` transfer companion, so the range it produces is *stated* rather than silently
  * defaulted to ⊤.
  *
  * Produced only when the check holds; its absence (an abort from the processor) blocks the value's [[WovenValue]] and so
  * its codegen, exactly as [[EffectAccounting]] gates codegen for an undeclared effect. Keyed like
  * [[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue.Key]].
  *
  * @param vfqn
  *   The value this check belongs to.
  * @param typeArguments
  *   The concrete type arguments of the instance.
  */
case class NativeMetaDeclared(
    vfqn: ValueFQN,
    typeArguments: Seq[GroundValue]
) extends CompilerFact {
  override def key(): CompilerFactKey[NativeMetaDeclared] =
    NativeMetaDeclared.Key(vfqn, typeArguments)
}

object NativeMetaDeclared {
  case class Key(vfqn: ValueFQN, typeArguments: Seq[GroundValue]) extends CompilerFactKey[NativeMetaDeclared]
}
