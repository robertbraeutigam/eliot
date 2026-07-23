package com.vanillasource.eliot.eliotc.monomorphize.channel

import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicExpression}
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The effects-as-channel **woven** form of one monomorphic instance (docs/effects-as-channel.md §6) — the direct-style
  * → monadic elaboration the checker performs today, done here post-monomorphization over concrete terms under
  * `--effect-channel`.
  *
  * Mirrors [[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue]]'s consumable shape (name / signature /
  * runtime body) so codegen (`uncurry`, `used`, the jvm backend) can read it in place of `MonomorphicValue` under the
  * flag, and is keyed identically. It carries the *woven* body: each abstract user-effect-operation reference
  * (`Console::printLine`, left abstract by the effect-blind checker) resolved to its concrete carrier-instance method at
  * the assigned base carrier, and (later slices) `flatMap`/`pure` inserted where an effectful sub-term meets a pure
  * position. Off the flag — or when no base carrier is supplied — it is the identity image of the value's
  * `MonomorphicValue`.
  *
  * Scope note (this slice): single top-level effect operations are resolved at the base carrier; bind/`pure` insertion,
  * precise carrier-headed node types, and control-effect stacks are later slices.
  *
  * @param vfqn
  *   The value this weave belongs to (the same instance identity as its `MonomorphicValue`).
  * @param typeArguments
  *   The concrete type arguments of the instance.
  * @param name
  *   The sourced name of the value (forwarded from `MonomorphicValue` for codegen).
  * @param signature
  *   The woven signature — the carrier-wrapped payload (`IO[Unit]`) for an effectful value, else the payload unchanged.
  * @param runtime
  *   The woven runtime body, or `None` for a body-less value.
  */
case class WovenValue(
    vfqn: ValueFQN,
    typeArguments: Seq[GroundValue],
    name: Sourced[QualifiedName],
    signature: GroundValue,
    runtime: Option[Sourced[MonomorphicExpression.Expression]]
) extends CompilerFact {
  override def key(): CompilerFactKey[WovenValue] = WovenValue.Key(vfqn, typeArguments)
}

object WovenValue {

  /** Keyed exactly like [[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue.Key]] — one weave stack per
    * instance in this slice (the Suspend-riding base carrier), so the key is the mono key. (`weave key = mono key ×
    * stack`, docs/effects-as-channel.md §6; the stack dimension is added when control-effect carriers arrive.)
    */
  case class Key(vfqn: ValueFQN, typeArguments: Seq[GroundValue]) extends CompilerFactKey[WovenValue]
}
