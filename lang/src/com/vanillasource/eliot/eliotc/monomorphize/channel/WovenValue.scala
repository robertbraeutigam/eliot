package com.vanillasource.eliot.eliotc.monomorphize.channel

import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicExpression}
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.content.Sourced

import scala.annotation.tailrec

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

  /** The number of arguments a direct call to this instance can absorb — the count of leading
    * [[MonomorphicExpression.FunctionLiteral]]s of the woven body, exactly as
    * [[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue.naturalArity]] computes it over the mono body.
    * The codegen driver (`used`, the jvm `ExpressionCodeGenerator`) reads this off the woven value it now consumes in
    * place of the `MonomorphicValue`; off the flag the woven body is the identity image so the arity is unchanged, and
    * on it the arity reflects the woven body (bind/`pure` insertion once that slice lands). `None` for a body-less value.
    */
  def naturalArity: Option[Int] = runtime.map(body => WovenValue.countLeadingLambdas(body.value, 0))
}

object WovenValue {

  /** Keyed exactly like [[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue.Key]] — one weave stack per
    * instance in this slice (the Suspend-riding base carrier), so the key is the mono key. (`weave key = mono key ×
    * stack`, docs/effects-as-channel.md §6; the stack dimension is added when control-effect carriers arrive.)
    */
  case class Key(vfqn: ValueFQN, typeArguments: Seq[GroundValue]) extends CompilerFactKey[WovenValue]

  @tailrec
  private def countLeadingLambdas(expression: MonomorphicExpression.Expression, count: Int): Int =
    expression match {
      case MonomorphicExpression.FunctionLiteral(_, _, body) => countLeadingLambdas(body.value.expression, count + 1)
      case _                                                 => count
    }
}
