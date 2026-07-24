package com.vanillasource.eliot.eliotc.monomorphize.channel

import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicExpression}
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.content.Sourced

import scala.annotation.tailrec

/** The effects-as-channel **woven** form of one monomorphic instance (docs/effects-as-channel.md §6) — the
  * `Id`-normalized image of the value's [[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue]], produced
  * post-monomorphization by [[WovenValueProcessor]] at the codegen seam.
  *
  * Mirrors [[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue]]'s consumable shape (name / signature /
  * runtime body) so codegen (`uncurry`, `used`, the jvm backend) reads it in place of `MonomorphicValue`, and is keyed
  * identically. The checker leaves the body already monadic (binds/`pure` inserted by the uniform elaboration) and
  * resolved; the woven stage erases the pervasive identity carrier `Id` (`runId`/`Id`/`Effect[Id]` methods rewritten
  * away, `Id[X]` types collapsed to `X`) so pure code recovers its efficient shape and no effect machinery ships for it.
  * A value that carries no `Id` (already-effectful or genuinely pure with none inserted) weaves to the identity image.
  *
  * @param vfqn
  *   The value this weave belongs to (the same instance identity as its `MonomorphicValue`).
  * @param typeArguments
  *   The concrete type arguments of the instance.
  * @param name
  *   The sourced name of the value (forwarded from `MonomorphicValue` for codegen).
  * @param signature
  *   The woven signature — the carrier-wrapped payload (`IO[Unit]`) for an effectful value, else the payload with any
  *   `Id` carrier erased.
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
    * place of the `MonomorphicValue`; the woven body reflects any `Id`-erasure the normalizer applied. `None` for a
    * body-less value.
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
