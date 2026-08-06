package com.vanillasource.eliot.eliotc.monomorphize.fact

import com.vanillasource.eliot.eliotc.codec.LangFactCodecs

import com.vanillasource.eliot.eliotc.compiler.cache.codec.FactCodec

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.module.fact.QualifiedName
import com.vanillasource.eliot.eliotc.source.content.Sourced

import scala.annotation.tailrec

/** A monomorphized (specialized) value with all type parameters instantiated to concrete types via NbE.
  *
  * @param vfqn
  *   The fully qualified name of the original value
  * @param typeArguments
  *   The concrete type arguments used for specialization
  * @param name
  *   The sourced name of the value
  * @param signature
  *   The concrete ground type of this specialized instance
  * @param runtime
  *   The optional monomorphized runtime body
  * @param ambientCarriers
  *   This value's own *ambient* effect carriers as **full ground values** (`IO`, `StateCarrier[S, IO]` — never just
  *   heads), the carriers its declared effect row (an open `{E...}` row's carrier binders) or pinned/concrete-carrier
  *   return rides. Empty for a pure value and for the synthetic entry (which run their bodies on a concrete carrier the
  *   caller never declares). The effect-accounting verifier's `derived ⊆ declared` test decides "does this reference
  *   ride the value's ambient" by **exact ground equality** against this set — the authoritative "ambient" input,
  *   forwarded here rather than reconstructed from the mono key ↔ signature-binder alignment
  *   (docs/effects-as-channel.md §5, U4-c-0a). Stamped by the single writer at mono-fact production
  *   ([[com.vanillasource.eliot.eliotc.monomorphize.check.TypeStackLoop]]) from the two carrier spellings.
  */
case class MonomorphicValue(
    vfqn: ValueFQN,
    typeArguments: Seq[GroundValue],
    name: Sourced[QualifiedName],
    signature: GroundValue,
    runtime: Option[Sourced[MonomorphicExpression.Expression]],
    ambientCarriers: Set[GroundValue]
) extends CompilerFact {
  override def key(): CompilerFactKey[MonomorphicValue] =
    MonomorphicValue.Key(vfqn, typeArguments)

  /** The number of arguments a direct call to this instance can absorb: the count of leading
    * [[MonomorphicExpression.FunctionLiteral]]s of the runtime body. An application spine longer than this is an
    * over-application — the excess arguments must be applied one at a time to the function *value* the direct call
    * returns — so both the `used` arity statistics and the backend call sites cap the direct-call arity here. `None`
    * for a body-less (native) value, whose emitted arity is the platform's decision.
    */
  def naturalArity: Option[Int] = runtime.map(body => MonomorphicValue.countLeadingLambdas(body.value, 0))
}

object MonomorphicValue {

  /** Composite key that uniquely identifies a monomorphic specialization. The same generic function with different type
    * arguments produces different keys.
    */
  case class Key(vfqn: ValueFQN, typeArguments: Seq[GroundValue]) extends CompilerFactKey[MonomorphicValue] {
    override def valueCodec: Option[FactCodec[MonomorphicValue]] = Some(LangFactCodecs.monomorphicValueCodec)
  }

  @tailrec
  private def countLeadingLambdas(expression: MonomorphicExpression.Expression, count: Int): Int =
    expression match {
      case MonomorphicExpression.FunctionLiteral(_, _, body) => countLeadingLambdas(body.value.expression, count + 1)
      case _                                                 => count
    }
}
