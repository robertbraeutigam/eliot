package com.vanillasource.eliot.eliotc.uncurry.fact

import com.vanillasource.eliot.eliotc.symbolic.fact.{QualifiedName, QuantifiedType, SymbolicType}
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** A type-checked value that has been uncurried to a specific arity.
  *
  * @param vfqn
  *   The fully qualified value name
  * @param arity
  *   The arity this value has been uncurried to
  * @param name
  *   The sourced name
  * @param signature
  *   The type of this value
  * @param parameters
  *   The uncurried parameter list
  * @param returnType
  *   The return type after uncurrying
  * @param body
  *   Optional uncurried expression body
  */
case class UncurriedValue(
    vfqn: ValueFQN,
    arity: Int,
    name: Sourced[QualifiedName],
    signature: QuantifiedType,
    parameters: Seq[ParameterDefinition],
    returnType: SymbolicType,
    body: Option[Sourced[UncurriedExpression.Expression]]
) extends CompilerFact {
  override def key(): CompilerFactKey[UncurriedValue] =
    UncurriedValue.Key(vfqn, arity)
}

object UncurriedValue {
  case class Key(vfqn: ValueFQN, arity: Int) extends CompilerFactKey[UncurriedValue]
}
