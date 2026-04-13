package com.vanillasource.eliot.eliotc.uncurry.fact

import com.vanillasource.eliot.eliotc.core.fact.QualifiedName
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** A monomorphic value that has been uncurried to a specific arity.
  *
  * @param vfqn
  *   The fully qualified value name
  * @param typeArguments
  *   The source-level type arguments for this monomorphic instance
  * @param arity
  *   The arity this value has been uncurried to
  * @param name
  *   The sourced name
  * @param signature
  *   The concrete type of this value
  * @param parameters
  *   The uncurried parameter list with concrete types
  * @param returnType
  *   The concrete return type after uncurrying
  * @param body
  *   Optional uncurried expression body
  */
case class UncurriedMonomorphicValue(
    vfqn: ValueFQN,
    typeArguments: Seq[Sourced[OperatorResolvedExpression]],
    arity: Int,
    name: Sourced[QualifiedName],
    signature: GroundValue,
    parameters: Seq[MonomorphicParameterDefinition],
    returnType: GroundValue,
    body: Option[Sourced[UncurriedMonomorphicExpression.Expression]]
) extends CompilerFact {
  override def key(): CompilerFactKey[UncurriedMonomorphicValue] =
    UncurriedMonomorphicValue.Key(vfqn, typeArguments, arity)
}

object UncurriedMonomorphicValue {
  case class Key(vfqn: ValueFQN, typeArguments: Seq[Sourced[OperatorResolvedExpression]], arity: Int)
      extends CompilerFactKey[UncurriedMonomorphicValue]
}
